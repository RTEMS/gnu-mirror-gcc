/* Calculate branch probahilities, and basic block execution counts.
   Copyright (C) 1990-2022 Free Software Foundation, Inc.
   Contributed by James E. Wilson, UC Berkeley/Cygnus Support;
   based on some ideas from Dain Samples of UC Berkeley.
   Further mangling by Bob Manson, Cygnus Support.
   Converted to use trees by Dale Johannesen, Apple Computer.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Generate basic block profile instrumentation and auxiliary files.
   Tree-based version.  See profile.cc for overview.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "memmodel.h"
#include "backend.h"
#include "target.h"
#include "tree.h"
#include "gimple.h"
#include "cfghooks.h"
#include "tree-pass.h"
#include "ssa.h"
#include "cgraph.h"
#include "coverage.h"
#include "diagnostic-core.h"
#include "fold-const.h"
#include "varasm.h"
#include "tree-nested.h"
#include "gimplify.h"
#include "gimple-iterator.h"
#include "gimplify-me.h"
#include "tree-cfg.h"
#include "tree-into-ssa.h"
#include "value-prof.h"
#include "profile.h"
#include "tree-cfgcleanup.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-pretty-print.h"
#include "langhooks.h"
#include "stor-layout.h"
#include "xregex.h"
#include "alloc-pool.h"
#include "symbol-summary.h"
#include "symtab-thunks.h"
#include "cfganal.h"
#include "cfgloop.h"

#include "tree.h"
#include "gimple-pretty-print.h"

static GTY(()) tree gcov_type_node;
static GTY(()) tree tree_interval_profiler_fn;
static GTY(()) tree tree_pow2_profiler_fn;
static GTY(()) tree tree_topn_values_profiler_fn;
static GTY(()) tree tree_indirect_call_profiler_fn;
static GTY(()) tree tree_average_profiler_fn;
static GTY(()) tree tree_ior_profiler_fn;
static GTY(()) tree tree_time_profiler_counter;


static GTY(()) tree ic_tuple_var;
static GTY(()) tree ic_tuple_counters_field;
static GTY(()) tree ic_tuple_callee_field;

namespace
{

struct conds_ctx
{
    /* This is both a reusable shared allocation which is also used to return
       single expressions, which means it for most code should only hold a
       couple of elements (the number of terms plus the two outcomes). */
    auto_vec<basic_block, 16> blocks;

    /* Bitmap of the processed blocks.  Bit n set means basic_block->index has
       been processed either explicitly or as a part of an expression. */
    auto_sbitmap marks;

    /* Map from basic_block->index to an ordering so that for a single
       expression (a || b && c) => index_map[a] < index_map[b] < index_map[c].
       The values do not have to be consecutive and can be interleaved by
       values from other expressions, so comparisons only make sense for blocks
       that belong to the same expression. */
    auto_vec<int, 16> index_map;

    /* Pre-allocate bitmaps and vectors for per-function book keeping.  This is
       pure instance reuse and the bitmaps carry no data between function
       calls. */
    auto_sbitmap G1;
    auto_sbitmap G2;
    auto_sbitmap G3;
    auto_vec<basic_block, 32> B1;
    auto_vec<basic_block, 16> B2;

    explicit conds_ctx (unsigned size) noexcept (true) : marks (size),
    G1 (size), G2 (size), G3 (size)
    {
	bitmap_clear (marks);
    }

    /* Mark a node as processed so nodes are not processed twice for example in
       loops, gotos. */
    void mark (const basic_block b) noexcept (true)
    {
	gcc_assert (!bitmap_bit_p (marks, b->index));
	bitmap_set_bit (marks, b->index);
    }

    /* Mark nodes as processed so they are not processed twice. */
    void mark (const vec<basic_block>& bs) noexcept (true)
    {
	for (const basic_block b : bs)
	    mark (b);
    }

    /* Check if all nodes are marked.  A successful run should visit & mark
       every reachable node exactly once. */
    bool all_marked (const vec<basic_block>& reachable) const noexcept (true)
    {
	for (const basic_block b : reachable)
	    if (!bitmap_bit_p (marks, b->index))
		return false;
	return true;
    }
};

/* Only instrument terms with fewer than number of bits in a (wide) gcov
   integer, which is probably 64.  The algorithm itself does not impose this
   limitation, but it makes for a simpler implementation.

   * Allocating the output data structure (coverage_counter_alloc ()) can
     assume pairs of gcov_type_unsigned and not use a separate length field.
   * A pair gcov_type_unsigned can be used as accumulators.
   * Updating accumulators is can use the bitwise operations |=, &= and not
     custom operators that work for arbitrary-sized bit-sets.

   Most real-world code should be unaffected by this, but it is possible
   (especially for generated code) to exceed this limit.
 */
#define CONDITIONS_MAX_TERMS (sizeof (gcov_type_unsigned) * BITS_PER_UNIT)
#define EDGE_CONDITION (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)

/* Compare two basic blocks by their order in the expression i.e. for (a || b)
   then cmp_index_map (a, b, ...) < 0.  The result is undefined if lhs, rhs
   belong to different expressions. */
int
cmp_index_map (const void *lhs, const void *rhs, void *index_map)
{
    const_basic_block l = *(const basic_block*) lhs;
    const_basic_block r = *(const basic_block*) rhs;
    const vec<int>* im = (const vec<int>*) index_map;
    return (*im)[l->index] - (*im)[r->index];
}

/* Find the index of needle in blocks; return -1 if not found. This has two
   uses, sometimes for the index and sometimes for set member checks.  Sets are
   typically very small (number of conditions, >8 is uncommon) so linear search
   should be very fast. */
int
index_of (const basic_block needle, array_slice<basic_block> blocks)
{
    const int size = int (blocks.size ());
    for (int i = 0; i < size; i++)
	if (blocks[i] == needle)
	    return i;
    return -1;
}

/* Returns true if this is a conditional node, i.e. it has outgoing true and
   false edges. */
bool
block_conditional_p (const basic_block b)
{
    unsigned t = 0;
    unsigned f = 0;
    for (edge e : b->succs)
    {
        t |= (e->flags & EDGE_TRUE_VALUE);
        f |= (e->flags & EDGE_FALSE_VALUE);
    }
    return t && f;
}

/* Check if the edge is a conditional. */
bool
edge_conditional_p (const edge e)
{
    return e->flags & EDGE_CONDITION;
}

/* Special cases of the single_*_p and single_*_edge functions in basic-block.h
   that don't consider exception handling or other complex edges.  This helps
   create a view of the CFG with only normal edges - if a basic block has both
   an outgoing fallthrough and exceptional edge [1], it should be considered a
   single-successor.

   [1] if this is not possible, these functions can be removed and replaced by
       their basic-block.h cousins. */
bool
single (const vec<edge, va_gc> *edges)
{
    int n = EDGE_COUNT (edges);
    if (n == 0)
	return false;

    for (edge e : edges)
	if (e->flags & EDGE_COMPLEX)
	    n -= 1;

    return n == 1;
}

/* Get the single, non-complex edge.  Behavior is undefined edges have more
   than 1 non-complex edges. */
edge
single_edge (const vec<edge, va_gc> *edges)
{
    for (edge e : edges)
    {
	if (e->flags & EDGE_COMPLEX)
	    continue;
	return e;
    }
    return NULL;
}

/* Sometimes, for example with function calls and C++ destructors, the CFG gets
   extra nodes that are essentially single-entry-single-exit in the middle of
   boolean expressions.  For example:

      x || can_throw (y)

               A
              /|
             / |
            B  |
            |  |
            C  |
           / \ |
          /   \|
         F     T

   Without the extra node inserted by the function + exception it becomes a
   proper 2-term graph, not 2 single-term graphs.

               A
              /|
             C |
            / \|
           F   T

   contract_edge ignores the series of intermediate nodes and makes a virtual
   edge A -> C without having to construct a new simplified CFG explicitly.  It
   gets more complicated as non-conditional edges is how the body of the
   then/else blocks are separated from the boolean expression, so only edges
   that are inserted because of function calls in the expression itself must be
   merged.

   Only chains of single-exit single-entry nodes that end with a condition
   should be contracted. */
edge
contract_edge (edge e)
{
    edge source = e;
    while (true)
    {
	basic_block dest = e->dest;
	if (!single (dest->preds))
	    return source;
	if (e->flags & EDGE_DFS_BACK)
	    return source;
	if (block_conditional_p (dest))
	    return e;

	e = single_edge (dest->succs);
	if (!e)
	    return source;
    }
}

/* This is the predecessor dual of contract_edge; it collapses the predecessor
   blocks between two operands in a boolean expression. */
edge
contract_edge_up (edge e)
{
    while (true)
    {
	basic_block src = e->src;
	if (edge_conditional_p (e))
	    return e;
	if (!single (src->preds))
	    return e;
	e = single_edge (src->preds);
    }
}

/* Scan upwards and find the ancestors of the node pre that are also in G,
   stopping at post. The ancestors are recorded in the bimap.
   dfs_enumerate_from () won't work as the filter function needs edge
   information. */
void
scan_up (sbitmap ancestors, basic_block pre, basic_block post, const sbitmap G)
{
    if (!bitmap_bit_p (G, pre->index))
	return;

    bitmap_set_bit (ancestors, pre->index);
    bitmap_set_bit (ancestors, post->index);
    if (pre == post)
	return;

    auto_vec<basic_block, 16> stack;
    stack.safe_push (pre);

    while (!stack.is_empty())
    {
	basic_block b = stack.pop ();
	if (single (b->preds))
	{
	    edge e = single_edge (b->preds);
	    e = contract_edge_up (e);
	    b = e->dest;
	}

	for (edge e : b->preds)
	{
	    basic_block src = e->src;
	    if (bitmap_bit_p (ancestors, e->src->index))
		continue;
	    if (!bitmap_bit_p (G, e->src->index))
		continue;
	    bitmap_set_bit (ancestors, src->index);
	    stack.safe_push (src);
	}
    }
}

/* A simple struct for storing/returning outcome block pairs.  Either both
   blocks are set or both are NULL. */
struct outcomes
{
    basic_block t = NULL;
    basic_block f = NULL;

    operator bool () const noexcept (true)
    {
	return t && f;
    }
};

/* Get the true/false successors of a basic block. If b is not a conditional
   block both edges are NULL. */
outcomes
conditional_succs (const basic_block b)
{
    outcomes c;
    for (edge e : b->succs)
    {
	if (e->flags & EDGE_TRUE_VALUE)
	    c.t = (e)->dest;
	if (e->flags & EDGE_FALSE_VALUE)
	    c.f = (e)->dest;
    }

    gcc_assert ((c.t && c.f) || (!c.t && !c.f));
    return c;
}

/* Get the index or offset of a conditional flag, 0 for true and 1 for false.
   These indices carry no semantics but must be consistent as they are used to
   index into data structures in code generation and gcov. */
unsigned
condition_index (unsigned flag)
{
    return (flag & EDGE_CONDITION) == EDGE_TRUE_VALUE ? 0 : 1;
}

/* Compute the masking vector.

   Masking and short circuiting are deeply connected - masking occurs when
   control flow reaches a state that is also reachable with short circuiting.
   In fact, masking appears as short circuiting in the reversed expression.
   This means we can find the limits, the last term in previous subexpressions
   by following the edges that short circuit to the same outcome.

TODO: doc algo

   In the simplest case a || b:

   a
   |\
   | b
   |/ \
   T   F

   T has has multiple incoming edges and is the outcome of a short circuit,
   with top = a, bot = b.  lim = a.succs[0] = b, which has 1 ancestor a and so
   the masking vector is b[1] = {a}.

   Now consider (a && b) || (c && d) and its masking vectors:

   a
   |\
   b \
   |\|
   | c
   | |\
   | d \
   |/ \|
   T   F

   a[0] = {}
   a[1] = {}
   b[0] = {a}
   b[1] = {}
   c[0] = {}
   c[1] = {}
   d[0] = {c}
   d[1] = {a,b}

   b[0] and d[0] are identical to the a || b example, but d[1].
   T.preds = {b,d}, top = b, bot = d, and b.succs[0] = c.  ancestors(c) = {a,b}
   which is d[1] and the search terminates as a.preds is empty and b.preds are
   discarded.

   The masking vector is represented as two bitfields per term in the
   expression.  The expression a || b && c becomes the term vector [a b c] and
   the bitsets are masks = [a.true a.false b.true ...].  The kth bit is set if
   the the kth term is masked by the node-outcome. */
void
build_masks (conds_ctx& ctx, array_slice<basic_block> blocks,
	     array_slice<gcov_type_unsigned> masks)
{
    gcc_assert (!blocks.empty ());
    gcc_assert (blocks.is_valid ());
    gcc_assert (masks.is_valid ());

    sbitmap marks = ctx.G1;
    sbitmap expr = ctx.G2;
    vec<basic_block>& queue = ctx.B1;
    const vec<int>& index_map = ctx.index_map;
    bitmap_clear (expr);

    for (const basic_block b : blocks)
	bitmap_set_bit (expr, b->index);

    // Ignore the first term as it cannot mask anything
    array_slice<basic_block> tail (blocks.begin () + 1, blocks.size () - 1);

    for (const basic_block b : tail)
    {
	for (edge e1 : b->preds)
	for (edge e2 : b->preds)
        {
	    const basic_block top = e1->src;
	    const basic_block bot = e2->src;
	    const unsigned flag = e1->flags & e2->flags & (EDGE_CONDITION);

	    if (!flag)
		continue;
	    if (e1 == e2)
		continue;
	    if (!bitmap_bit_p (expr, top->index))
		continue;
	    if (!bitmap_bit_p (expr, bot->index))
		continue;
	    if (index_map[top->index] > index_map[bot->index])
		continue;

	    outcomes out = conditional_succs (top);
	    gcc_assert (out);
	    bitmap_clear (marks);
	    bitmap_set_bit (marks, out.t->index);
	    bitmap_set_bit (marks, out.f->index);
	    queue.truncate (0);
	    queue.safe_push (top);

	    // The edge bot -> outcome triggers the masking
	    const int term = index_of (bot, blocks);
	    const int m = term*2 + condition_index (flag);
	    while (!queue.is_empty())
	    {
		basic_block q = queue.pop ();
		/* q may have been processed & completed by being added to the
		   queue multiple times, so check that there is still work to
		   do before continuing. */
		if (bitmap_bit_p (marks, q->index))
		    continue;

		outcomes succs = conditional_succs (q);
		if (!bitmap_bit_p (marks, succs.t->index))
		    continue;
		if (!bitmap_bit_p (marks, succs.f->index))
		    continue;

		const int index = index_of (q, blocks);
		gcc_assert (index != -1);
		masks[m] |= gcov_type_unsigned (1) << index;
		bitmap_set_bit (marks, q->index);

		for (edge e : q->preds)
		{
		    e = contract_edge_up (e);
		    if (!edge_conditional_p (e))
			continue;
		    if (e->flags & EDGE_DFS_BACK)
			continue;
		    if (bitmap_bit_p (marks, e->src->index))
			continue;
		    if (!bitmap_bit_p (expr, e->src->index))
			continue;
		    queue.safe_push (e->src);
		}
	    }
	}
    }
}

/* TODO: doc */
void
scan_down (basic_block pre, basic_block post, sbitmap expr,
	   vec<basic_block>& out)
{
    out.safe_push (pre);
    bitmap_set_bit (expr, pre->index);
    for (unsigned pos = 0; pos < out.length (); pos++)
    {
	for (edge e : out[pos]->succs)
	{
	    basic_block dest = contract_edge (e)->dest;
	    if (dest == post)
		continue;
	    if (!dominated_by_p (CDI_DOMINATORS, dest, pre))
		continue;
	    if (!block_conditional_p (dest))
		continue;
	    if (bitmap_bit_p (expr, dest->index))
		continue;
	    if (e->flags & EDGE_DFS_BACK)
		continue;

	    bitmap_set_bit (expr, dest->index);
	    out.safe_push (dest);
	}
    }
}

/* Find the neighborhood of the graph G = [blocks, blocks+n), the
   successors of nodes in G that are not also in G. */
void
neighborhood (vec<basic_block>& blocks, const sbitmap G, vec<basic_block>& out)
{
    for (const basic_block b : blocks)
    {
	for (edge e : b->succs)
	{
	    basic_block dest = contract_edge (e)->dest;
	    if (bitmap_bit_p (G, dest->index))
		continue;
	    if (!out.contains (dest))
		out.safe_push (dest);
	}
    }
}

/* Find and isolate the expression starting at pre.

   Make a cut C = (G, G') by following all condition edges from pre and find
   the neighborhood of G.  The first conditional expression is made up of the
   intersection of ancestors of every node in the neighborhood. */
void
find_first_conditional (conds_ctx &ctx, basic_block pre, vec<basic_block>& out)
{
    sbitmap expr = ctx.G1;
    sbitmap reachable = ctx.G2;
    sbitmap ancestors = ctx.G3;
    bitmap_clear (expr);
    bitmap_clear (reachable);

    vec<basic_block>& G = ctx.B1;
    vec<basic_block>& NG = ctx.B2;
    G.truncate (0);
    NG.truncate (0);

    basic_block post = get_immediate_dominator (CDI_POST_DOMINATORS, pre);
    scan_down (pre, post, expr, G);
    if (G.length () == 1)
    {
	out.safe_push (pre);
	return;
    }

    neighborhood (G, expr, NG);
    bitmap_copy (reachable, expr);

    for (const basic_block neighbor : NG)
    {
	bitmap_clear (ancestors);
	for (edge e : neighbor->preds)
	    scan_up (ancestors, e->src, pre, reachable);
	bitmap_and (expr, expr, ancestors);
    }

    for (const basic_block b : G)
	if (bitmap_bit_p (expr, b->index))
	    out.safe_push (b);
    out.sort (cmp_index_map, &ctx.index_map);
}

/* Emit lhs = op1 <op> op2 on edges.  This emits non-atomic instructions and
   should only be used on the local accumulators. */
void
emit_bitwise_op (edge e, tree lhs, tree op1, tree_code op, tree op2)
{
    tree tmp;
    gassign *read;
    gassign *bitw;
    gimple *write;

    tmp = make_temp_ssa_name (gcov_type_node, NULL, "__conditions_tmp");
    read = gimple_build_assign (tmp, op1);
    tmp = make_temp_ssa_name (gcov_type_node, NULL, "__conditions_tmp");
    bitw = gimple_build_assign (tmp, op, gimple_assign_lhs (read), op2);
    write = gimple_build_assign (lhs, gimple_assign_lhs (bitw));

    gsi_insert_on_edge (e, read);
    gsi_insert_on_edge (e, bitw);
    gsi_insert_on_edge (e, write);
}

/* Visitor for make_index_map. */
void
make_index_map_visit (basic_block b, vec<basic_block>& L, vec<int>& marks)
{
    if (marks[b->index])
	return;

    for (edge e : b->succs)
	if (!(e->flags & EDGE_DFS_BACK))
	    make_index_map_visit (e->dest, L, marks);

    marks[b->index] = 1;
    L.quick_push (b);
}

/* Find a topological sorting of the blocks in a function so that left operands
   are before right operands including subexpressions.  Sorting on block index
   does not guarantee this property and the syntactical order of terms is very
   important to the condition coverage.  The sorting algorithm is from Cormen
   et al (2001) but with back-edges ignored and thus there is no need for
   temporary marks (for cycle detection).

   It is important to select unvisited nodes in DFS order to ensure the
   roots/leading terms of boolean expressions are visited first (the other
   terms being covered by the recursive step), but the visiting order of
   individual boolean expressions carries no significance.

   For the expression (a || (b && c) || d) the blocks should be [a b c d]. */
void
make_index_map (const vec<basic_block>& blocks, int max_index,
		vec<basic_block>& L, vec<int>& index_map)
{
    L.truncate (0);
    L.reserve (max_index);

    /* Use of the output map as a temporary for tracking visited status. */
    index_map.truncate (0);
    index_map.safe_grow_cleared (max_index);
    for (const basic_block b : blocks)
	make_index_map_visit (b, L, index_map);

    /* Insert canaries - if there are unreachable nodes (for example infinite
       loops) then the unreachable nodes should never be needed for comparison,
       and L.length () < max_index.  An index mapping should also never be
       recorded twice. */
    for (unsigned i = 0; i < index_map.length (); i++)
	index_map[i] = -1;

    gcc_assert (blocks.length () == L.length ());
    L.reverse ();
    const int nblocks = L.length ();
    for (int i = 0; i < nblocks; i++)
    {
	gcc_assert (L[i]->index != -1);
	index_map[L[i]->index] = i;
    }
}

/* Walk the CFG and collect conditionals.

   1. Collect a candidate set G by walking from the root following all
      (contracted) condition edges.
   2. This creates a cut C = (G, G'); find the neighborhood N(G).
   3. For every node in N(G), follow the edges across the cut and collect all
      ancestors (that are also in G).
   4. The intersection of all these ancestor sets is the boolean expression B
      that starts in root.

   Walking is not guaranteed to find nodes in the order of the expression, it
   might find (a || b) && c as [a c b], so the result must be sorted by the
   index map. */
const vec<basic_block>&
collect_conditions (conds_ctx& ctx, const basic_block block)
{
    vec<basic_block>& blocks = ctx.blocks;
    blocks.truncate (0);

    if (bitmap_bit_p (ctx.marks, block->index))
	return blocks;

    if (!block_conditional_p (block))
    {
	ctx.mark (block);
	return blocks;
    }

    find_first_conditional (ctx, block, blocks);
    ctx.mark (blocks);

    if (blocks.length () <= CONDITIONS_MAX_TERMS)
    {
	outcomes out = conditional_succs (blocks.last ());
	blocks.safe_push (out.t);
	blocks.safe_push (out.f);
    }
    else
    {
	location_t loc = gimple_location (gsi_stmt (gsi_last_bb (block)));
	warning_at (loc, OPT_Wcoverage_too_many_conditions,
		    "Too many conditions (found %u); giving up coverage",
		    blocks.length ());
	blocks.truncate (0);
    }
    return blocks;
}

/* Used for dfs_enumerate_from () to include all reachable nodes. */
bool yes (const_basic_block, const void *) {
    return true;
}

}

array_slice<basic_block>
condition_coverage::blocks (unsigned n) noexcept (true)
{
    if (n >= m_index.length ())
	return array_slice<basic_block>::invalid ();

    basic_block *begin = m_blocks.begin () + m_index[n];
    basic_block *end = m_blocks.begin () + m_index[n + 1];
    return array_slice<basic_block> (begin, end - begin);
}

array_slice<gcov_type_unsigned>
condition_coverage::masks (unsigned n) noexcept (true)
{
    if (n >= m_index.length ())
	return array_slice<gcov_type_unsigned>::invalid ();

    gcov_type_unsigned *begin = m_masks.begin () + 2*m_index[n];
    gcov_type_unsigned *end = m_masks.begin () + 2*m_index[n + 1];
    return array_slice<gcov_type_unsigned> (begin, end - begin);
}

unsigned
condition_coverage::length () const noexcept (true)
{
    if (m_index.is_empty ())
	return 0;
    return m_index.length () - 1;
}

/* Condition coverage (MC/DC)

   Algorithm
   ---------
   Whalen, Heimdahl, De Silva in "Efficient Test Coverage Measurement for
   MC/DC" describe an algorithm for modified condition/decision coverage based
   on AST analysis.  This algorithm analyses the control flow graph to analyze
   expressions and compute masking vectors, but is inspired by their marking
   functions for recording outcomes.  The individual phases are described in
   more detail closer to the implementation.

   The CFG is broken up into segments between dominators.  This isn't strictly
   necessary, but since boolean expressions cannot cross dominators it makes
   for a nice way to reduce work.

   The coverage only considers the positions, not the symbols, in a
   conditional, e.g. !A || (!B && A) is a 3-term conditional even though A
   appears twice.  Subexpressions have no effect on term ordering:
   (a && (b || (c && d)) || e) comes out as [a b c d e].

   The output for gcov is a vector of pairs of unsigned integers, interpreted
   as bit-sets, where the bit index corresponds to the index of the condition
   in the expression.

   Implementation and interface
   ----------------------------
   Two public functions - find_conditions and instrument_decisions.

   find_conditions outputs two arrays, blocks and sizes.  The sizes describes
   the ranges of blocks that make up every conditional, in a [first, last)
   fashion, i.e. begin = blocks[sizes[n]], end = blocks[sizes[n+1]] for
   expression n.  The function returns the number of expressions.

   The coverage is designed to get most of its memory needs met by the caller,
   and heavily uses the end of the blocks array for buffer.  This is both for
   performance (no resizes, amortized cost of allocation) and
   ease-of-implementation.  This makes the caller responsible for allocating
   large enough arrays.

   blocks:
    Every permanent conditions add 2 blocks (the true & false dest blocks), and
    assuming a worst case of one-block-per-expr just storing the output needs
    3*n_basic_blocks_for_fn ().  Additionally, the searches might need to buffer
    the full graph between entry and exit [1]. In total that means
    5*n_basic_blocks_for_fn () should should be plenty, and the savings for
    reducing this number is probably not worth the risk.
   sizes:
    sizes gets one entry per expression plus initial, so
    1+n_basic_blocks_for_fn () is sufficient.

   instrument_decisions uses the information provided by find_conditions to
   inject code onto edges in the CFG.  Every instrumented function gets local
   accumulators zero'd on function entry, which on function exit are flushed to
   the global accumulators (created by coverage_counter_alloc ()).

   [1] In truth, the set of nodes that could be buffered gets smaller as the
       algorithm walks the CFG, but assuming just using node-count comes at
       little run-time cost and is guaranteed to be sufficient.
 */
int
condition_coverage::find_conditions (struct function *fn)
{
    record_loop_exits ();
    mark_dfs_back_edges (fn);

    const bool have_dom = dom_info_available_p (fn, CDI_DOMINATORS);
    const bool have_post_dom = dom_info_available_p (fn, CDI_POST_DOMINATORS);
    if (!have_dom)
	calculate_dominance_info (CDI_DOMINATORS);
    if (!have_post_dom)
	calculate_dominance_info (CDI_POST_DOMINATORS);

    const int nblocks = n_basic_blocks_for_fn (fn);
    conds_ctx ctx (nblocks);

    auto_vec<basic_block, 16> dfs;
    dfs.safe_grow (nblocks);
    const basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (fn);
    const basic_block exit = ENTRY_BLOCK_PTR_FOR_FN (fn);
    int n = dfs_enumerate_from (entry, 0, yes, dfs.address (), nblocks, exit);
    dfs.truncate (n);
    make_index_map (dfs, nblocks, ctx.B1, ctx.index_map);

    /* Visit all reachable nodes and collect conditions.  DFS order is
       important so the first node of a boolean expression is visited first
       (it will mark subsequent terms). */
    m_index.safe_push (0);
    for (const basic_block b : dfs)
    {
	const vec<basic_block>& expr = collect_conditions (ctx, b);
	if (!expr.is_empty ())
	{
	    m_blocks.safe_splice (expr);
	    m_index.safe_push (m_blocks.length ());
	}
    }
    gcc_assert (ctx.all_marked (dfs));

    if (!have_dom)
	free_dominance_info (fn, CDI_DOMINATORS);
    if (!have_post_dom)
	free_dominance_info (fn, CDI_POST_DOMINATORS);

    // TODO: must be grow_cleared; if reset (), truncate
    m_masks.safe_grow_cleared (2 * m_index.last());
    for (unsigned i = 0; i < this->length (); i++)
	build_masks (ctx, blocks (i), masks (i));

    return this->length ();
}

int instrument_decisions (basic_block *blocks, int nblocks, int condno,
			  tree *accu, gcov_type_unsigned *masks)
{
    /* Zero the local accumulators. */
    tree zero = build_int_cst (get_gcov_type (), 0);
    for (edge e : blocks[0]->succs)
    {
	gsi_insert_on_edge (e, gimple_build_assign (accu[0], zero));
	gsi_insert_on_edge (e, gimple_build_assign (accu[1], zero));
    }
    /* The true/false target blocks are included in the nblocks set, but
       their outgoing edges should not be instrumented. */
    gcc_assert (nblocks > 2);

    array_slice<basic_block> body(blocks, nblocks - 2);

    /* Add instructions for updating the function-local accumulators.  */
    for (int i = 0; i < nblocks - 2; i++)
    {
	for (edge e : blocks[i]->succs)
	{
	    if (!edge_conditional_p (e))
		continue;

	    /* accu |= expr[i] */
	    const int k = condition_index (e->flags);
	    tree rhs = build_int_cst (gcov_type_node, 1ULL << i);
	    emit_bitwise_op (e, accu[k], accu[k], BIT_IOR_EXPR, rhs);

	    if (masks[2*i + k] == 0)
		continue;

	    /* accu &= mask[i] */
	    tree mask = build_int_cst (gcov_type_node, ~masks[2*i + k]);
	    for (int j = 0; j < 2; j++)
		emit_bitwise_op (e, accu[j], accu[j], BIT_AND_EXPR, mask);
	}
    }

    const bool atomic = flag_profile_update == PROFILE_UPDATE_ATOMIC;
    const tree atomic_ior = builtin_decl_explicit
	(TYPE_PRECISION (gcov_type_node) > 32
	 ? BUILT_IN_ATOMIC_FETCH_OR_8
	 : BUILT_IN_ATOMIC_FETCH_OR_4);

    /* Add instructions for flushing the local accumulators.
       The two last blocks are the outcome blocks, and we need to flush the
       accumulators at the end-of-expression.  If it is done later then flushes
       could be lost to exception handling or unexpected termination.

       It is important that the flushes happen on on the outcome's incoming
       edges as it might not have outgoing edges:

       void fn (int a)
       {
	   if (a)
	    fclose();
	   exit();
       }

       Can yield the CFG:
       A
       |\
       | B
       |/
       e

       This typically only happen in optimized builds, but gives linker errors
       because the counter is left as an undefined symbol. */
    const basic_block outcome_blocks[] = {
	blocks[nblocks - 2],
	blocks[nblocks - 2],
	blocks[nblocks - 1],
	blocks[nblocks - 1],
    };
    const int outcome[] = { 0, 1, 0, 1 };

    // TODO: ref -> counter
    for (int i = 0; i < 4; i++)
    {
	const int k = outcome[i];
	for (edge e : outcome_blocks[i]->preds)
	{
	    /* Only instrument edges from inside the expression. Sometimes
	       complicated control flow (like sigsetjmp and gotos) add
	       predecessors that don't come from the boolean expression. */
	    if (index_of (e->src, body) == -1)
		continue;

	    tree ref = tree_coverage_counter_ref (GCOV_COUNTER_CONDS,
						  2*condno + k);
	    tree tmp = make_temp_ssa_name (gcov_type_node, NULL,
					   "__conditions_tmp");
	    if (atomic)
	    {
		tree relaxed = build_int_cst (integer_type_node,
					      MEMMODEL_RELAXED);
		ref = unshare_expr (ref);
		gassign *read = gimple_build_assign (tmp, accu[k]);
		gcall *flush = gimple_build_call (atomic_ior, 3,
						  build_addr (ref),
						  gimple_assign_lhs (read),
						  relaxed);

		gsi_insert_on_edge (e, read);
		gsi_insert_on_edge (e, flush);
	    }
	    else
	    {
		gassign *read = gimple_build_assign (tmp, ref);
		tmp = gimple_assign_lhs (read);
		gsi_insert_on_edge (e, read);
		ref = unshare_expr (ref);
		emit_bitwise_op (e, ref, accu[k], BIT_IOR_EXPR, tmp);
	    }
	}
    }
    return body.size ();
}

#undef CONDITIONS_MAX_TERMS
#undef EDGE_CONDITION

/* Do initialization work for the edge profiler.  */

/* Add code:
   __thread gcov*	__gcov_indirect_call.counters; // pointer to actual counter
   __thread void*	__gcov_indirect_call.callee; // actual callee address
   __thread int __gcov_function_counter; // time profiler function counter
*/
static void
init_ic_make_global_vars (void)
{
  tree gcov_type_ptr;

  gcov_type_ptr = build_pointer_type (get_gcov_type ());

  tree tuple_type = lang_hooks.types.make_type (RECORD_TYPE);

  /* callee */
  ic_tuple_callee_field = build_decl (BUILTINS_LOCATION, FIELD_DECL, NULL_TREE,
				      ptr_type_node);

  /* counters */
  ic_tuple_counters_field = build_decl (BUILTINS_LOCATION, FIELD_DECL,
					NULL_TREE, gcov_type_ptr);
  DECL_CHAIN (ic_tuple_counters_field) = ic_tuple_callee_field;

  finish_builtin_struct (tuple_type, "indirect_call_tuple",
			 ic_tuple_counters_field, NULL_TREE);

  ic_tuple_var
    = build_decl (UNKNOWN_LOCATION, VAR_DECL,
		  get_identifier ("__gcov_indirect_call"), tuple_type);
  TREE_PUBLIC (ic_tuple_var) = 1;
  DECL_ARTIFICIAL (ic_tuple_var) = 1;
  DECL_INITIAL (ic_tuple_var) = NULL;
  DECL_EXTERNAL (ic_tuple_var) = 1;
  if (targetm.have_tls)
    set_decl_tls_model (ic_tuple_var, decl_default_tls_model (ic_tuple_var));
}

/* Create the type and function decls for the interface with gcov.  */

void
gimple_init_gcov_profiler (void)
{
  tree interval_profiler_fn_type;
  tree pow2_profiler_fn_type;
  tree topn_values_profiler_fn_type;
  tree gcov_type_ptr;
  tree ic_profiler_fn_type;
  tree average_profiler_fn_type;
  const char *fn_name;

  if (!gcov_type_node)
    {
      const char *fn_suffix
	= flag_profile_update == PROFILE_UPDATE_ATOMIC ? "_atomic" : "";

      gcov_type_node = get_gcov_type ();
      gcov_type_ptr = build_pointer_type (gcov_type_node);

      /* void (*) (gcov_type *, gcov_type, int, unsigned)  */
      interval_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  integer_type_node,
					  unsigned_type_node, NULL_TREE);
      fn_name = concat ("__gcov_interval_profiler", fn_suffix, NULL);
      tree_interval_profiler_fn = build_fn_decl (fn_name,
						 interval_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_interval_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_interval_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_interval_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      pow2_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_pow2_profiler", fn_suffix, NULL);
      tree_pow2_profiler_fn = build_fn_decl (fn_name, pow2_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_pow2_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_pow2_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_pow2_profiler_fn));

      /* void (*) (gcov_type *, gcov_type)  */
      topn_values_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_topn_values_profiler", fn_suffix, NULL);
      tree_topn_values_profiler_fn
	= build_fn_decl (fn_name, topn_values_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));

      TREE_NOTHROW (tree_topn_values_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_topn_values_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_topn_values_profiler_fn));

      init_ic_make_global_vars ();

      /* void (*) (gcov_type, void *)  */
      ic_profiler_fn_type
	       = build_function_type_list (void_type_node,
					  gcov_type_node,
					  ptr_type_node,
					  NULL_TREE);
      fn_name = concat ("__gcov_indirect_call_profiler_v4", fn_suffix, NULL);
      tree_indirect_call_profiler_fn
	= build_fn_decl (fn_name, ic_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));

      TREE_NOTHROW (tree_indirect_call_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_indirect_call_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_indirect_call_profiler_fn));

      tree_time_profiler_counter
	= build_decl (UNKNOWN_LOCATION, VAR_DECL,
		      get_identifier ("__gcov_time_profiler_counter"),
		      get_gcov_type ());
      TREE_PUBLIC (tree_time_profiler_counter) = 1;
      DECL_EXTERNAL (tree_time_profiler_counter) = 1;
      TREE_STATIC (tree_time_profiler_counter) = 1;
      DECL_ARTIFICIAL (tree_time_profiler_counter) = 1;
      DECL_INITIAL (tree_time_profiler_counter) = NULL;

      /* void (*) (gcov_type *, gcov_type)  */
      average_profiler_fn_type
	      = build_function_type_list (void_type_node,
					  gcov_type_ptr, gcov_type_node, NULL_TREE);
      fn_name = concat ("__gcov_average_profiler", fn_suffix, NULL);
      tree_average_profiler_fn = build_fn_decl (fn_name,
						average_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_average_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_average_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_average_profiler_fn));
      fn_name = concat ("__gcov_ior_profiler", fn_suffix, NULL);
      tree_ior_profiler_fn = build_fn_decl (fn_name, average_profiler_fn_type);
      free (CONST_CAST (char *, fn_name));
      TREE_NOTHROW (tree_ior_profiler_fn) = 1;
      DECL_ATTRIBUTES (tree_ior_profiler_fn)
	= tree_cons (get_identifier ("leaf"), NULL,
		     DECL_ATTRIBUTES (tree_ior_profiler_fn));

      /* LTO streamer needs assembler names.  Because we create these decls
         late, we need to initialize them by hand.  */
      DECL_ASSEMBLER_NAME (tree_interval_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_pow2_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_topn_values_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_indirect_call_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_average_profiler_fn);
      DECL_ASSEMBLER_NAME (tree_ior_profiler_fn);
    }
}

/* Output instructions as GIMPLE trees to increment the edge
   execution count, and insert them on E.  We rely on
   gsi_insert_on_edge to preserve the order.  */

void
gimple_gen_edge_profiler (int edgeno, edge e)
{
  tree one;

  one = build_int_cst (gcov_type_node, 1);

  if (flag_profile_update == PROFILE_UPDATE_ATOMIC)
    {
      /* __atomic_fetch_add (&counter, 1, MEMMODEL_RELAXED); */
      tree addr = tree_coverage_counter_addr (GCOV_COUNTER_ARCS, edgeno);
      tree f = builtin_decl_explicit (TYPE_PRECISION (gcov_type_node) > 32
				      ? BUILT_IN_ATOMIC_FETCH_ADD_8:
				      BUILT_IN_ATOMIC_FETCH_ADD_4);
      gcall *stmt = gimple_build_call (f, 3, addr, one,
				       build_int_cst (integer_type_node,
						      MEMMODEL_RELAXED));
      gsi_insert_on_edge (e, stmt);
    }
  else
    {
      tree ref = tree_coverage_counter_ref (GCOV_COUNTER_ARCS, edgeno);
      tree gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
						   NULL, "PROF_edge_counter");
      gassign *stmt1 = gimple_build_assign (gcov_type_tmp_var, ref);
      gcov_type_tmp_var = make_temp_ssa_name (gcov_type_node,
					      NULL, "PROF_edge_counter");
      gassign *stmt2 = gimple_build_assign (gcov_type_tmp_var, PLUS_EXPR,
					    gimple_assign_lhs (stmt1), one);
      gassign *stmt3 = gimple_build_assign (unshare_expr (ref),
					    gimple_assign_lhs (stmt2));
      gsi_insert_on_edge (e, stmt1);
      gsi_insert_on_edge (e, stmt2);
      gsi_insert_on_edge (e, stmt3);
    }
}

/* Emits code to get VALUE to instrument at GSI, and returns the
   variable containing the value.  */

static tree
prepare_instrumented_value (gimple_stmt_iterator *gsi, histogram_value value)
{
  tree val = value->hvalue.value;
  if (POINTER_TYPE_P (TREE_TYPE (val)))
    val = fold_convert (build_nonstandard_integer_type
			  (TYPE_PRECISION (TREE_TYPE (val)), 1), val);
  return force_gimple_operand_gsi (gsi, fold_convert (gcov_type_node, val),
				   true, NULL_TREE, true, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE trees to increment the interval histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_interval_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref = tree_coverage_counter_ref (tag, 0), ref_ptr;
  gcall *call;
  tree val;
  tree start = build_int_cst_type (integer_type_node,
				   value->hdata.intvl.int_start);
  tree steps = build_int_cst_type (unsigned_type_node,
				   value->hdata.intvl.steps);

  ref_ptr = force_gimple_operand_gsi (&gsi,
				      build_addr (ref),
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_interval_profiler_fn, 4,
			    ref_ptr, val, start, steps);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees to increment the power of two histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the tag
   of the section for counters.  */

void
gimple_gen_pow2_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_pow2_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees for code to find the most N common
   values.  VALUE is the expression whose value is profiled.  TAG is the tag
   of the section for counters.  */

void
gimple_gen_topn_values_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_topn_values_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call.
   VALUE is the call expression whose indirect callee is profiled.
   TAG is the tag of the section for counters.  */

void
gimple_gen_ic_profiler (histogram_value value, unsigned tag)
{
  tree tmp1;
  gassign *stmt1, *stmt2, *stmt3;
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);

  /* Insert code:

    stmt1: __gcov_indirect_call.counters = get_relevant_counter_ptr ();
    stmt2: tmp1 = (void *) (indirect call argument value)
    stmt3: __gcov_indirect_call.callee = tmp1;

    Example:
      f_1 = foo;
      __gcov_indirect_call.counters = &__gcov4.main[0];
      PROF_fn_9 = f_1;
      __gcov_indirect_call.callee = PROF_fn_9;
      _4 = f_1 ();
   */

  tree gcov_type_ptr = build_pointer_type (get_gcov_type ());

  tree counter_ref = build3 (COMPONENT_REF, gcov_type_ptr,
			     ic_tuple_var, ic_tuple_counters_field, NULL_TREE);

  stmt1 = gimple_build_assign (counter_ref, ref_ptr);
  tmp1 = make_temp_ssa_name (ptr_type_node, NULL, "PROF_fn");
  stmt2 = gimple_build_assign (tmp1, unshare_expr (value->hvalue.value));
  tree callee_ref = build3 (COMPONENT_REF, ptr_type_node,
			     ic_tuple_var, ic_tuple_callee_field, NULL_TREE);
  stmt3 = gimple_build_assign (callee_ref, tmp1);

  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt2, GSI_SAME_STMT);
  gsi_insert_before (&gsi, stmt3, GSI_SAME_STMT);
}


/* Output instructions as GIMPLE trees for code to find the most
   common called function in indirect call. Insert instructions at the
   beginning of every possible called function.
  */

void
gimple_gen_ic_func_profiler (void)
{
  struct cgraph_node * c_node = cgraph_node::get (current_function_decl);
  gcall *stmt1;
  tree tree_uid, cur_func, void0;

  if (c_node->only_called_directly_p ())
    return;

  gimple_init_gcov_profiler ();

  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block cond_bb = split_edge (single_succ_edge (entry));
  basic_block update_bb = split_edge (single_succ_edge (cond_bb));

  /* We need to do an extra split in order to not create an input
     for a possible PHI node.  */
  split_edge (single_succ_edge (update_bb));

  edge true_edge = single_succ_edge (cond_bb);
  true_edge->flags = EDGE_TRUE_VALUE;

  profile_probability probability;
  if (DECL_VIRTUAL_P (current_function_decl))
    probability = profile_probability::very_likely ();
  else
    probability = profile_probability::unlikely ();

  true_edge->probability = probability;
  edge e = make_edge (cond_bb, single_succ_edge (update_bb)->dest,
		      EDGE_FALSE_VALUE);
  e->probability = true_edge->probability.invert ();

  /* Insert code:

     if (__gcov_indirect_call.callee != NULL)
       __gcov_indirect_call_profiler_v3 (profile_id, &current_function_decl);

     The function __gcov_indirect_call_profiler_v3 is responsible for
     resetting __gcov_indirect_call.callee to NULL.  */

  gimple_stmt_iterator gsi = gsi_start_bb (cond_bb);
  void0 = build_int_cst (ptr_type_node, 0);

  tree callee_ref = build3 (COMPONENT_REF, ptr_type_node,
			    ic_tuple_var, ic_tuple_callee_field, NULL_TREE);

  tree ref = force_gimple_operand_gsi (&gsi, callee_ref, true, NULL_TREE,
				       true, GSI_SAME_STMT);

  gcond *cond = gimple_build_cond (NE_EXPR, ref,
				   void0, NULL, NULL);
  gsi_insert_before (&gsi, cond, GSI_NEW_STMT);

  gsi = gsi_after_labels (update_bb);

  cur_func = force_gimple_operand_gsi (&gsi,
				       build_addr (current_function_decl),
				       true, NULL_TREE,
				       true, GSI_SAME_STMT);
  tree_uid = build_int_cst
	      (gcov_type_node,
	       cgraph_node::get (current_function_decl)->profile_id);
  stmt1 = gimple_build_call (tree_indirect_call_profiler_fn, 2,
			     tree_uid, cur_func);
  gsi_insert_before (&gsi, stmt1, GSI_SAME_STMT);
}

/* Output instructions as GIMPLE tree at the beginning for each function.
   TAG is the tag of the section for counters, BASE is offset of the
   counter position and GSI is the iterator we place the counter.  */

void
gimple_gen_time_profiler (unsigned tag)
{
  tree type = get_gcov_type ();
  basic_block entry = ENTRY_BLOCK_PTR_FOR_FN (cfun);
  basic_block cond_bb = split_edge (single_succ_edge (entry));
  basic_block update_bb = split_edge (single_succ_edge (cond_bb));

  /* We need to do an extra split in order to not create an input
     for a possible PHI node.  */
  split_edge (single_succ_edge (update_bb));

  edge true_edge = single_succ_edge (cond_bb);
  true_edge->flags = EDGE_TRUE_VALUE;
  true_edge->probability = profile_probability::unlikely ();
  edge e
    = make_edge (cond_bb, single_succ_edge (update_bb)->dest, EDGE_FALSE_VALUE);
  e->probability = true_edge->probability.invert ();

  gimple_stmt_iterator gsi = gsi_start_bb (cond_bb);
  tree original_ref = tree_coverage_counter_ref (tag, 0);
  tree ref = force_gimple_operand_gsi (&gsi, original_ref, true, NULL_TREE,
				       true, GSI_SAME_STMT);
  tree one = build_int_cst (type, 1);

  /* Emit: if (counters[0] != 0).  */
  gcond *cond = gimple_build_cond (EQ_EXPR, ref, build_int_cst (type, 0),
				   NULL, NULL);
  gsi_insert_before (&gsi, cond, GSI_NEW_STMT);

  gsi = gsi_start_bb (update_bb);

  /* Emit: counters[0] = ++__gcov_time_profiler_counter.  */
  if (flag_profile_update == PROFILE_UPDATE_ATOMIC)
    {
      tree ptr = make_temp_ssa_name (build_pointer_type (type), NULL,
				     "PROF_time_profiler_counter_ptr");
      tree addr = build1 (ADDR_EXPR, TREE_TYPE (ptr),
			  tree_time_profiler_counter);
      gassign *assign = gimple_build_assign (ptr, NOP_EXPR, addr);
      gsi_insert_before (&gsi, assign, GSI_NEW_STMT);
      tree f = builtin_decl_explicit (TYPE_PRECISION (gcov_type_node) > 32
				      ? BUILT_IN_ATOMIC_ADD_FETCH_8:
				      BUILT_IN_ATOMIC_ADD_FETCH_4);
      gcall *stmt = gimple_build_call (f, 3, ptr, one,
				       build_int_cst (integer_type_node,
						      MEMMODEL_RELAXED));
      tree result_type = TREE_TYPE (TREE_TYPE (f));
      tree tmp = make_temp_ssa_name (result_type, NULL, "PROF_time_profile");
      gimple_set_lhs (stmt, tmp);
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
      tmp = make_temp_ssa_name (type, NULL, "PROF_time_profile");
      assign = gimple_build_assign (tmp, NOP_EXPR,
				    gimple_call_lhs (stmt));
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (original_ref, tmp);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
    }
  else
    {
      tree tmp = make_temp_ssa_name (type, NULL, "PROF_time_profile");
      gassign *assign = gimple_build_assign (tmp, tree_time_profiler_counter);
      gsi_insert_before (&gsi, assign, GSI_NEW_STMT);

      tmp = make_temp_ssa_name (type, NULL, "PROF_time_profile");
      assign = gimple_build_assign (tmp, PLUS_EXPR, gimple_assign_lhs (assign),
				    one);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (original_ref, tmp);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (tree_time_profiler_counter, tmp);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
    }
}

/* Output instructions as GIMPLE trees to increment the average histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_average_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE,
				      true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_average_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

/* Output instructions as GIMPLE trees to increment the ior histogram
   counter.  VALUE is the expression whose value is profiled.  TAG is the
   tag of the section for counters, BASE is offset of the counter position.  */

void
gimple_gen_ior_profiler (histogram_value value, unsigned tag)
{
  gimple *stmt = value->hvalue.stmt;
  gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
  tree ref_ptr = tree_coverage_counter_addr (tag, 0);
  gcall *call;
  tree val;

  ref_ptr = force_gimple_operand_gsi (&gsi, ref_ptr,
				      true, NULL_TREE, true, GSI_SAME_STMT);
  val = prepare_instrumented_value (&gsi, value);
  call = gimple_build_call (tree_ior_profiler_fn, 2, ref_ptr, val);
  gsi_insert_before (&gsi, call, GSI_NEW_STMT);
}

static vec<regex_t> profile_filter_files;
static vec<regex_t> profile_exclude_files;

/* Parse list of provided REGEX (separated with semi-collon) and
   create expressions (of type regex_t) and save them into V vector.
   If there is a regular expression parsing error, error message is
   printed for FLAG_NAME.  */

static void
parse_profile_filter (const char *regex, vec<regex_t> *v,
		      const char *flag_name)
{
  v->create (4);
  if (regex != NULL)
    {
      char *str = xstrdup (regex);
      for (char *p = strtok (str, ";"); p != NULL; p = strtok (NULL, ";"))
	{
	  regex_t r;
	  if (regcomp (&r, p, REG_EXTENDED | REG_NOSUB) != 0)
	    {
	      error ("invalid regular expression %qs in %qs",
		     p, flag_name);
	      return;
	    }

	  v->safe_push (r);
	}
    }
}

/* Parse values of -fprofile-filter-files and -fprofile-exclude-files
   options.  */

static void
parse_profile_file_filtering ()
{
  parse_profile_filter (flag_profile_filter_files, &profile_filter_files,
			"-fprofile-filter-files");
  parse_profile_filter (flag_profile_exclude_files, &profile_exclude_files,
			"-fprofile-exclude-files");
}

/* Parse vectors of regular expressions.  */

static void
release_profile_file_filtering ()
{
  profile_filter_files.release ();
  profile_exclude_files.release ();
}

/* Return true when FILENAME should be instrumented based on
   -fprofile-filter-files and -fprofile-exclude-files options.  */

static bool
include_source_file_for_profile (const char *filename)
{
  /* First check whether file is included in flag_profile_exclude_files.  */
  for (unsigned i = 0; i < profile_exclude_files.length (); i++)
    if (regexec (&profile_exclude_files[i],
		 filename, 0, NULL, 0) == REG_NOERROR)
      return false;

  /* For non-empty flag_profile_filter_files include only files matching a
     regex in the flag.  */
  if (profile_filter_files.is_empty ())
    return true;

  for (unsigned i = 0; i < profile_filter_files.length (); i++)
    if (regexec (&profile_filter_files[i], filename, 0, NULL, 0) == REG_NOERROR)
      return true;

  return false;
}

#ifndef HAVE_sync_compare_and_swapsi
#define HAVE_sync_compare_and_swapsi 0
#endif
#ifndef HAVE_atomic_compare_and_swapsi
#define HAVE_atomic_compare_and_swapsi 0
#endif

#ifndef HAVE_sync_compare_and_swapdi
#define HAVE_sync_compare_and_swapdi 0
#endif
#ifndef HAVE_atomic_compare_and_swapdi
#define HAVE_atomic_compare_and_swapdi 0
#endif

/* Profile all functions in the callgraph.  */

static unsigned int
tree_profiling (void)
{
  struct cgraph_node *node;

  /* Verify whether we can utilize atomic update operations.  */
  bool can_support_atomic = false;
  unsigned HOST_WIDE_INT gcov_type_size
    = tree_to_uhwi (TYPE_SIZE_UNIT (get_gcov_type ()));
  if (gcov_type_size == 4)
    can_support_atomic
      = HAVE_sync_compare_and_swapsi || HAVE_atomic_compare_and_swapsi;
  else if (gcov_type_size == 8)
    can_support_atomic
      = HAVE_sync_compare_and_swapdi || HAVE_atomic_compare_and_swapdi;

  if (flag_profile_update == PROFILE_UPDATE_ATOMIC
      && !can_support_atomic)
    {
      warning (0, "target does not support atomic profile update, "
	       "single mode is selected");
      flag_profile_update = PROFILE_UPDATE_SINGLE;
    }
  else if (flag_profile_update == PROFILE_UPDATE_PREFER_ATOMIC)
    flag_profile_update = can_support_atomic
      ? PROFILE_UPDATE_ATOMIC : PROFILE_UPDATE_SINGLE;

  /* This is a small-ipa pass that gets called only once, from
     cgraphunit.cc:ipa_passes().  */
  gcc_assert (symtab->state == IPA_SSA);

  init_node_map (true);
  parse_profile_file_filtering ();

  FOR_EACH_DEFINED_FUNCTION (node)
    {
      bool thunk = false;
      if (!gimple_has_body_p (node->decl) && !node->thunk)
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      if (lookup_attribute ("no_profile_instrument_function",
			    DECL_ATTRIBUTES (node->decl)))
	continue;
      /* Do not instrument extern inline functions when testing coverage.
	 While this is not perfectly consistent (early inlined extern inlines
	 will get acocunted), testsuite expects that.  */
      if (DECL_EXTERNAL (node->decl)
	  && flag_test_coverage)
	continue;

      const char *file = LOCATION_FILE (DECL_SOURCE_LOCATION (node->decl));
      if (!include_source_file_for_profile (file))
	continue;

      if (node->thunk)
	{
	  /* We cannot expand variadic thunks to Gimple.  */
	  if (stdarg_p (TREE_TYPE (node->decl)))
	    continue;
	  thunk = true;
	  /* When generate profile, expand thunk to gimple so it can be
	     instrumented same way as other functions.  */
	  if (profile_arc_flag || profile_condition_flag)
	    expand_thunk (node, false, true);
	  /* Read cgraph profile but keep function as thunk at profile-use
	     time.  */
	  else
	    {
	      read_thunk_profile (node);
	      continue;
	    }
	}

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));

      if (dump_file)
	dump_function_header (dump_file, cfun->decl, dump_flags);

      /* Local pure-const may imply need to fixup the cfg.  */
      if (gimple_has_body_p (node->decl)
	  && (execute_fixup_cfg () & TODO_cleanup_cfg))
	cleanup_tree_cfg ();

      branch_prob (thunk);

      if (! flag_branch_probabilities
	  && flag_profile_values)
	gimple_gen_ic_func_profiler ();

      if (flag_branch_probabilities
	  && !thunk
	  && flag_profile_values
	  && flag_value_profile_transformations
	  && profile_status_for_fn (cfun) == PROFILE_READ)
	gimple_value_profile_transformations ();

      /* The above could hose dominator info.  Currently there is
	 none coming in, this is a safety valve.  It should be
	 easy to adjust it, if and when there is some.  */
      free_dominance_info (CDI_DOMINATORS);
      free_dominance_info (CDI_POST_DOMINATORS);
      pop_cfun ();
    }

  release_profile_file_filtering ();

  /* Drop pure/const flags from instrumented functions.  */
  if (profile_arc_flag || profile_condition_flag || flag_test_coverage)
    FOR_EACH_DEFINED_FUNCTION (node)
      {
	if (!gimple_has_body_p (node->decl)
	    || !(!node->clone_of
	    || node->decl != node->clone_of->decl))
	  continue;

	/* Don't profile functions produced for builtin stuff.  */
	if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	  continue;

	node->set_const_flag (false, false);
	node->set_pure_flag (false, false);
      }

  /* Update call statements and rebuild the cgraph.  */
  FOR_EACH_DEFINED_FUNCTION (node)
    {
      basic_block bb;

      if (!gimple_has_body_p (node->decl)
	  || !(!node->clone_of
	  || node->decl != node->clone_of->decl))
	continue;

      /* Don't profile functions produced for builtin stuff.  */
      if (DECL_SOURCE_LOCATION (node->decl) == BUILTINS_LOCATION)
	continue;

      push_cfun (DECL_STRUCT_FUNCTION (node->decl));

      FOR_EACH_BB_FN (bb, cfun)
	{
	  gimple_stmt_iterator gsi;
	  for (gsi = gsi_start_bb (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	    {
	      gimple *stmt = gsi_stmt (gsi);
	      if (is_gimple_call (stmt))
		update_stmt (stmt);
	    }
	}

      /* re-merge split blocks.  */
      cleanup_tree_cfg ();
      update_ssa (TODO_update_ssa);

      cgraph_edge::rebuild_edges ();

      pop_cfun ();
    }

  handle_missing_profiles ();

  del_node_map ();
  return 0;
}

namespace {

const pass_data pass_data_ipa_tree_profile =
{
  SIMPLE_IPA_PASS, /* type */
  "profile", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_PROFILE, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  TODO_dump_symtab, /* todo_flags_finish */
};

class pass_ipa_tree_profile : public simple_ipa_opt_pass
{
public:
  pass_ipa_tree_profile (gcc::context *ctxt)
    : simple_ipa_opt_pass (pass_data_ipa_tree_profile, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *);
  virtual unsigned int execute (function *) { return tree_profiling (); }

}; // class pass_ipa_tree_profile

bool
pass_ipa_tree_profile::gate (function *)
{
  /* When profile instrumentation, use or test coverage shall be performed.
     But for AutoFDO, this there is no instrumentation, thus this pass is
     disabled.  */
  return (!in_lto_p && !flag_auto_profile
	  && (flag_branch_probabilities || flag_test_coverage
	      || profile_arc_flag || profile_condition_flag));
}

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_tree_profile (gcc::context *ctxt)
{
  return new pass_ipa_tree_profile (ctxt);
}

#include "gt-tree-profile.h"
