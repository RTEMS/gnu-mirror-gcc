/* Calculate branch probabilities, and basic block execution counts.
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

#define INCLUDE_ALGORITHM
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

namespace {

struct conds_ctx {
    /* Output arrays allocated by the caller.  */
    basic_block *blocks;
    int *sizes;

    /* The size of the blocks buffer.  This is just bug protection,
       the caller should have allocated enough memory for blocks to never get
       this many elements.
     */
    int maxsize;

    /* Number of expressions found - this value is the number of entries in the
       gcov output and the parameter to gcov_counter_alloc ().
     */
    int exprs;

    /* Bitmap of the processed blocks - bit n set means basic_block->index has
       been processed as a first-in-expression block.  This effectively stops
       loop edges from being taken and subgraphs re-processed.
     */
    auto_sbitmap seen;

    /* Pre-allocate bitmaps for per-function book keeping.  This is pure
       instance reuse and the bitmaps carries no data between function calls.
     */
    auto_sbitmap expr;
    auto_sbitmap reachable;

    explicit conds_ctx (unsigned size) noexcept (true) : maxsize (0), exprs (0),
        seen (size), expr (size), reachable (size)
    {
        bitmap_clear (seen);
    }

    void commit (basic_block top, int nblocks) noexcept (true)
    {
        blocks  += nblocks;
        *sizes  += nblocks;
        maxsize -= nblocks;

        exprs++;
        sizes++;
        *sizes = 0;

        bitmap_set_bit (seen, top->index);
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

int
index_of (const_basic_block needle, const const_basic_block *blocks, int size)
noexcept (true)
{
    for (int i = 0; i < size; i++)
    {
        if (blocks[i] == needle)
            return i;
    }
    return -1;
}

bool
index_lt (const basic_block x, const basic_block y)
noexcept (true)
{
    return x->index < y->index;
}

/* Special cases of the single_*_p and single_*_edge functions in basic-block.h
   that don't consider exception handling or other complex edges.  This helps
   create a view of the CFG with only normal edges - if a basic block has both
   an outgoing fallthrough and exceptional edge [1], it should be considered a
   single-successor.

   [1] if this is not possible, these functions can be removed and replaced by
       their basic-block.h cousins.
 */
bool
single (const vec<edge, va_gc> *edges)
noexcept (true)
{
    int n = EDGE_COUNT (edges);
    if (n == 0)
        return false;

    for (edge e : edges)
        if (e->flags & EDGE_COMPLEX)
            n -= 1;

    return n == 1;
}

edge
single_edge (const vec<edge, va_gc> *edges)
noexcept (true)
{
    for (edge e : edges)
    {
        if (e->flags & EDGE_COMPLEX)
            continue;
        return e;
    }
    gcc_unreachable ();
}

/* Sometimes, for example with function calls and C++ destructors the CFG gets
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
   edge A -> C, without having to construct a new simplified CFG explicitly.

   Such an expression cannot correctly be repreted as two 1-term expressions,
   as it would break the condition masking.
 */
edge
contract_edge (edge e, sbitmap expr)
noexcept (true)
{
    while (true)
    {
        basic_block src  = e->src;
        basic_block dest = e->dest;

        if (!single (dest->succs))
            return e;
        if (!single (dest->preds))
            return e;
        if (!single (src->preds))
            return e;

        edge succe = single_edge (dest->succs);
        if (!single (succe->dest->preds))
            return e;

        bitmap_set_bit (expr, dest->index);
        e = succe;
    }
}

edge
contract_edge_up (edge e, sbitmap expr)
noexcept (true)
{
    while (true)
    {
        basic_block src  = e->src;
        basic_block dest = e->dest;

        if (!single (dest->succs))
            return e;
        if (!single (dest->preds))
            return e;
        if (!single (src->preds))
            return e;

        if (expr) bitmap_set_bit (expr, src->index);
        e = single_pred_edge (src);
    }
}

bool
is_conditional_p (const basic_block b)
noexcept (true)
{
    if (single_succ_p (b))
        return false;

    unsigned t = 0;
    unsigned f = 0;
    for (edge e : b->succs)
    {
        t |= (e->flags & EDGE_TRUE_VALUE);
        f |= (e->flags & EDGE_FALSE_VALUE);
    }
    return t && f;
}

/* The first block in the output will always be the source block of the edge
   that will apply the masking operation, with the remaining blocks effectively
   unordered.
 */
int
find_conditions_masked_by (
    basic_block block,
    const sbitmap expr,
    const unsigned *flag,
    basic_block *out,
    int maxsize)
noexcept (true)
{
    int n = 0;
    for (edge e : block->preds)
    {
        /* Skip any predecessor not in the expression - there might be such an
           edge to the enclosing expression or in the presence of loops, but
           masking cannot happen outside the expression itself.
         */
        if (!bitmap_bit_p (expr, e->src->index))
            continue;

        e = contract_edge_up (e, NULL);
        if (e->flags & flag[0])
            out[n++] = e->src;
    }

    if (n > 1)
    {
        basic_block *top = std::max_element (out, out + n, index_lt);
        std::iter_swap (top, out);
    }

    for (int pos = 0; pos < n; pos++)
    {
        for (edge e : out[pos]->preds)
        {
            if (!bitmap_bit_p (expr, e->src->index))
                continue;
            if (index_of (e->src, out, n) != -1)
                continue;

            e = contract_edge_up (e, NULL);
            if (e->flags & flag[1])
                out[n++] = e->src;

            gcc_assert (n < maxsize);
        }
    }

    return n;
}

/* Scan the blocks that make up an expression and look for conditions that
   would mask other conditions.  For a deeper discussion on masking, see
   Whalen, Heimdahl, De Silva "Efficient Test Coverage Measurement for MC/DC".
   Masking is best illustrated with an example:

   A || B.  If B is true then A will does not independently affect the decision.
   In a way, this is "reverse" short circuiting, and always work on the
   right-hand side of expressions.  * || true is always true and * && false is
   always false - the left-hand-side does not affect the outcome, and their
   values should not contribute to modifidied condition/decision coverage.

   A || B interpreted as a decision diagram becomes:

   A
  t|\f
   | \
   |  B
   |t/ \f
   |/   \
   T     F

   The algorithm looks for triangles like ATB.  Masking right-hand sides happen
   when a block has a pair of incoming edges of the same boolean value, and
   there is an edge connecting the two predecessors with the *opposite* boolean
   value.  The masking block is B, and the masked block is A.  In this
   particular example:

   Masking can affect "outside" its own subexpression; in A || (B && C) if C is
   false, B is masked.  However, if (B && C) is true, A gets masked.  B && C
   can be determined from evaluating C since !B would short-circuit, so a true
   C would mask A.

   A
  t|\f
   | \
   |  \
   |   \
   |    B
   |  t/ \f
   |  C   |
   |t/ \f |
   |/   \ |
   T     F

   Notice how BFC forms a triangle.  Expressions masked by an edge are
   determined by:
   * Go to the predecessor with truth value b (if it exists).
   * Follow the path of ancestors with taking only !b edges, recording every
     node from here on.

   For the case where C can mask A, the path goes C [true]-> B -> [false] A, so
   C [true] masks A.

   The mask is output as a bit-set stored in a gcov_type_unsigned.  The
   bit-sets are output in pairs, one for each decision (the outcome of the
   boolean expression, or which arc to take in the CFG).
 */
void
find_subexpr_masks (
    const basic_block *blocks,
    int nblocks,
    gcov_type_unsigned *masks)
noexcept (true)
{
    const unsigned flags[] = {
        EDGE_TRUE_VALUE,
        EDGE_FALSE_VALUE,
        EDGE_TRUE_VALUE,
    };

    basic_block path[CONDITIONS_MAX_TERMS];
    auto_sbitmap expr (n_basic_blocks_for_fn (cfun));
    bitmap_clear (expr);
    for (int i = 0; i < nblocks; i++)
        bitmap_set_bit (expr, blocks[i]->index);

    for (int i = 0; i < nblocks; i++)
    {
        basic_block block = blocks[i];
        if (single_pred_p (block))
            continue;

        for (int k = 0; k < 2; k++)
        {
            const int n = find_conditions_masked_by
                (block, expr, flags + k, path, CONDITIONS_MAX_TERMS);

            if (n < 2)
                continue;

            const int m = 2*index_of (path[0], blocks, nblocks) + k;
            for (int i = 1; i < n; i++)
            {
                const int index = index_of (path[i], blocks, nblocks);
                masks[m] |= gcov_type_unsigned (1) << index;
            }
        }
    }
}

int
collect_reachable_conditionals (
    basic_block pre,
    basic_block post,
    basic_block *out,
    int maxsize,
    sbitmap expr)
noexcept (true)
{
    gcc_assert (maxsize > 0);

    basic_block loop = pre->loop_father->header;
    int n = 0;
    out[n++] = pre;
    bitmap_set_bit (expr, pre->index);

    for (int pos = 0; pos < n; pos++)
    {
        basic_block block = out[pos];

        for (edge e : block->succs)
        {
            basic_block dest = contract_edge (e, expr)->dest;

            /* Skip loop edges, as they go outside the expression.  */
            if (dest == loop)
                continue;
            if (dest == post)
                continue;
            if (!is_conditional_p (dest))
                continue;
            /* Already-seen, don't re-add.  */
            if (bitmap_bit_p (expr, dest->index))
                continue;

            bitmap_set_bit (expr, dest->index);
            out[n++] = dest;
            if (n == maxsize)
                return n;
        }
    }

    return n;
}

int
collect_neighbors (basic_block *blocks, int nblocks, sbitmap reachable)
noexcept (true)
{
    int n = 0;
    basic_block *exits = blocks + nblocks;
    for (int i = 0; i < nblocks; i++)
    {
        for (edge e : blocks[i]->succs)
        {
            if (bitmap_bit_p (reachable, e->dest->index))
                continue;

            bitmap_set_bit (reachable, e->dest->index);
            exits[n++] = e->dest;
        }
    }

    return n;
}

/* Find and isolate the first expression between two dominators.

   Either block of a conditional could have more decisions and loops, so
   isolate the first decision by set-intersecting all paths from the
   post-dominator to the entry block.

   The function returns the number of blocks from n that make up the leading
   expression in prefix order (i.e. the order expected by the instrumenting
   code).  When this function returns 0 there are no decisions between pre and
   post, and this segment of the CFG can safely be skipped.

   The post nodes can have predecessors that do not belong to this subgraph,
   which are skipped.  This is expected, for example when there is a
   conditional in the else-block of a larger expression:

   if (A)
{
      if (B) {}
   } else {
      if (C) {} else {}
   }

             A
          t / \ f
           /   \
          B     C
         /\    / \
        /  \  T   F
       T    \  \ /
        \   |   o
         \  |  /
          \ | /
           \|/
            E

   Processing [B, E) which looks like:

      B
     /|
    / |
   T  |
    \ /
     E ----> o // predecessor outside [B, e)
 */

/* Do a (upwards) search for reachable nodes and mark them in the reachable
   set, making sure not to take loop edges.  dfs_enumerate_from () won't work as
   the filter function needs information from the edge.
 */
void
find_reachable (
    sbitmap reachable,
    basic_block pre,
    basic_block post,
    basic_block *stack)
noexcept (true)
{
    stack[0] = pre;
    bitmap_set_bit (reachable, pre->index);
    bitmap_set_bit (reachable, post->index);
    for (int n = 0; n >= 0; n--)
    {
        for (edge e : stack[n]->preds)
        {
            if (bitmap_bit_p (reachable, e->src->index))
                continue;

            /* Ignore any loop edges.  */
            if (dominated_by_p (CDI_DOMINATORS, e->src, e->dest))
                continue;

            basic_block src = contract_edge_up (e, reachable)->src;
            bitmap_set_bit (reachable, src->index);
            stack[n++] = src;
        }
    }
}

int
find_first_conditional (conds_ctx &ctx, basic_block pre, basic_block post)
noexcept (true)
{
    basic_block *blocks = ctx.blocks;
    sbitmap expr = ctx.expr;
    sbitmap reachable = ctx.reachable;
    bitmap_clear (expr);
    bitmap_clear (reachable);
    blocks[0] = pre;

    /* If there is a direct edge to the post dominator then this cannot only be
       a single-term conditional *unless* it is a loop (in which case the
       to-post edge is the loop exit edge).
     */
    const bool dowhile = !loop_exits_from_bb_p (pre->loop_father, pre);
    const bool isloop = bb_loop_header_p (pre) && !dowhile;
    if (find_edge (pre, post) && !isloop)
        return 1;

    const int nblocks = collect_reachable_conditionals
        (pre, post, blocks, ctx.maxsize, expr);
    if (nblocks == 1)
        return nblocks;

    bitmap_copy (reachable, expr);
    const int nexits = collect_neighbors (blocks, nblocks, reachable);
    if (nexits == 2)
        return nblocks;

    /* Find reachable nodes from the neighbors.  */
    basic_block *exits = blocks + nblocks;
    for (int i = 0; i < nexits; i++)
    {
        for (edge e : exits[i]->preds)
        {
            if (!dominated_by_p (CDI_DOMINATORS, e->src, pre))
                continue;

            bitmap_clear (reachable);
            find_reachable (reachable, e->src, pre, exits + nexits);
            for (edge f : exits[i]->preds)
                bitmap_set_bit (reachable, f->src->index);
            bitmap_and (expr, expr, reachable);
        }
    }

    int k = 0;
    for (int i = 0; i < nblocks; i++)
        if (bitmap_bit_p (expr, blocks[i]->index))
            blocks[k++] = blocks[i];
    return k;
}

void
emit_bitwise_op (edge e, tree lhs, tree op1, tree_code op, tree op2)
noexcept (true)
{
    tree tmp;
    gassign *read;
    gassign *bitw;
    gassign *write;
    /* Insert lhs = op1 <bit-op> op2.  */
    tmp   = make_temp_ssa_name  (gcov_type_node, NULL, "__conditions_tmp");
    read  = gimple_build_assign (tmp, op1);
    tmp   = make_temp_ssa_name  (gcov_type_node, NULL, "__conditions_tmp");
    bitw  = gimple_build_assign (tmp, op, gimple_assign_lhs (read), op2);
    write = gimple_build_assign (lhs, gimple_assign_lhs (bitw));

    gsi_insert_on_edge (e, read);
    gsi_insert_on_edge (e, bitw);
    gsi_insert_on_edge (e, write);
}

/* Walk the CFG and collect conditionals.

   1. Collect all nodes reachable from the root node through (contracted) paths
      of true/false edges.
   2. Collect the neighbors of the reachable node (set).
   3. From every node in the neighborhood, walk the up the CFG and mark every
      reachable node. Only the nodes reachable from *every* node in the
      neighborhood are a part of the first expression.
   4. Record the expression plus the two successors of the last (highest-index)
      node in the expression, i.e. the last term.
   5. Repeat using the two successors as new root nodes.

   It is not guaranteed to find nodes in the order of the expression, i.e. it
   might find (a || b) && c as [a c b], so the output is sorted by
   basic_block->index.

   Steps 2 and 3 are necessary to distinguish chained conditionals from
   multi-term conditionals, e.g. to separate

       if (a)
       {
           if (b)
               work ();
       }
       if (a && b)
           work ();
 */
void
collect_conditions (conds_ctx& ctx, basic_block entry, basic_block exit)
noexcept (true)
{
    basic_block pre;
    basic_block post;
    for (pre = entry ;; pre = post)
    {
        if (pre == exit)
            break;
        if (bitmap_bit_p (ctx.seen, pre->index))
            break;

        post = get_immediate_dominator (CDI_POST_DOMINATORS, pre);
        if (!is_conditional_p (pre))
        {
            for (edge e : pre->succs)
                collect_conditions (ctx, e->dest, post);
            continue;
        }

        int nterms = find_first_conditional (ctx, pre, post);
        std::sort (ctx.blocks, ctx.blocks + nterms, index_lt);
        basic_block last = ctx.blocks[nterms - 1];
        if (size_t (nterms) <= CONDITIONS_MAX_TERMS)
        {
            for (edge e : last->succs)
                if (e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE))
                    ctx.blocks[nterms++] = e->dest;
            ctx.commit (pre, nterms);
        } else {
            location_t loc = gimple_location (gsi_stmt (gsi_last_bb (pre)));
            warning_at
                (loc, OPT_Wcoverage_too_many_conditions,
                 "Too many conditions (found %d); giving up coverage", nterms);
        }

        for (edge e : last->succs)
            collect_conditions (ctx, e->dest, post);
    }
}

}

/* Condition coverage (MC/DC)

   Algorithm
   ---------
   This is a modified version of the algorithm in Whalen, Heimdahl, De Silva
   "Efficient Test Coverage Measurement for MC/DC".  Their algorithm work on
   ASTs, but this algorithm work on control flow graphs.  The individual phases
   are described in more detail closer to the implementation.

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
find_conditions (
    basic_block entry,
    basic_block exit,
    basic_block *blocks,
    int *sizes,
    int maxsize)
{
    record_loop_exits ();
    bool free_dom = false;
    bool free_post_dom = false;
    if (!dom_info_available_p (CDI_POST_DOMINATORS))
    {
        calculate_dominance_info (CDI_POST_DOMINATORS);
        free_post_dom = true;
    }

    if (!dom_info_available_p (CDI_DOMINATORS))
    {
        calculate_dominance_info (CDI_DOMINATORS);
        free_dom = true;
    }

    conds_ctx ctx (maxsize);
    ctx.blocks = blocks;
    ctx.sizes = sizes + 1;
    ctx.maxsize = maxsize;
    sizes[0] = sizes[1] = 0;
    collect_conditions (ctx, entry, exit);

    /* Partial sum.  */
    for (int i = 0; i < ctx.exprs; ++i)
        sizes[i + 1] += sizes[i];

    if (free_post_dom)
        free_dominance_info (CDI_POST_DOMINATORS);
    if (free_dom)
        free_dominance_info (CDI_DOMINATORS);

    return ctx.exprs;
}

int instrument_decisions (basic_block *blocks, int nblocks, int condno)
{
    /* Insert function-local accumulators per decision.  */
    tree accu[2] = {
        build_decl (
            UNKNOWN_LOCATION,
            VAR_DECL,
            get_identifier ("__conditions_accu_true"),
            gcov_type_node),
        build_decl (
            UNKNOWN_LOCATION,
            VAR_DECL,
            get_identifier ("__conditions_accu_false"),
            gcov_type_node),
    };
    for (tree acc : accu)
    {
        tree zero = build_int_cst (gcov_type_node, 0);
        for (edge e : ENTRY_BLOCK_PTR_FOR_FN (cfun)->succs)
            gsi_insert_on_edge (e, gimple_build_assign (acc, zero));
    }

    auto_vec<gcov_type_unsigned, 32> masks (nblocks * 2);
    masks.quick_grow_cleared (nblocks * 2);
    find_subexpr_masks (blocks, nblocks, masks.address ());

    /* The true/false target blocks are included in the nblocks set, but
       their outgoing edges should not be instrumented.
     */
    gcc_assert (nblocks > 2);

    /* Add instructions for updating the function-local accumulators.  */
    for (int i = 0; i < nblocks - 2; i++)
    {
        for (edge e : blocks[i]->succs)
        {
            if (!(e->flags & (EDGE_TRUE_VALUE | EDGE_FALSE_VALUE)))
                continue;

            /* accu |= expr[i] */
            const int t = !!(e->flags & EDGE_FALSE_VALUE);
            tree rhs = build_int_cst (gcov_type_node, 1ULL << i);
            emit_bitwise_op (e, accu[t], accu[t], BIT_IOR_EXPR, rhs);

            if (masks[2*i + t] == 0)
                continue;

            /* accu &= mask[i] */
            tree mask = build_int_cst (gcov_type_node, ~masks[2*i + t]);
            for (int j = 0; j < 2; j++)
                emit_bitwise_op (e, accu[j], accu[j], BIT_AND_EXPR, mask);
        }
    }

    /* Add instructions for updating the global accumulators.  */
    basic_block exit = EXIT_BLOCK_PTR_FOR_FN (cfun);
    for (edge e : exit->preds)
    {
        for (int k = 0; k < 2; k++)
        {
            tree ref = tree_coverage_counter_ref
                (GCOV_COUNTER_CONDS, 2*condno + k);

            tree tmp = make_temp_ssa_name
                (gcov_type_node, NULL, "__conditions_tmp");
            gassign *read = gimple_build_assign (tmp, ref);
            gsi_insert_on_edge (e, read);

            tree rop = gimple_assign_lhs (read);
            emit_bitwise_op (e, unshare_expr (ref), accu[k], BIT_IOR_EXPR, rop);
        }
    }

    return nblocks - 2;
}

#undef CONDITIONS_MAX_TERMS

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
      PROF_9 = f_1;
      __gcov_indirect_call.callee = PROF_9;
      _4 = f_1 ();
   */

  tree gcov_type_ptr = build_pointer_type (get_gcov_type ());

  tree counter_ref = build3 (COMPONENT_REF, gcov_type_ptr,
			     ic_tuple_var, ic_tuple_counters_field, NULL_TREE);

  stmt1 = gimple_build_assign (counter_ref, ref_ptr);
  tmp1 = make_temp_ssa_name (ptr_type_node, NULL, "PROF");
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
				     "time_profiler_counter_ptr");
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
      tree tmp = make_temp_ssa_name (result_type, NULL, "time_profile");
      gimple_set_lhs (stmt, tmp);
      gsi_insert_after (&gsi, stmt, GSI_NEW_STMT);
      tmp = make_temp_ssa_name (type, NULL, "time_profile");
      assign = gimple_build_assign (tmp, NOP_EXPR,
				    gimple_call_lhs (stmt));
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
      assign = gimple_build_assign (original_ref, tmp);
      gsi_insert_after (&gsi, assign, GSI_NEW_STMT);
    }
  else
    {
      tree tmp = make_temp_ssa_name (type, NULL, "time_profile");
      gassign *assign = gimple_build_assign (tmp, tree_time_profiler_counter);
      gsi_insert_before (&gsi, assign, GSI_NEW_STMT);

      tmp = make_temp_ssa_name (type, NULL, "time_profile");
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
	  if (profile_arc_flag)
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
  if (profile_arc_flag || flag_test_coverage)
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
	      || profile_arc_flag));
}

} // anon namespace

simple_ipa_opt_pass *
make_pass_ipa_tree_profile (gcc::context *ctxt)
{
  return new pass_ipa_tree_profile (ctxt);
}

#include "gt-tree-profile.h"
