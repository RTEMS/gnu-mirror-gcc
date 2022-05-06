/* Control flow redundancy hardening.
   Copyright (C) 2022 Free Software Foundation, Inc.
   Contributed by Alexandre Oliva <oliva@adacore.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "fold-const.h"
#include "gimple.h"
#include "gimplify.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-iterator.h"
#include "tree-cfg.h"
#include "basic-block.h"
#include "cfghooks.h"
#include "cfgloop.h"
#include "cgraph.h"
#include "alias.h"
#include "varasm.h"
#include "output.h"
#include "langhooks.h"
#include "diagnostic.h"
#include "intl.h"

namespace {

/* This pass introduces verification, at function exits, that booleans
   set in each basic block during function execution reflect the
   control flow graph: for each visited block, check that at least one
   predecessor and at least one successor were also visited.  This
   sort of hardening may detect various kinds of attacks.  */

/* Define a pass to harden code through control flow redundancy.  */

const pass_data pass_data_harden_control_flow_redundancy = {
  GIMPLE_PASS,
  "hardcfr",
  OPTGROUP_NONE,
  TV_NONE,
  PROP_cfg | PROP_ssa, // properties_required
  0,	    // properties_provided
  0,	    // properties_destroyed
  TODO_cleanup_cfg, // properties_start
  TODO_update_ssa
  | TODO_cleanup_cfg
  | TODO_verify_il, // properties_finish
};

class pass_harden_control_flow_redundancy : public gimple_opt_pass
{
public:
  pass_harden_control_flow_redundancy (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_harden_control_flow_redundancy, ctxt)
  {}
  opt_pass *clone () { return new pass_harden_control_flow_redundancy (m_ctxt); }
  virtual bool gate (function *fun) {
    /* Return quickly if the pass is disabled, without checking any of
       the conditions that might give rise to warnings that would only
       be appropriate if hardening was requested.  */
    if (!flag_harden_control_flow_redundancy)
      return false;

    /* We don't verify when an exception escapes, propagated or raised
       by the function itself, so we're only concerned with edges to
       the exit block.  If there aren't any, the function doesn't
       return normally, so there won't be any checking point, so
       there's no point in running the pass.  Should we add
       verification at exception escapes, we should at least look at
       !flag_exceptions here.  */
    if (EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (fun)->preds) == 0)
      return false;

    /* Functions that return more than once, like setjmp and vfork
       (that also gets this flag set), will start recording a path
       after the first return, and then may take another path when
       they return again.  The unterminated path may then be flagged
       as an error.  ??? We could save the visited array before the
       call and restore it if it returns again.  */
    if (fun->calls_setjmp)
      {
	if (flag_harden_control_flow_redundancy < 0)
	  return false;
	warning_at (DECL_SOURCE_LOCATION (fun->decl), 0,
		    "%qD calls %<setjmp%> or similar,"
		    " %<-fharden-control-flow-redundancy%> is not supported",
		    fun->decl);
	return false;
      }

    /* Some targets bypass the abnormal dispatcher block in nonlocal
       gotos, and then we'd miss its visited bit.  It might be doable
       to make it work uniformly, but this feature is not used often
       enough to make it worthwhile.  */
    if (fun->has_nonlocal_label)
      {
	if (flag_harden_control_flow_redundancy < 0)
	  return false;
	warning_at (DECL_SOURCE_LOCATION (fun->decl), 0,
		    "%qD receives nonlocal gotos,"
		    " %<-fharden-control-flow-redundancy%> is not supported",
		    fun->decl);
	return false;
      }

    if (param_hardcfr_max_blocks > 0
	&& n_basic_blocks_for_fn (fun) - 2 > param_hardcfr_max_blocks)
      {
	if (flag_harden_control_flow_redundancy < 0)
	  return false;
	warning_at (DECL_SOURCE_LOCATION (fun->decl), 0,
		    "%qD has more than %u blocks, the requested"
		    " maximum for %<-fharden-control-flow-redundancy%>",
		    fun->decl, param_hardcfr_max_blocks);
	return false;
      }

    return true;
  }
  virtual unsigned int execute (function *);
};

}

class rt_bb_visited
{
  /* Use a sufficiently wide unsigned type to hold basic block numbers.  */
  typedef size_t blknum;

  /* Record the original block count of the function.  */
  blknum nblocks;
  /* Record the number of bits per VWORD (short for VISITED WORD), an
     efficient mode to set and test bits for blocks we visited, and to
     encode the CFG in case out-of-line verification is used.  */
  unsigned vword_bits;

  /* Hold the unsigned integral VWORD type.  */
  tree vword_type;
  /* Hold a pointer-to-VWORD type.  */
  tree vword_ptr;

  /* Hold a growing sequence used to check, inline or out-of-line,
     that VISITED encodes an expected execution path.  */
  gimple_seq ckseq;
  /* If nonNULL, hold a growing representation of the CFG for
     out-of-line testing.  */
  tree rtcfg;

  /* Hold the declaration of an array of VWORDs, used as an array of
     NBLOCKS-2 bits.  */
  tree visited;

  /* If performing inline checking, hold a declarations of boolean
     variables used for inline checking.  CKBLK holds the result of
     testing whether the VISITED bit corresponding to a predecessor or
     successor is set, CKINV inverts that bit, CKPART gets cleared if
     a block was not visited or if CKINV for any of its predecessors
     or successors is set, and CKFAIL gets set if CKPART remains set
     at the end of a block's predecessors or successors list.  */
  tree ckfail, ckpart, ckinv, ckblk;

  /* Convert a block index N to a block vindex, the index used to
     identify it in the VISITED array.  Check that it's in range:
     neither ENTRY nor EXIT, but maybe one-past-the-end, to compute
     the visited array length.  */
  blknum num2idx (blknum n) {
    gcc_checking_assert (n >= 2 && n <= nblocks);
    return (n - 2);
  }
  /* Return the block vindex for BB, that must not be ENTRY or
     EXIT.  */
  blknum bb2idx (basic_block bb) {
    gcc_checking_assert (bb != ENTRY_BLOCK_PTR_FOR_FN (cfun)
			 && bb != EXIT_BLOCK_PTR_FOR_FN (cfun));
    gcc_checking_assert (blknum (bb->index) < nblocks);
    return num2idx (bb->index);
  }

  /* Compute the type to be used for the VISITED array.  */
  tree vtype ()
  {
    blknum n = num2idx (nblocks);
    return build_array_type_nelts (vword_type,
				   (n + vword_bits - 1) / vword_bits);
  }

  /* Compute and return the index into VISITED for block BB.  If BITP
     is non-NULL, also compute and store the bit mask corresponding to
     block BB in *BITP, so that (visited[index] & mask) tells whether
     BB was visited.  */
  tree vwordidx (basic_block bb, tree *bitp = NULL)
  {
    blknum idx = bb2idx (bb);
    if (bitp)
      {
	unsigned bit = idx % vword_bits;
	if (BITS_BIG_ENDIAN)
	  bit = vword_bits - bit - 1;
	wide_int wbit = wi::set_bit_in_zero (bit, vword_bits);
	*bitp = wide_int_to_tree (vword_type, wbit);
      }
    return build_int_cst (vword_ptr, idx / vword_bits);
  }

  /* Return an expr to accesses the visited element that holds
     information about BB.  If BITP is non-NULL, set it to the mask to
     tell which bit in that expr refers to BB.  */
  tree vword (basic_block bb, tree *bitp = NULL)
  {
    return build2 (MEM_REF, vword_type,
		   build1 (ADDR_EXPR, vword_ptr, visited),
		   int_const_binop (MULT_EXPR, vwordidx (bb, bitp),
				    fold_convert (vword_ptr,
						  TYPE_SIZE_UNIT
						  (vword_type))));
  }

  /* Return an expr that evaluates to true iff BB was marked as
     VISITED.  Add any gimple stmts to SEQP.  */
  tree vindex (basic_block bb, gimple_seq *seqp)
  {
    if (bb == ENTRY_BLOCK_PTR_FOR_FN (cfun)
	|| bb == EXIT_BLOCK_PTR_FOR_FN (cfun))
      return boolean_true_node;

    tree bit, setme = vword (bb, &bit);
    tree temp = create_tmp_var (vword_type, ".cfrtemp");

    gassign *vload = gimple_build_assign (temp, setme);
    gimple_seq_add_stmt (seqp, vload);

    gassign *vmask = gimple_build_assign (temp, BIT_AND_EXPR, temp, bit);
    gimple_seq_add_stmt (seqp, vmask);

    return build2 (NE_EXPR, boolean_type_node,
		   temp, build_int_cst (vword_type, 0));
  }

  /* Set the bit corresponding to BB in VISITED.  Add to SEQ any
     required gimple statements, and return SEQ, possibly
     modified.  */
  gimple_seq vset (basic_block bb, gimple_seq seq = NULL)
  {
    tree bit, setme = vword (bb, &bit);
    tree temp = create_tmp_var (vword_type, ".cfrtemp");

    gassign *vload = gimple_build_assign (temp, setme);
    gimple_seq_add_stmt (&seq, vload);

    gassign *vbitset = gimple_build_assign (temp, BIT_IOR_EXPR, temp, bit);
    gimple_seq_add_stmt (&seq, vbitset);

    gassign *vstore = gimple_build_assign (unshare_expr (setme), temp);
    gimple_seq_add_stmt (&seq, vstore);

    return seq;
  }

public:
  /* Prepare to add control flow redundancy testing to CFUN.  */
  rt_bb_visited ()
    : nblocks (n_basic_blocks_for_fn (cfun)),
      vword_type (NULL), ckseq (NULL), rtcfg (NULL)
  {
    /* If we've already added a declaration for the builtin checker,
       extract vword_type and vword_bits from its declaration.  */
    if (tree checkfn = builtin_decl_explicit (BUILT_IN___HARDCFR_CHECK))
      {
	tree check_arg_list = TYPE_ARG_TYPES (TREE_TYPE (checkfn));
	tree vword_const_ptr_type = TREE_VALUE (TREE_CHAIN (check_arg_list));
	vword_type = TYPE_MAIN_VARIANT (TREE_TYPE (vword_const_ptr_type));
	vword_bits = tree_to_shwi (TYPE_SIZE (vword_type));
      }
    /* Otherwise, select vword_bits, vword_type et al, and use it to
       declare the builtin checker.  */
    else
      {
	/* This setting needs to be kept in sync with libgcc/hardcfr.c.
	   We aim for at least 28 bits, which enables us to refer to as
	   many as 28 << 28 blocks in a function's CFG.  That's way over
	   4G blocks.  */
	machine_mode VWORDmode;
	if (BITS_PER_UNIT >= 28)
	  {
	    VWORDmode = QImode;
	    vword_bits = BITS_PER_UNIT;
	  }
	else if (BITS_PER_UNIT >= 14)
	  {
	    VWORDmode = HImode;
	    vword_bits = 2 * BITS_PER_UNIT;
	  }
	else
	  {
	    VWORDmode = SImode;
	    vword_bits = 4 * BITS_PER_UNIT;
	  }

	vword_type = lang_hooks.types.type_for_mode (VWORDmode, 1);
	gcc_checking_assert (vword_bits == tree_to_shwi (TYPE_SIZE
							 (vword_type)));

	vword_type = build_variant_type_copy (vword_type);
	TYPE_ALIAS_SET (vword_type) = new_alias_set ();

	tree vword_const = build_qualified_type (vword_type, TYPE_QUAL_CONST);
	tree vword_const_ptr = build_pointer_type (vword_const);
	tree type = build_function_type_list (void_type_node, sizetype,
					      vword_const_ptr, vword_const_ptr,
					      NULL_TREE);
	tree decl = add_builtin_function_ext_scope
	  ("__builtin___hardcfr_check",
	   type, BUILT_IN___HARDCFR_CHECK, BUILT_IN_NORMAL,
	   "__hardcfr_check", NULL_TREE);
	TREE_NOTHROW (decl) = true;
	set_builtin_decl (BUILT_IN___HARDCFR_CHECK, decl, true);
      }

    /* The checker uses a qualified pointer, so we can't reuse it,
       so build a new one.  */
    vword_ptr = build_pointer_type (vword_type);

    tree visited_type = vtype ();
    visited = create_tmp_var (visited_type, ".cfrvisited");

    /* Prevent stores into visited from being used to optimize the
       control flow redundancy checks. asm ("" : "+m" (visited)); */
    vec<tree, va_gc> *inputs = NULL;
    vec<tree, va_gc> *outputs = NULL;
    vec_safe_push (outputs,
		   build_tree_list
		   (build_tree_list
		    (NULL_TREE, build_string (2, "=m")),
		    visited));
    vec_safe_push (inputs,
		   build_tree_list
		   (build_tree_list
		    (NULL_TREE, build_string (1, "m")),
		    visited));
    gasm *detach = gimple_build_asm_vec ("", inputs, outputs,
					 NULL, NULL);
    gimple_seq_add_stmt (&ckseq, detach);

    if (nblocks - 2 > blknum (param_hardcfr_max_inline_blocks)
	|| !single_pred_p (EXIT_BLOCK_PTR_FOR_FN (cfun)))
      {
	/* Make sure vword_bits is wide enough for the representation
	   of nblocks in rtcfg.  Compare with vword_bits << vword_bits,
	   but avoiding overflows, shifting nblocks right instead.  If
	   vword_bits is wider than HOST_WIDE_INT, assume it fits, so
	   as to avoid undefined shifts.  */
	gcc_assert (HOST_BITS_PER_WIDE_INT <= vword_bits
		    || (((unsigned HOST_WIDE_INT)(num2idx (nblocks))
			 >> vword_bits) < vword_bits));

	/* Build a terminator for the constructor list.  */
	rtcfg = build_tree_list (NULL_TREE, NULL_TREE);
	return;
      }

    ckfail = create_tmp_var (boolean_type_node, ".cfrfail");
    ckpart = create_tmp_var (boolean_type_node, ".cfrpart");
    ckinv = create_tmp_var (boolean_type_node, ".cfrinv");
    ckblk = create_tmp_var (boolean_type_node, ".cfrblk");

    gassign *ckfail_init = gimple_build_assign (ckfail, boolean_false_node);
    gimple_seq_add_stmt (&ckseq, ckfail_init);
  }

  /* Insert SEQ on E, or close enough (e.g., before a noreturn or tail
     call at the end of E->src).  */
  void insert_exit_check (gimple_seq seq, edge e)
  {
    basic_block insbb = e->src;

    /* If the returning block ends with a noreturn call, insert
       checking before it.  This is particularly important for
       __builtin_return.  Other noreturn calls won't have an edge to
       the exit block, so they won't even be considered as exit paths.

       Insert-on-edge inserts before other return stmts, but not
       before calls, and if a single-block function had the check
       sequence inserted after a noreturn call, it would be useless,
       but checking would still flag it as malformed if block 2 has a
       fallthru edge to the exit block.

       Also avoid disrupting tail calls, inserting the check before
       them.  This works for must-tail calls, but tail calling as an
       optimization is detected too late for us.  */
    gimple_stmt_iterator gsi = gsi_last_bb (insbb);
    gimple *ret = gsi_stmt (gsi);
    if (ret && is_a <greturn *> (ret))
      {
	gsi_prev (&gsi);
	if (!gsi_end_p (gsi))
	  ret = gsi_stmt (gsi);
      }
    if (ret && is_a <gcall *> (ret)
	&& (gimple_call_noreturn_p (ret)
	    || gimple_call_must_tail_p (as_a <gcall *> (ret))
	    || gimple_call_tail_p (as_a <gcall *> (ret))))
      gsi_insert_seq_before (&gsi, seq, GSI_SAME_STMT);
    else
      gsi_insert_seq_on_edge_immediate (e, seq);
  }

  /* Add checking code on every exit edge, and initialization code on
     the entry edge.  Before this point, the CFG has been undisturbed,
     and all the needed data has been collected and safely stowed.  */
  void check ()
  {
    /* Insert initializers for visited at the entry.  */
    gimple_seq iseq = NULL;

    gcall *vinit = gimple_build_call (builtin_decl_explicit
				      (BUILT_IN_MEMSET), 3,
				      build1 (ADDR_EXPR,
					      build_pointer_type
					      (TREE_TYPE (visited)),
					      visited),
				      integer_zero_node,
				      TYPE_SIZE_UNIT (TREE_TYPE (visited)));
    gimple_seq_add_stmt (&iseq, vinit);

    gsi_insert_seq_on_edge_immediate (single_succ_edge
				      (ENTRY_BLOCK_PTR_FOR_FN (cfun)),
				      iseq);

    /* If we're using out-of-line checking, create and statically
       initialize the CFG checking representation, generate the
       checker call for the checking sequence, and insert it in all
       exit edges, if there's more than one.  If there's only one, we
       use the same logic as the inline case to insert the check
       sequence.  */
    if (rtcfg)
      {
	/* Unreverse the list, and drop the tail node turned into head.  */
	rtcfg = TREE_CHAIN (nreverse (rtcfg));

	/* Turn the indices stored in TREE_PURPOSE into separate
	   nodes.  It was useful to keep them together to enable
	   combination of masks and for clear separation of
	   terminators while constructing it, but now we have to turn
	   it into a sequence of words.  */
	for (tree node = rtcfg; node; node = TREE_CHAIN (node))
	  {
	    tree wordidx = TREE_PURPOSE (node);
	    if (!wordidx)
	      continue;

	    TREE_PURPOSE (node) = NULL_TREE;
	    TREE_CHAIN (node) = tree_cons (NULL_TREE,
					   fold_convert (vword_type, wordidx),
					   TREE_CHAIN (node));
	  }

	/* Build the static initializer for the array with the CFG
	   representation for out-of-line checking.  */
	tree init = build_constructor_from_list (NULL_TREE, rtcfg);
	TREE_TYPE (init) = build_array_type_nelts (vword_type,
						   CONSTRUCTOR_NELTS (init));
	char buf[32];
	ASM_GENERATE_INTERNAL_LABEL (buf, "Lhardcfg",
				     current_function_funcdef_no);
	rtcfg = build_decl (UNKNOWN_LOCATION, VAR_DECL,
			    get_identifier (buf),
			    TREE_TYPE (init));
	TREE_READONLY (rtcfg) = 1;
	TREE_STATIC (rtcfg) = 1;
	TREE_ADDRESSABLE (rtcfg) = 1;
	TREE_USED (rtcfg) = 1;
	DECL_ARTIFICIAL (rtcfg) = 1;
	DECL_IGNORED_P (rtcfg) = 1;
	DECL_INITIAL (rtcfg) = init;
	make_decl_rtl (rtcfg);
	varpool_node::finalize_decl (rtcfg);

	/* Add the checker call to ckseq.  */
	gcall *call_chk = gimple_build_call (builtin_decl_explicit
					     (BUILT_IN___HARDCFR_CHECK), 3,
					     build_int_cst (sizetype,
							    num2idx (nblocks)),
					     build1 (ADDR_EXPR, vword_ptr,
						     visited),
					     build1 (ADDR_EXPR, vword_ptr,
						     rtcfg));
	gimple_seq_add_stmt (&ckseq, call_chk);

	/* If we have multiple exit edges, insert (copies of)
	   ckseq in all of them.  */
	for (int i = EDGE_COUNT (EXIT_BLOCK_PTR_FOR_FN (cfun)->preds);
	     i--; )
	  {
	    gimple_seq seq = ckseq;
	    /* Copy the sequence, unless we're dealing with the
	       last edge (we're counting down to zero).  */
	    if (i)
	      seq = gimple_seq_copy (seq);

	    insert_exit_check (seq,
			       EDGE_PRED (EXIT_BLOCK_PTR_FOR_FN (cfun), i));
	  }
      }
    else
      {
	/* Inline checking requires a single exit edge.  */
	gimple *last = gsi_stmt (gsi_last (ckseq));

	insert_exit_check (ckseq,
			   single_pred_edge (EXIT_BLOCK_PTR_FOR_FN (cfun)));

	/* The inserted ckseq computes CKFAIL at LAST.  Now we have to
	   conditionally trap on it.  */
	basic_block insbb = gimple_bb (last);

	/* Create a block with the unconditional trap.  */
	basic_block trp = create_empty_bb (insbb);
	gimple_stmt_iterator gsit = gsi_after_labels (trp);

	gcall *trap = gimple_build_call (builtin_decl_explicit
					 (BUILT_IN_TRAP), 0);
	gsi_insert_before (&gsit, trap, GSI_SAME_STMT);

	if (BB_PARTITION (insbb))
	  BB_SET_PARTITION (trp, BB_COLD_PARTITION);

	if (current_loops)
	  add_bb_to_loop (trp, current_loops->tree_root);

	/* Insert a conditional branch to the trap block.  If the
	   conditional wouldn't be the last statement, split the
	   block.  */
	gimple_stmt_iterator gsi = gsi_for_stmt (last);
	if (!gsi_one_before_end_p (gsi))
	  split_block (gsi_bb (gsi), gsi_stmt (gsi));

	gcond *cond = gimple_build_cond (NE_EXPR, ckfail,
					 fold_convert (TREE_TYPE (ckfail),
						       boolean_false_node),
					 NULL, NULL);
	gsi_insert_after (&gsi, cond, GSI_SAME_STMT);

	/* Adjust the edges.  */
	single_succ_edge (gsi_bb (gsi))->flags &= ~EDGE_FALLTHRU;
	single_succ_edge (gsi_bb (gsi))->flags |= EDGE_FALSE_VALUE;
	make_edge (gsi_bb (gsi), trp, EDGE_TRUE_VALUE);

	/* Set the trap's dominator after splitting.  */
	if (dom_info_available_p (CDI_DOMINATORS))
	  set_immediate_dominator (CDI_DOMINATORS, trp, gimple_bb (last));
      }
  }

  /* Push onto RTCFG a (mask, index) pair to test for IBB when BB is
     visited. XSELF is to be the ENTRY or EXIT block (depending on
     whether we're looking at preds or succs), to be remapped to BB
     because we can't represent them, and there's no point in testing
     them anyway.  Return true if no further blocks need to be visited
     in the list, because we've already encountered a
     self-reference.  */
  bool
  push_rtcfg_pair (basic_block ibb, basic_block bb,
		   basic_block xself)
  {
    /* We don't have a bit to test for the entry and exit
       blocks, but it is always visited, so we test for the
       block itself, which gets us the right result and
       enables the self-test optimization below.  */
    if (ibb == xself)
      ibb = bb;

    tree mask, idx = vwordidx (ibb, &mask);
    /* Combine masks with the same idx, but not if we're going
       to optimize for self-test.  */
    if (ibb != bb && TREE_PURPOSE (rtcfg)
	&& tree_int_cst_equal (idx, TREE_PURPOSE (rtcfg)))
      TREE_VALUE (rtcfg) = int_const_binop (BIT_IOR_EXPR, mask,
					    TREE_VALUE (rtcfg));
    else
      rtcfg = tree_cons (idx, mask, rtcfg);

    /* For self-tests (i.e., tests that the block itself was
       also visited), testing anything else is pointless,
       because it's a tautology, so just drop other edges.  */
    if (ibb == bb)
      {
	while (TREE_PURPOSE (TREE_CHAIN (rtcfg)))
	  TREE_CHAIN (rtcfg) = TREE_CHAIN (TREE_CHAIN (rtcfg));
	return true;
      }

    return false;
  }

  /* Add to CKSEQ statements to clear CKPART if OBB is visited.  */
  void
  build_block_check (basic_block obb)
  {
    tree vobb = fold_convert (TREE_TYPE (ckblk),
			      vindex (obb, &ckseq));
    gassign *blkrunp = gimple_build_assign (ckblk, vobb);
    gimple_seq_add_stmt (&ckseq, blkrunp);

    gassign *blknotrunp = gimple_build_assign (ckinv,
					       EQ_EXPR,
					       ckblk,
					       fold_convert
					       (TREE_TYPE (ckblk),
						boolean_false_node));
    gimple_seq_add_stmt (&ckseq, blknotrunp);

    gassign *andblk = gimple_build_assign (ckpart,
					   BIT_AND_EXPR,
					   ckpart, ckinv);
    gimple_seq_add_stmt (&ckseq, andblk);
  }

  /* Add to BB code to set its bit in VISITED, and add to RTCFG or
     CKSEQ the data or code needed to check BB's predecessors and
     successors.  Do NOT change the CFG.  */
  void visit (basic_block bb)
  {
    /* Set the bit in VISITED when entering the block.  */
    gimple_stmt_iterator gsi = gsi_after_labels (bb);
    gsi_insert_seq_before (&gsi, vset (bb), GSI_SAME_STMT);

    if (rtcfg)
      {
	/* Build a list of (index, mask) terminated by (NULL, 0).
	   Consolidate masks with the same index when they're
	   adjacent.  First, predecessors.  Count backwards, because
	   we're going to reverse the list.  The order shouldn't
	   matter, but let's not make it surprising.  */
	for (int i = EDGE_COUNT (bb->preds); i--; )
	  if (push_rtcfg_pair (EDGE_PRED (bb, i)->src, bb,
			       ENTRY_BLOCK_PTR_FOR_FN (cfun)))
	    break;
	rtcfg = tree_cons (NULL_TREE, build_int_cst (vword_type, 0), rtcfg);

	/* Then, successors.  */
	for (int i = EDGE_COUNT (bb->succs); i--; )
	  if (push_rtcfg_pair (EDGE_SUCC (bb, i)->dest, bb,
			       EXIT_BLOCK_PTR_FOR_FN (cfun)))
	    break;
	rtcfg = tree_cons (NULL_TREE, build_int_cst (vword_type, 0), rtcfg);
      }
    else
      {
	/* Schedule test to fail if the block was reached but somehow none
	   of its predecessors were.  */
	tree bit = fold_convert (TREE_TYPE (ckpart), vindex (bb, &ckseq));
	gassign *blkrunp = gimple_build_assign (ckpart, bit);
	gimple_seq_add_stmt (&ckseq, blkrunp);

	for (int i = 0, e = EDGE_COUNT (bb->preds); i < e; i++)
	  build_block_check (EDGE_PRED (bb, i)->src);
	gimple *orfailp = gimple_build_assign (ckfail, BIT_IOR_EXPR,
					       ckfail, ckpart);
	gimple_seq_add_stmt (&ckseq, orfailp);

	/* Likewise for successors.  */
	gassign *blkruns = gimple_build_assign (ckpart, unshare_expr (bit));
	gimple_seq_add_stmt (&ckseq, blkruns);

	for (int i = 0, e = EDGE_COUNT (bb->succs); i < e; i++)
	  build_block_check (EDGE_SUCC (bb, i)->dest);

	gimple *orfails = gimple_build_assign (ckfail, BIT_IOR_EXPR,
					       ckfail, ckpart);
	gimple_seq_add_stmt (&ckseq, orfails);
      }
  }
};

/* Control flow redundancy hardening: record the execution path, and
   verify at exit that an expect path was taken.  */

unsigned int
pass_harden_control_flow_redundancy::execute (function *)
{
  rt_bb_visited vstd;

  basic_block bb;
  FOR_EACH_BB_FN (bb, cfun)
    vstd.visit (bb);

  vstd.check ();

  return 0;
}

/* Instantiate a hardcfr pass.  */

gimple_opt_pass *
make_pass_harden_control_flow_redundancy (gcc::context *ctxt)
{
  return new pass_harden_control_flow_redundancy (ctxt);
}
