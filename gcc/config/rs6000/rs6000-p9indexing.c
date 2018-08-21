/* Subroutines used to transform array subscripting expressions into
   forms that are more amenable to d-form instruction selection for p9
   little-endian VSX code.
   Copyright (C) 1991-2018 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define IN_TARGET_CODE 1

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "df.h"
#include "tm_p.h"
#include "ira.h"
#include "print-tree.h"
#include "varasm.h"
#include "explow.h"
#include "expr.h"
#include "output.h"
#include "tree-pass.h"
#include "rtx-vector-builder.h"

#include "cfgloop.h"
#include "print-rtl.h"
#include "tree-pretty-print.h"

/* This pass transforms array indexing expressions from a form that
   favors selection of X-form instructions into a form that favors
   selection of D-form instructions.

   Showing favor for D-form instructions is especially important when
   targeting Power9, as the Power9 architecture added a number of new
   D-form instruction capabilities.  */

/* Print on FILE the indexes for the predecessors of basic_block BB.  */

static void
print_pred_bbs (FILE *file, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->preds)
    fprintf (file, "bb_%d ", e->src->index);
}


/* Print on FILE the indexes for the successors of basic_block BB.  */

static void
print_succ_bbs (FILE *file, basic_block bb)
{
  edge e;
  edge_iterator ei;

  FOR_EACH_EDGE (e, ei, bb->succs)
    fprintf (file, "bb_%d ", e->dest->index);
}



/* This pass runs after loops have been unrolled.  In the case that an
   unrolled loop body has multiple occurrences of:

   *(array_base + offset)
   offset += element_size;

   we replace this with:

   derived_array_ptr = array_base + offset	(once)
   derived_array_ptr ]constant_offset]		(N times)
   offset += N * element_size;			(once, may become dead)

*/
unsigned int
rs6000_fix_indexing (function *fun)
{
  struct loop *loop;
  int verbosity = 3;

  if (dump_file) {
    fprintf (dump_file, "Kelvin is fixing indices\n");
    print_rtx_function (dump_file, fun, true);
  }

  /* this code should borrow from rs6000_analyze_swaps ()  */

  /* Note even sure if I want to restrict myself to loops, but if I
     did, this code might be useful.  */
  FOR_EACH_LOOP (loop, LI_FROM_INNERMOST)
    {
      extern void kelvin_debug_loop (FILE *, struct loop *, int);
      if (dump_file) {
	fprintf (dump_file,
		 "Kelvin examining loop: %d\n", loop->num);
	if (loop->header)
	  fprintf (dump_file, "header = %d, ", loop->header->index);
	else {
	  fprintf (dump_file, "deleted - nothing to do here\n");
	  return 0;
	}

	if (loop->latch)
          fprintf (dump_file, "latch = %d, ", loop->latch->index);
        else
          fprintf (dump_file, "multiple latches, ");

        fprintf (dump_file, " niter = ");
        print_generic_expr (dump_file, loop->nb_iterations);
        fprintf (dump_file, ")\n");

        if (verbosity >= 1) {
          basic_block bb;
          fprintf (dump_file, "These are the blocks that comprise my loop\n");
          FOR_EACH_BB_FN (bb, cfun) {
            if (bb->loop_father == loop) {
              fprintf (dump_file, "bb_%d (preds = {", bb->index);
              print_pred_bbs (dump_file, bb);
              fprintf (dump_file, "}, succs = {");
              print_succ_bbs (dump_file, bb);
              fprintf (dump_file, "})\n");

              fprintf (dump_file, "insns = {");

              rtx_insn *last = BB_END (bb);
              if (last)
                last = NEXT_INSN (last);
              for (rtx_insn *insn = BB_HEAD (bb);
                   insn != last; insn = NEXT_INSN (insn)) {
                /*
                df_dump_insn_top (insn, dump_file);
                dump_insn_slim (dump_file, insn);
                df_dump_insn_bottom (insn, dump_file);

                probably borrow some code from rs6000-swaps...
                */
                dump_bb (dump_file, bb, 4, TDF_VOPS|TDF_MEMSYMS);
              }
              fprintf (dump_file, "}\n\n");
            }
          }
        }

      }
    }

  // kelvin plan of attack:
  // Borrow code from rs6000-p8swap.c: rs6000_analyze_swaps ()
  // I think I'll need to dataflow analysis to set up use-def
  // chains.
  //  because i need this info to assure that I can "fold"
  //  constants across basic blocks.
  //
  // maybe my pass doesn't need to have anything to do with loop
  // unrolling.  maybe i just look for any sequence of array load
  // and store operations comprised of expressions of the form
  //
  //  load or store from MEM [array + index]
  //  index += constant
  //
  //  and then the same thing all over again...  I need to make
  //  sure the initial expression dominates the subsequent
  //  expressions, and that the new use of index defers only from
  //  the initial use of index in the addition of a constant
  //  value.  In these cases, I'm better off if I replace the code
  //  with:
  //     base_address = array + index;
  //     load or store from MEM [base_address]
  //
  //  in all subsequent uses
  //     just do load or store from MEM [base_address + constant]
  //
  //  go ahead and leave the original code for incrementing index,
  //  but that code may become dead, or may become optimized away
  //  in some other way.

  //
  // the draft experimentation began in loop-unroll.c, but it
  // would be better to put this out as an rs6000-specific
  // optimization, like rs6000-p8swap.

  // We only want to look at the inner-most loop.  so let's
  // break after first iteration.

  return 0;
}  // anon namespace


const pass_data pass_data_fix_indexing =
{
  RTL_PASS, /* type */
  "indexing", /* name */
  OPTGROUP_NONE, /* optinfo_flags, or could use OPTGROUP_LOOP */
  TV_NONE, /* tv_id, or could use TV_LOOP_UNROLL */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_fix_indexing : public rtl_opt_pass
{
public:
  pass_fix_indexing(gcc::context *ctxt)
    : rtl_opt_pass(pass_data_fix_indexing, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      fprintf (stderr, "Kelvin is testing the gating function: %d\n",
	       (optimize > 0 && !BYTES_BIG_ENDIAN && TARGET_VSX
		&& TARGET_P9_VECTOR));

      // This is most relevant to P9 targets since that architecture
      // introduces new D-form instructions, but this may pay off on
      // other architectures as well.  Might want to experiment.
      return (optimize > 0 && !BYTES_BIG_ENDIAN && TARGET_VSX
	      && TARGET_P9_VECTOR);
    }

  virtual unsigned int execute (function *fun)
    {
      return rs6000_fix_indexing (fun);
    }

  opt_pass *clone ()
    {
      return new pass_fix_indexing (m_ctxt);
    }

}; // class pass_fix_indexing

rtl_opt_pass *make_pass_fix_indexing (gcc::context *ctxt)
{
  return new pass_fix_indexing (ctxt);
}
