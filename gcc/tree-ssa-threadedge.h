/* Header file for SSA jump threading.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.

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

#ifndef GCC_TREE_SSA_THREADEDGE_H
#define GCC_TREE_SSA_THREADEDGE_H

class jump_thread_edge;
class jump_threader_simplifier;
class vr_values;
class const_and_copies;
class avail_exprs_stack;
class evrp_range_analyzer;

class jump_threader
{
public:
  jump_threader (const_and_copies *, avail_exprs_stack *);
  ~jump_threader ();
  void thread_outgoing_edges (basic_block,
			      evrp_range_analyzer *,
			      jump_threader_simplifier &);

private:
  tree simplify_control_stmt_condition (edge, gimple *,
					jump_threader_simplifier &);
  tree simplify_control_stmt_condition_1 (edge,
					  gimple *,
					  tree op0,
					  tree_code cond_code,
					  tree op1,
					  jump_threader_simplifier &simplify,
					  unsigned limit);

  bool thread_around_empty_blocks (edge,
				   jump_threader_simplifier &,
				   bitmap visited,
				   vec<jump_thread_edge *> *path);
  int thread_through_normal_block (edge,
				   evrp_range_analyzer *,
				   jump_threader_simplifier &,
				   vec<jump_thread_edge *> *path,
				   bitmap visited);
  void thread_across_edge (edge,
			   evrp_range_analyzer *,
			   jump_threader_simplifier &simplify);
  bool record_temporary_equivalences_from_phis (edge,
						evrp_range_analyzer *);
  gimple *record_temporary_equivalences_from_stmts_at_dest (edge,
    evrp_range_analyzer *,
    jump_threader_simplifier &);

  // Dummy condition to avoid creating lots of throw away statements.
  gcond *dummy_cond;

  class const_and_copies *m_const_and_copies;
  class avail_exprs_stack *m_avail_exprs_stack;

  class jump_thread_registry *m_registry;
};

class jump_threader_simplifier
{
public:
  jump_threader_simplifier (vr_values *v) : m_vr_values (v) { }
  virtual tree simplify (gimple *, gimple *, avail_exprs_stack *, basic_block);
protected:
  vr_values *m_vr_values;
};

extern vec<tree> ssa_name_values;
#define SSA_NAME_VALUE(x) \
    (SSA_NAME_VERSION (x) < ssa_name_values.length () \
     ? ssa_name_values[SSA_NAME_VERSION (x)] \
     : NULL_TREE)
extern void set_ssa_name_value (tree, tree);
extern void propagate_threaded_block_debug_into (basic_block, basic_block);

#endif /* GCC_TREE_SSA_THREADEDGE_H */
