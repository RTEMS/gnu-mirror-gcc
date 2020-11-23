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

class jump_threader_simplifier
{
public:
  jump_threader_simplifier (class vr_values *v) : m_vr_values (v) { }
  virtual tree simplify (gimple *, gimple *,
			 class avail_exprs_stack *, basic_block);
protected:
  class vr_values *m_vr_values;
};

extern vec<tree> ssa_name_values;
#define SSA_NAME_VALUE(x) \
    (SSA_NAME_VERSION (x) < ssa_name_values.length () \
     ? ssa_name_values[SSA_NAME_VERSION (x)] \
     : NULL_TREE)
extern void set_ssa_name_value (tree, tree);
extern void propagate_threaded_block_debug_into (basic_block, basic_block);

class jump_threader
{
public:
  jump_threader ();
  ~jump_threader ();
  void thread_outgoing_edges (basic_block,
			      const_and_copies *,
			      avail_exprs_stack *,
			      class evrp_range_analyzer *,
			      jump_threader_simplifier &);

private:
  // Dummy condition to avoid creating lots of throw away statements.
  gcond *dummy_cond;
  class jump_thread_paths *blah_blah_m_paths;
};

#endif /* GCC_TREE_SSA_THREADEDGE_H */
