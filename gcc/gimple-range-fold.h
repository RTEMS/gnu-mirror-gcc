/* Header file for the GIMPLE range interface.
   Copyright (C) 2019-2021 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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

#ifndef GCC_GIMPLE_RANGE_FOLD_H
#define GCC_GIMPLE_RANGE_FOLD_H


// Source of an operand for fold.
// it can be range at a specific stmt, range on and edge, or range which 
// originates from a gori_comnpute module.

class op_source
{
public:
  inline op_source (range_query &q, edge e);
  inline op_source (range_query &q, gimple *s, bool process = false);
  inline op_source (range_query &q, class gori_compute *g, edge e, gimple *s);
  // Return true if s in is the source block and there is an oracle present.
  bool get_operand (irange &r, tree expr);
  relation_kind query_relation (tree op1, tree op2);

  gori_compute *m_gori;
  range_query &m_query;
  edge m_edge;
  gimple *m_stmt;
  bool m_process;	// Process relations, dependencies and after-effects.
};

op_source::op_source (range_query &q, edge e) : m_query (q) 
{
  m_gori = NULL;
  m_edge = e;
  m_stmt = NULL;
  m_process = false;
}

op_source::op_source (range_query &q, gimple *s, bool process) : m_query (q)
{
  m_gori = NULL;
  m_edge = NULL;
  m_stmt = s;
  m_process = process;
}

op_source::op_source (range_query &q, gori_compute *g, edge e, gimple *s)
								  : m_query (q)
{
  m_gori = g;
  m_edge = e;
  m_stmt = s;
  m_process = (q.oracle () && e == NULL);
}


class fold_using_range 
{
public:
  bool fold_stmt (irange &r, gimple *s, op_source &src, tree name = NULL_TREE);
//protected:
  bool range_of_range_op (irange &r, gimple *s, op_source &src);
  bool range_of_call (irange &r, gcall *call, op_source &src);
  bool range_of_cond_expr (irange &r, gassign* cond, op_source &src);
  bool range_of_address (irange &r, gimple *s, op_source &src);
  bool range_of_builtin_call (irange &r, gcall *call, op_source &src);
  void range_of_builtin_ubsan_call (irange &r, gcall *call, tree_code code,
				    op_source &src);
  bool range_of_phi (irange &r, gphi *phi, op_source &src);
  void range_of_ssa_name_with_loop_info (irange &, tree, class loop *, gphi *,
					 op_source &src);

  void relation_fold_and_or (irange& lhs_range, gimple *s, op_source &src);
  void postfold_range_op (range_operator *handler, gimple *s, tree lhs,
			  irange &lhs_range, tree op1, const irange &range1,
			  op_source &src);
  void postfold_range_op (range_operator *handler, gimple *s, tree lhs,
			  irange &lhs_range, tree op1, const irange &range1,
			  tree op2, const irange& range2, op_source &src);
  void postfold_gcond_edges (gcond *s, op_source &src);
};


// Calculate a range for statement S and return it in R. If NAME is provided it
// represents the SSA_NAME on the LHS of the statement. It is only required
// if there is more than one lhs/output.  If a range cannot
// be calculated, return false.
//
inline bool
fold_range (range_query &q, irange &r, gimple *s)
{
  fold_using_range f;
  op_source src (q, s);
  return f.fold_stmt (r, s, src);
}

inline bool
fold_range (range_query &q, irange &r, gimple *s, edge e)
{
  fold_using_range f;
  op_source src (q, e);
  return f.fold_stmt (r, s, src);
}


#endif // GCC_GIMPLE_RANGE_FOLD_H
