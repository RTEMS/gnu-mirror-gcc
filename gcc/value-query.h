/* Support routines for value queries.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com> and
   Andrew Macleod <amacleod@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_QUERY_H
#define GCC_QUERY_H

// value_query is used by optimization passes that require valueizing
// ssa_names in terms of a tree value, but have no neeed for ranges.
//
// value_of_expr is required to be provided.
// value_on_edge and value_of_stmt default to simply calling value_of_expr.
//
// This implies the valuation is global in nature.  If a pass can make use
// of more specific information, then it can override the other queries.
//
// Proper usage of the correct query in passes will enable other valuation
// mechanisms to produce more precise results.

class value_query
{
public:
  virtual bool value_of_expr (tree &, tree name, gimple * = NULL) = 0;
  virtual bool value_on_edge (tree &, edge, tree name);
  virtual bool value_of_stmt (tree &, gimple *, tree name = NULL);
};

// range_query is used by optimization passes which are range aware.
//
// range_of_expr must be provided.  The default for range_on_edge and
// range_of_stmt is to call range_of_expr.  If more precise results can
// be calculated, those functions can be overridden.
//
// The default for the value_* routines is to call the equivalent range_*
// routine, check if the range is a singleton, and return it if so.
//
//  get_value_range is currently provided for compatibility with
//  vr-values.  It will be deprecated when possible.

class range_query : public value_query
{
public:
  range_query ();
  virtual ~range_query ();

  virtual bool value_of_expr (tree &, tree name, gimple * = NULL) OVERRIDE;
  virtual bool value_on_edge (tree &, edge, tree name) OVERRIDE;
  virtual bool value_of_stmt (tree &, gimple *, tree name = NULL) OVERRIDE;

  virtual bool range_of_expr (irange &, tree name, gimple * = NULL) = 0;
  virtual bool range_on_edge (irange &, edge, tree name);
  virtual bool range_of_stmt (irange &, gimple *, tree name = NULL);

  // DEPRECATED: This method is used from vr-values.  The plan is to
  // rewrite all uses of it to the above API.
  virtual const class value_range_equiv *get_value_range (const_tree,
							  gimple * = NULL);

private:
  class equiv_allocator *equiv_alloc;
};

#endif // GCC_QUERY_H
