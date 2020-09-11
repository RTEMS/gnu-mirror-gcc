/* Support routines for value queries.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com> and
   Andrew MacLeod <amacleod@redhat.com>.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "value-range-equiv.h"
#include "value-query.h"
#include "alloc-pool.h"

// value_query default methods.

bool
value_query::value_on_edge (tree &t, edge, tree name)
{
  return value_of_expr (t, name);
}

bool
value_query::value_of_stmt (tree &t, gimple *, tree name)
{
  if (name)
    return value_of_expr (t, name);
  return false;
}

// range_query default methods.

bool
range_query::range_on_edge (irange &r, edge, tree name)
{
  return range_of_expr (r, name);
}

bool
range_query::range_of_stmt (irange &r, gimple *, tree name)
{
  if (name)
    return range_of_expr (r, name);
  return false;
}

bool
range_query::value_of_expr (tree &t, tree name, gimple *stmt)
{
  int_range_max r;
  if (!irange::supports_type_p (TREE_TYPE (name)))
    return false;
  return (range_of_expr (r, name, stmt) && r.singleton_p (&t));
}

bool
range_query::value_on_edge (tree &t, edge e, tree name)
{
  int_range_max r;
  if (!irange::supports_type_p (TREE_TYPE (name)))
    return false;
  return (range_on_edge (r, e, name) && r.singleton_p (&t));
}

bool
range_query::value_of_stmt (tree &t, gimple *stmt, tree name)
{
  int_range_max r;
  if (!irange::supports_type_p (TREE_TYPE (name)))
    return false;
  return (range_of_stmt (r, stmt, name) && r.singleton_p (&t));
}

// valuation_query support routines for value_range_equiv's.

class equiv_allocator : public object_allocator<value_range_equiv>
{
public:
  equiv_allocator ()
    : object_allocator<value_range_equiv> ("equiv_allocator pool") { }
};

const class value_range_equiv *
range_query::get_value_range (const_tree expr, gimple *stmt)
{
  int_range_max r;
  if (range_of_expr (r, const_cast<tree> (expr), stmt))
    return new (equiv_alloc->allocate ()) value_range_equiv (r);
  return new (equiv_alloc->allocate ()) value_range_equiv (TREE_TYPE (expr));
}

range_query::range_query ()
{
  equiv_alloc = new equiv_allocator;
}

range_query::~range_query ()
{
  equiv_alloc->release ();
}
