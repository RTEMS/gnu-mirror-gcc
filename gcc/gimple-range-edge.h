/* Gimple range edge header file.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>
   and Aldy Hernandez <aldyh@redhat.com>.

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

#ifndef GIMPLE_RANGE_EDGE_H
#define GIMPLE_RANGE_EDGE_H

class outgoing_range
{
public:
  outgoing_range ();
  ~outgoing_range ();
  gimple *edge_range_p (irange &r, edge e);
private:
  void calc_switch_ranges (gswitch *sw);
  bool get_edge_range (irange &r, gimple *s, edge e);

  hash_map<edge, irange *> *m_edge_table;
  irange_pool range_pool;
}; 


gimple *gimple_outgoing_range_stmt_p (basic_block bb);

#endif  // GIMPLE_RANGE_EDGE_H
