/* TODO Popis
   Copyright (C) 2023-2023 Free Software Foundation, Inc.
   Contributed by Filip Kastl <filip.kastl@gmail.com>

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

#ifndef GCC_INSERT_GIMPLE_SSA_H
#define GCC_INSERT_GIMPLE_SSA_H

struct hack_rvalue {};
struct hack_lvalue : hack_rvalue
{
  int index;
  tree var_decl;
  tree type;
};

//struct hack_rvalue_2 : hack_rvalue {};
// rvalue s 2 operandy

struct hack_stmt : hack_rvalue
{
  hack_lvalue *var;
  hack_rvalue *val; // Invariant: V stmt už rvalues neobsahují lvalues, ale stmts
  tree ssa = NULL_TREE;
};
struct hack_stmt_assign : hack_stmt {};
struct hack_phi : hack_stmt
{
  vec<edge> edges;
  vec<*hack_stmt> operands;
};

#endif /* GCC_INSERT_GIMPLE_SSA_H */
