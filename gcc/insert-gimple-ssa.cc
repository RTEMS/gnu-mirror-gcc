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

#include "coretypes.h"
#include "tree.h"
#include "insert-gimple-ssa.h"
#include "vec.h"
#include "hash-map.h"
#include "hash-set.h"
#include "basic-block.h"
#include "gimple-iterator.h"
#include "tree-ssanames.h"
#include "tree-phinodes.h"

class hack_ssa_builder
{
public:
    hack_lvalue new_local (type type, optionally name)
    {
      hack_lvalue local;
      local.index = next_index;
      next_index++;
      local.var_decl = build_decl (..., VAR_DECL, ...); // TODO How to build
      return local;
    }

    void add_assignment (basic_block bb, hack_lvalue left, hack_rvalue right)
    {
      hack_stmt_assign stmt;
      stmt.value = rvalue_to_rvalue_vn (right);
      append_stmt (bb, stmt);
      write_variable (bb, left, stmt);
    }

    void seal_block (basic_block bb)
    {
      sealed_bbs.add (bb->index);
    }

    void set_block_filled (basic_block bb)
    {
      filled_bbs.add (bb->index);
      convert_to_gimple_bb (bb);
    }

    tree ssa_name_from_lvalue (basic_block bb, hack_lvalue var)
    {
      gcc_checking_assert (filled_bbs.contains (bb->index));
      return curr_ssa.get (bb->index).get (var);
    }

private:
    int next_index = 0; // TODO Ohandlovat, když je proměnných fakt hodně
    hash_map<int, hash_map<hack_lvalue, *hack_stmt>> curr_defs;
    hash_map<int, hash_map<hack_lvalue, tree>> curr_ssa;
    hash_map<int, vec<hack_stmt>> stmt_lists;
    hash_map<int, vec<hack_phi>> phi_lists;
    hash_set<int> sealed_bbs;
    hash_set<hack_phi> incomplete_phis;

    hash_map<hack_lvalue, *hack_stmt> get_bb_map (basic_block bb)
    {
      hash_map<hack_lvalue, *hack_stmt> *bb_map;
      bb_map = curr_def.get (bb->index);
      if (bb_map == NULL)
	{
	  // TODO Jak vyrábět hash mapy
	  bb_map = new hash_map<hack_lvalue, *hack_stmt>;
	  curr_def.put (bb->index, bb_map);
	}
      return *bb_map;
    }

    vec<hack_stmt> get_stmt_list (basic_block bb)
    {
      vec<hack_stmt> *stmt_list;
      stmt_list = stmt_lists.get (bb->index);
      if (stmt_list == NULL)
	{
	  vec<hack_stmt> foo; // TODO Rozmyslet, jak tady budou fungovat
			      // pointery
	  stmt_list = &foo;
	  stmt_lists.put (bb->index, stmt_list);
	}
      return *stmt_list;
    }

    vec<hack_stmt> get_phi_list (basic_block bb)
    {
      vec<hack_phi> *phi_list;
      phi_list = phi_lists.get (bb->index);
      if (phi_list == NULL)
	{
	  vec<hack_phi> foo; // TODO Rozmyslet, jak tady budou fungovat
			      // pointery
	  phi_list = &foo;
	  phi_lists.put (bb->index, phi_list);
	}
      return *phi_list;
    }

    hash_map<...> get_curr_ssa (basic_block bb)
    {
      // TODO -||-
    }

    hack_rvalue rvalue_to_rvalue_vn (hack_rvalue val)
    {
      // TODO Tohle ještě bude třeba rozmyslet - i s objektovým návrhem
      if (val je hack_lvalue)
	{
	  hack_stmt *s = read_variable (val);
	  return s;
	}
      else
	{
	  hack_rvalue_vn val_vn;
	  val_vn.op = val.op; // Operátor
	  val_vn.left = rvalue_to_rvalue_vn (val.left);
	  val_vn.right = rvalue_to_rvalue_vn (val.right);

	  // TODO Neměl bych tu chybu nějak propagovat?
	  gcc_checking_assert (val_vn.left != NULL);
	  gcc_checking_assert (val_vn.right != NULL);

	  return val_vn;
	  // TODO Rozmyslet: Přeci ne vše bude binární
	}
    }

    void append_stmt (basic_block bb, hack_stmt stmt)
    {
      vec<hack_stmt> stmt_list = get_stmt_list (bb);
      stmt_list.safe_push (stmt);
    }

    void add_phi (basic_block bb, hack_phi phi)
    {
      vec<hack_phi> phi_list = get_phi_list (bb);
      phi_list.safe_push (phi);
    }

    tree rvalue_to_tree (hack_rvalue val)
    {
      // TODO
      // Foldneš jeden typ stromu do jiného typu stromu
      // Je tu vlastně šance narazit na nezkonvertovaný stmt? Pokud ano, asi je
      // třeba zkonvertovat ho
    }

    void convert_to_gimple_phi (basic_block bb, hack_phi hp)
    {
      tree lhs = make_ssa_name (hp.var.type);
      gphi *phi = create_phi_node (hp.var, bb);
      for (int i = 0; i < hp.operands.length (); i++)
	{
	  hack_stmt hs = hp.operands [i];
	  edge e = hp.edges [i];

	  gcc_checking_assert (hs->ssa != NULL_TREE); /* Predecessor blocks
							 should be filled */

	  add_phi_arg (phi, hs->ssa, e, UNKNOWN_LOCATION); // TODO Location?
	}

      get_curr_ssa (gsi->bb)->put (hp.val, lhs);
    }

    /* Contrary to the name, this actually converts AND inserts the stmt  */
    void convert_to_gimple_stmt (gimple_stmt_iterator *gsi, hack_stmt hs)
    {
      // TODO Vyrábí se takhle assign s novým ssa jménem? Bude po tomhle
      // možné z SSA jména získat tenhle assign?
      tree lhs = make_ssa_name (hs.var.type);
      tree rhs = rvalue_to_tree (hs.val);
      gimple *s = gimple_build_assign (lhs, rhs);
      gsi_insert_after (&gsi, s, GSI_NEW_STMT);

      get_curr_ssa (gsi->bb)->put (hs.val, lhs);
    }

    void convert_to_gimple_bb (basic_block bb)
    {
      // NOTE Predecessors jsou filled. Pokud tenhle blok je sealed, jsou
      // dokonce všichni k dispozici. Tedy teď mám dostupné všechny definice,
      // které kdy budu moct mít dostupné.
      // NOTE Ou! Protože jsou predecessors filled, jsou taky converted.

      // TODO Prozatím předpokládám, že vše je assignment
      // TODO Při vytváření LHS SSA jmen jim neassignuji VAR_DECL

      for (hack_phi phi : get_phi_list (bb))
      {
	convert_to_gimple_phi (phi);
      }

      gimple_stmt_iterator gsi;
      gsi = gsi_start_bb (bb);
      for (hack_stmt hs : get_stmt_list (bb))
      {
	convert_to_gimple_stmt (&gsi, hs);
      }
    }

    void add_phi_operand (edge e, hack_phi phi)
    {
      basic_block bb_pred = e->dest; // TODO Nemělo by tady být src místo dest?

      gcc_checking_assert (phi.operands.is_empty ());

      phi.operands.safe_push (read_variable (bb_pred, var));
      phi.edges.safe_push (e);
    }

    hack_phi add_phi_operands (basic_block bb, hack_lvalue var, hack_phi phi)
    {
      // TODO Tuhle funkci by to chtělo přejmenovat. Teď kinda koliduje s
      // funkcemi z tree-phinodes.cc
      edge e;
      edge_iterator ei;

      // TODO Proč ta závorka navíc?
      for (ei = ei_start (bb->preds); (e = ei_safe_edge (ei)); ei_next (&ei))
	{
	  add_phi_operand (e, phi);
	}
    }

    void write_variable (basic_block bb, hack_lvalue var, *hack_stmt *stmt)
    {
      hash_map<hack_lvalue, *hack_stmt> *bb_map = get_bb_map (bb);
      bb_map->put (var, stmt);
    }

    hack_stmt *read_variable (basic_block bb, hack_lvalue var)
    {
      hash_map<hack_lvalue, *hack_stmt> *bb_map = get_bb_map (bb);
      hack_stmt **stmt_p = bb_map->get (var);
      if (stmt_p == NULL)
	{
	  return read_variable_recursive (bb, var);
	}
      return *stmt_p;
    }

    hack_stmt *read_variable_recursive (basic_block bb, hack_lvalue var)
    {
      hack_stmt *stmt;

      if (!sealed_bbs.contains (bb->index))
	{
	  hack_phi phi;
	  stmt = phi;
	  incomplete_phis.add (op);
	  append_stmt (bb, stmt);
	}
      else if (single_pred_p (bb))
	{
	  stmt = read_variable (single_pred (bb), var);
	}
      else if (má vůbec predecessors) // TODO
	{
	  hack_phi phi;
	  stmt = phi;
	  // TODO Proč tady vlastně dělám write_variable 2x?
	  write_variable (bb, var, stmt);
	  stmt = add_phi_operands (var, stmt); // TODO Tady mi nesedí typy
	}
      else
	{
	  // TODO Řeším takhle správně, že definici nenajdu?
	  return NULL;
	}

      write_variable (bb, var, stmt);
      return stmt;
    }
}
