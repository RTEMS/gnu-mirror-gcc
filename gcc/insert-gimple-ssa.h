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

// -- FORWARD DECLARATION --

struct hack_edge_stmt;
class hstmt;
class hstmt_left;
class hphi;
class hstmt_nonphi;
class hstmt_assign;
class hack_tuple_internal;
struct hack_bb;
class hack_rvalue;
class hack_tuple;
class hack_ssa;
class hack_lvalue;
class hack_ssa_builder;

// -- INTERNAL STRUCTS --

struct hack_edge_stmt // TODO Nechci spíš pair?
{
  edge e;
  hstmt_left *s;
};

class hstmt
{
  public:
    bool killed = false; /* Stmt shouldn't be commited. Set by optimizations.  */

    virtual bool is_phi (void);
    virtual gimple *to_gimple (void);
    virtual void replace_op_by (hstmt_left *op, hstmt_left *replace_by);
};

class hstmt_left : public hstmt
{
  public:
    hack_lvalue *var;
    vec<hstmt *> uses; // TODO Nahradit obstack strukturou?
    tree ssa;
};

class hphi : public hstmt_left
{
  public:
    hack_lvalue *var;
    unsigned num_ops;
    hack_edge_stmt **op_p; /* Pointer to the array of operands. NULL if PHI
			      incomplete.  */

    hstmt_left *get_op (unsigned i)
      {
	gcc_checking_assert (op_p != NULL && "PHI has to be completed");
	gcc_checking_assert (i < num_ops);

	return (*op_p)[i].s;
      }

    edge get_edge (unsigned i)
      {
	gcc_checking_assert (op_p != NULL && "PHI has to be completed");
	gcc_checking_assert (i < num_ops);

	return (*op_p)[i].e;
      }

    virtual bool is_phi (void) override
      {
	return true;
      }

    virtual gimple *to_gimple (void) override
      {
	return NULL;
      }

    virtual void replace_op_by (hstmt_left *op, hstmt_left *replace_by) override;
};

class hstmt_nonphi : public hstmt_left
{
  public:
    virtual bool is_phi (void) override
      {
	return false;
      }
};

class hstmt_assign : public hstmt_nonphi
{
  public:
    hack_tuple_internal *val;

    virtual gimple *to_gimple (void) override;
    virtual void replace_op_by (hstmt_left *op, hstmt_left *replace_by) override;
};

class hack_tuple_internal
{
  public:
    enum tree_code code;
    unsigned num_ops;
    hstmt_left *op[1];
};

struct hack_bb
{
  vec<hstmt *> stmt_list;
  vec<hphi *> phi_list;
  hash_map<hack_lvalue *, hstmt_left *> curr_def;
  hash_map<hack_tuple_internal *, hstmt_left *> tuple_provider; // TODO Jmeno
};

// -- API STRUCTS --

class hack_rvalue { };

class hack_tuple
{
  public:
    enum tree_code code;
    unsigned num_ops;
    hack_rvalue *op[1];
};

class hack_ssa : public hack_rvalue
{
  public:
    tree ssa_name;
};

class hack_lvalue : public hack_rvalue
{
  public:
    int index;
    tree var_tree;
};

// -- SSA BUILDER --

class hack_ssa_builder
{
 public:
  hack_tuple &tuple_alloc (enum tree_code code, unsigned num_ops);
  void tuple_set_operand (unsigned op_num, hack_tuple &tuple, // SPÍŠ POINTER
			  hack_lvalue &val);
  hack_lvalue &new_local (tree var_tree);
  void append_assign (basic_block bb, hack_lvalue &left, hack_tuple &right);
  edge hack_make_edge (basic_block src, basic_block dest, int flags);

  void seal_block (basic_block bb);
  void set_block_filled (basic_block bb);

  void finalize (void);
  tree ssa_name_from_lvalue (basic_block bb, hack_lvalue &var);

 private:
  bool finalized = false;
  unsigned next_index = 0;

  hash_set<basic_block> seen_bbs;
  vec<hack_lvalue *> allocated_locals; // NAHRADI OBSTACK
  vec<hack_tuple *> allocated_tuples;
  vec<hack_tuple_internal *> allocated_internal;

  hash_map<int, hack_bb> bb_record_map;
  hash_set<int> sealed_bbs;
  hash_set<int> filled_bbs;
  hash_set<hphi *> incomplete_phis;

  hack_bb &get_bb_record (basic_block bb);
  hack_tuple_internal *tuple_to_internal (basic_block bb, hack_tuple *tuple);
  void append_stmt (basic_block bb, hstmt *stmt);
  hphi *add_empty_phi (basic_block bb, hack_lvalue *var);

  void commit_ssa_name (hstmt_left *s);
  void commit_phi (basic_block bb, hphi *hp);
  void commit_stmt (gimple_stmt_iterator *gsi, hstmt *hs);

  void complete_phi (basic_block bb, hack_lvalue *var, hphi *phi);
  void replace_uses (hstmt_left *to_replace, hstmt_left *replace_by);
  void try_remove_trivial_phi (hphi *phi);

  void write_variable (basic_block bb, hack_lvalue *var, hstmt_left *stmt);
  hstmt_left *read_variable (basic_block bb, hack_lvalue *var);
  hstmt_left *read_variable_recursive (basic_block bb, hack_lvalue *var);

  void tuple_register (basic_block bb, hstmt_left *stmt);
  hstmt_left *tuple_lookup (basic_block bb, hack_tuple_internal *val);

  void run_final_optimizations (void);
};

#endif /* GCC_INSERT_GIMPLE_SSA_H */
