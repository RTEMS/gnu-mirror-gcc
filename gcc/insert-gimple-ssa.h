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

#include <iostream> // DEBUG

// -- FORWARD DECLARATION --

struct hack_edge_stmt;
class hstmt;
class hstmt_left;
class hphi;
class hstmt_assign;
struct hack_tuple_internal;
class hack_bb;
struct hvar;
class hack_ssa_builder;

// -- API STRUCTS --

/* Hack variable

   INVAR .. Incoming variable. Represents SSA name that generated code uses but
	    originates outside of it
   LOCAL .. Local variable. Analogous to a var_decl. Represents a variable used
            only inside the generated code.
   OUTVAR .. Outgoing variable. Doesn't represent an actual object. Represents
	     a point where user of API will want to know SSA name of a LOCAL
	     variable.  */

enum hvar_code
{
  INVAR,
  LOCAL,
  OUTVAR
};

struct hvar
{
  int index; /* Index only has meaning for INVARs. It is assigned to LOCALs and
		OUTVARs too, however.  */
  enum hvar_code code;
  tree type_or_ssa_value; /* type for LOCAL, ssa for INVAR. OUTVAR has
			     NULL_TREE until finalization when it gets an ssa
			     name.  */
};

// -- INTERNAL STRUCTS --

enum hstmt_code
{
  HPHI,
  HSTMT_ASSIGN,
  HSTMT_CONST,
  HSTMT_COND,
  HSTMT_OUTVAR
};

struct hack_edge_stmt // TODO Nechci spíš pair?
{
  edge e;
  hstmt_left *s;
};

/* Hack statement

   Abstract class for internal representation of statements.  */

class hstmt
{
  public:
    bool killed = false; /* Killed stmts shouldn't be commited. Flag used by
			    optimizations.  */
    hstmt_code code;

    hstmt (hstmt_code code) : code (code) { }

    /* Builds and returns gimple representation of this stmt or NULL if this
       stmt is purely virtual (as in the case of hstmt_outvar).  */
    virtual gimple *to_gimple (void) { }

    /* When substituting one definition (represented by hstmt_left *) by
       another, this method should be called on all relevant stmts. This is
       usually done by traversing the 'uses' vector of 'hstmt_left' stmts.  */
    virtual void replace_op_by (hstmt_left *op, hstmt_left *replace_by) { }
};

/* Hack statement with left side

   Abstract class representing statements that assign to a LOCAL hvar.
   Has vector to keep track of stmts using this stmt.
   When commiting to GIMPLE, 'ssa' field should be defined.  */

class hstmt_left : public hstmt
{
  public:
    hstmt_left (hstmt_code code, hvar *var) : hstmt (code), var (var)
      {
	// TODO Možnost, že var je NULL, když stmt levou stranu nemá
	gcc_checking_assert (var->code == LOCAL);
      }

    hvar *var;
    vec<hstmt *> uses = vNULL; // TODO Nahradit obstack-friendly strukturou?
    tree ssa = NULL_TREE;
};

/* Hack PHI.  */

class hphi : public hstmt_left
{
  public:
    unsigned num_ops = 0;
    hack_edge_stmt *op = NULL; /* Array of operands. Not embedded into object.
				  NULL if PHI incomplete.  */

    hphi (hvar *var) : hstmt_left (HPHI, var) { }

    hstmt_left *get_op (unsigned i)
      {
	gcc_checking_assert (op != NULL && "PHI has to be completed");
	gcc_checking_assert (i < num_ops);

	return op[i].s;
      }

    edge get_edge (unsigned i)
      {
	gcc_checking_assert (op != NULL && "PHI has to be completed");
	gcc_checking_assert (i < num_ops);

	return op[i].e;
      }

    virtual gimple *to_gimple (void) override
      {
	return NULL;
      }

    virtual void replace_op_by (hstmt_left *op, hstmt_left *replace_by) override;
};

/* Hack assign stmt.

   'val' contains the tuple of rhs operands along with a rhs code. */

class hstmt_assign : public hstmt_left
{
  public:
    hack_tuple_internal *val;

    hstmt_assign (hvar *var, hack_tuple_internal *val) :
      hstmt_left (HSTMT_ASSIGN, var), val (val) { }

    virtual gimple *to_gimple (void) override;
    virtual void replace_op_by (hstmt_left *op, hstmt_left *replace_by) override;
};

/* Hack const stmt (will rename this to hack invar stmt)
   
   Virtual statement representing values originating outside the code we're
   building.

   'var' should be only INVAR.  */

class hstmt_const : public hstmt_left
{
  public:
    hstmt_const (hvar *var) : hstmt_left (HSTMT_CONST, var) { }
};

/* Hack cond stmt.  */

class hstmt_cond : public hstmt
{
  public:
    enum tree_code pred_code;
    hstmt_left *lhs;
    hstmt_left *rhs;

    hstmt_cond (enum tree_code pred_code, hstmt_left *lhs, hstmt_left *rhs)
      : hstmt (HSTMT_COND), pred_code (pred_code), lhs (lhs), rhs (rhs) { }

    virtual gimple *to_gimple (void) override;
    virtual void replace_op_by (hstmt_left *op, hstmt_left *replace_by) override;
};

/* Hack outvar stmt
   
   Virtual statement representing places in code where user of API will want to
   know SSA name of a LOCAL.

   'outvar' should be only OUTVAR.  */

class hstmt_outvar : public hstmt
{
  public:
    hvar *outvar;
    hstmt_left *rhs;

    hstmt_outvar (hvar *outvar, hstmt_left *rhs) :
      hstmt (HSTMT_OUTVAR), outvar (outvar), rhs (rhs) { }

    virtual gimple *to_gimple (void) override;
};

/* Hack internal tuple

   Right side of assign statements. I may merge this into 'hstmt_assign'
   later.  */

struct hack_tuple_internal
{
  enum tree_code code;
  unsigned num_ops;
  hstmt_left *op[1];  /* Trailing array idiom.  */
};

/* TODO
template<>
struct default_hash_traits<hack_tuple_internal>
{
  typedef hack_tuple_internal compare_type;
  typedef hack_tuple_internal value_type;

  static inline hashval_t
  hash (value_type v)
  {
    inchash::hash h;
    h.add_int (v.code);
    unsigned i;
    for (i = 0; i < v.num_ops; i++)
      {
	h.add_int (v.op[i]);
      }
    return h.end ();
  }

  static inline bool
  equal (value_type existing, value_type candidate)
  {
    bool b = true;
    b &= existing.code == candidate.code;
    b &= existing.num_ops == candidate.num_ops;
    if (!b)
      return b;
    unsigned i;
    for (i = 0; i < existing.num_ops; i++)
      {
	b &= existing.op[i] == candidate.op[i];
      }
    return b;
  }

  static inline void mark_deleted (value_type &) { }

  static inline bool is_deleted (const value_type &)
  {
    return false;
  }

  static inline void remove (value_type &) { }
};
*/

/* Hack basic block record

   Contains data structures related to a basic block.  */

class hack_bb
{
  public:
    vec<hstmt *> stmt_list = vNULL;
    vec<hphi *> phi_list = vNULL;
    hash_map<hvar *, hstmt_left *> curr_def;  /* See the Braun alg paper for
						 what 'curr_def' means.  */

    /* Remembers seen tuples (rhs of assigns) and which statements contain
       them. Will be used for build-time optimizations.

       TODO Rename this.  */
    //hash_map<hack_tuple_internal, hstmt_assign *> tuple_provider;
};

// -- SSA BUILDER --

/* Hack SSA builder

   Main class. Create CFG structure, initialize builder, add statements,
   finalize, extract SSA names, dispose.  */

class hack_ssa_builder
{
 public:
  hvar &new_local (tree type);
  hvar &new_invar (tree ssa);
  void append_assign (basic_block bb, enum tree_code code, hvar &left,
		      hvar &op1);
  void append_assign (basic_block bb, enum tree_code code, hvar &left,
		      hvar &op1, hvar &op2);
  void append_assign (basic_block bb, enum tree_code code, hvar &left,
		      hvar &op1, hvar &op2, hvar &op3);
  void append_cond (basic_block bb, enum tree_code pred_code,
		    hvar &left, hvar &right);
  hvar &append_outvar (basic_block bb, hvar &local);
  edge hack_make_edge (basic_block src, basic_block dest, int flags);

  void seal_block (basic_block bb);
  void set_block_filled (basic_block bb);

  void finalize (void);
  void dispose (void); // TODO Možná destruktor?
  tree ssa_from_outvar (hvar &var);

 private:
  bool finalized = false;  /* Have all hack stmts been placed yet?  */
  unsigned next_index = 0;  /* Incrementing counter of LOCAL and OUTVAR
			       indexes. These indexes currently aren't
			       used.  */
  unsigned next_const_index = 0; /* Incrementing counter of INVAR indexes.  */

  /* Data structures used to keep track of structs to eventually free.
     Will become obsolete when allocation on obstack is implemented.  */
  vec<hvar *> allocated_hvars = vNULL;
  vec<hack_tuple_internal *> allocated_internal = vNULL;

  vec<hstmt_const *> const_list = vNULL; /* Links INVAR hack vars and their
					    respective hack stmts together
					    using INVAR indexes.  */
  hash_set<basic_block> seen_bbs;
  hash_map<basic_block, hack_bb *> bb_record_map;
  hash_set<basic_block> sealed_bbs;
  hash_set<basic_block> filled_bbs;
  hash_set<hphi *> incomplete_phis; /* See Braun alg paper for explanation of
				       incomplete PHIs.  */

  void append_assign1 (basic_block bb, enum tree_code code, hvar *left,
		       hvar *op1, hvar *op2, hvar *op3,
		       unsigned num_ops);

  hack_bb *get_bb_record (basic_block bb);
  hack_tuple_internal *tuple_alloc (enum tree_code code, unsigned num_ops);
  void tuple_set_operand (unsigned op_num, hack_tuple_internal *tuple,
			  hstmt_left *op);
  void append_stmt (basic_block bb, hstmt *stmt);
  hphi *add_empty_phi (basic_block bb, hvar *var);

  void commit_ssa_name (hstmt_left *s);
  void commit_phi (basic_block bb, hphi *hp);
  void commit_stmt (gimple_stmt_iterator *gsi, hstmt *hs);

  void complete_phi (basic_block bb, hvar *var, hphi *phi);
  void replace_uses (hstmt_left *to_replace, hstmt_left *replace_by);
  void try_remove_trivial_phi (hphi *phi);

  void write_variable (basic_block bb, hvar *var, hstmt_left *stmt);
  hstmt_left *read_variable (basic_block bb, hvar *var);
  hstmt_left *read_variable_recursive (basic_block bb, hvar *var);

  void tuple_register (basic_block bb, hstmt_assign *stmt);
  hstmt_assign *tuple_lookup (basic_block bb, hack_tuple_internal *val);

  void run_final_optimizations (void);
};

// -- dyn_cast STUFF --

template<>
struct is_a_helper<hstmt_left *> : static_is_a_helper<hstmt_left *>
{
  static inline bool test (const hstmt *p)
    {
      hstmt_code code = p->code;
      switch (code)
	{
	case HSTMT_ASSIGN:
	case HPHI:
	case HSTMT_CONST:
	  return true;
	default:
	  return false;
	}
    }
};

template<>
struct is_a_helper<hphi *> : static_is_a_helper<hphi *>
{
  static inline bool test (const hstmt *p)
    {
      hstmt_code code = p->code;
      switch (code)
	{
	case HPHI:
	  return true;
	default:
	  return false;
	}
    }
};

#endif /* GCC_INSERT_GIMPLE_SSA_H */
