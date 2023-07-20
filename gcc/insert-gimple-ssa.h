/* Hack code generation API.

   Generates GIMPLE in SSA form.

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

struct hphi_edge;
class hstmt;
class hstmt_with_lhs;
class hphi;
class hstmt_assign;
class hstmt_const;
class hstmt_call;
struct hack_tuple_internal;
struct hack_tuple_fn;
class hack_bb;
struct hvar;
class hack_ssa_builder;

// -- API STRUCTS --

/* Hack variable

   INVAR .. Incoming variable. Represents SSA name that generated code uses but
	    originates outside of it
   MEMORY .. Represents a memory access -- a handled component.
   LOCAL .. Local variable. Analogous to a VAR_DECL tree. Represents a variable
	    used only inside the generated code.
   OUTVAR .. Outgoing variable. Doesn't represent an actual object. Represents
	     a point where user of API will want to know SSA name of a LOCAL
	     variable.
   PARAM .. Parameter of cfun. Analogous to a PARM_DECL tree.  */

enum hvar_code
{
  INVAR,
  MEMORY,
  LOCAL,
  OUTVAR,
  PARAM
};

struct hvar
{
  int index; /* Not currently used.  */
  enum hvar_code code;
  tree t; /* LOCAL           ... VAR_DECL
	     anonymous LOCAL ... type
	     PARAM           ... PARM_DECL
	     INVAR           ... null
	     OUTVAR          ... null or SSA name when finalized
	     MEMORY          ... handled component.  */
  hstmt_with_lhs *default_def; /* For PARAM, INVAR and MEMORY.  */
};

// -- INTERNAL STRUCTS --

enum hstmt_code
{
  HPHI,
  HSTMT_ASSIGN,
  HSTMT_CONST,
  HSTMT_COND,
  HSTMT_OUTVAR,
  HSTMT_RETURN,
  HSTMT_CALL,
  HSTMT_HANDLED_COMPONENT
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
       stmt is purely virtual (for example hstmt_outvar is virtual).  */
    virtual gimple *to_gimple (void) { return NULL; }

    /* When substituting one definition (represented by hstmt_with_lhs *) by
       another, this method should be called on all relevant stmts. This is
       usually done by traversing the 'uses' vector of 'hstmt_with_lhs' stmts.  */
    virtual void replace_op_by (hstmt_with_lhs *, hstmt_with_lhs *) { }
    
    virtual ~hstmt () { }
};

/* Hack statement with left side

   Abstract class representing statements that assign to a LOCAL hvar.
   Has vector to keep track of stmts using this stmt.
   When commiting to GIMPLE, 'ssa' field should be defined.  */

class hstmt_with_lhs : public hstmt
{
  public:
    hstmt_with_lhs (hstmt_code code, hvar *var, tree ssa) :
      hstmt (code), var (var), ssa (ssa) { }
    hstmt_with_lhs (hstmt_code code, hvar *var) : hstmt (code), var (var)
      {
	ssa = NULL_TREE;
      }

    ~hstmt_with_lhs () { }

    hvar *var;
    vec<hstmt *> uses = vNULL;
    tree ssa;
};

/* Hack PHI.  */

struct hphi_edge
{
  edge e;
  hstmt_with_lhs *s;
};

class hphi : public hstmt_with_lhs
{
  public:
    unsigned num_ops = 0;
    hphi_edge *op = NULL; /* Array of operands. Not embedded into the object.
			     NULL if PHI incomplete.  */

    hphi (hvar *var) : hstmt_with_lhs (HPHI, var)
      {
	gcc_checking_assert (var->code == LOCAL || var->code == PARAM);
      }

    hstmt_with_lhs *get_op (unsigned i)
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

    virtual void replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by) override;

    ~hphi () { }
};

/* Hack assign stmt.

   'val' contains the tuple of rhs operands along with a rhs code. */

class hstmt_assign : public hstmt_with_lhs
{
  public:
    hack_tuple_internal *val;

    hstmt_assign (hvar *var, hack_tuple_internal *val)
      : hstmt_with_lhs (HSTMT_ASSIGN, var), val (val)
      {
	gcc_checking_assert (var->code != OUTVAR);
      }

    virtual gimple *to_gimple (void) override;
    virtual void replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by)
      override;

    ~hstmt_assign () { }
};

/* Hack const stmt
   
   Virtual statement representing values originating outside the code we're
   building.  */

class hstmt_const : public hstmt_with_lhs
{
  public:
    hstmt_const (hvar *var, tree ssa)
      : hstmt_with_lhs (HSTMT_CONST, var, ssa)
      {
	gcc_checking_assert (var->code != OUTVAR && var->code != MEMORY);
      }

    ~hstmt_const () { }
};

/* Hack handled component stmt

   Virtual statement representing a memory access.  */

class hstmt_handled_component : public hstmt_with_lhs
{
  public:
    vec<hstmt_with_lhs *> operands = vNULL;

    hstmt_handled_component ()
      : hstmt_with_lhs (HSTMT_HANDLED_COMPONENT, NULL) { }

    virtual gimple *to_gimple (void) override;
    virtual void replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by)
      override;

    ~hstmt_handled_component () { }
};

/* Hack cond stmt.  */

class hstmt_cond : public hstmt
{
  public:
    enum tree_code pred_code;
    hstmt_with_lhs *lhs;
    hstmt_with_lhs *rhs;

    hstmt_cond (enum tree_code pred_code, hstmt_with_lhs *lhs, hstmt_with_lhs *rhs)
      : hstmt (HSTMT_COND), pred_code (pred_code), lhs (lhs), rhs (rhs) { }

    virtual gimple *to_gimple (void) override;
    virtual void replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by) override;

    ~hstmt_cond () { }
};

/* Hack outvar stmt
   
   Virtual statement representing places in code where user of API will want to
   know SSA name of a LOCAL.

   'outvar' should be only OUTVAR.  */

class hstmt_outvar : public hstmt
{
  public:
    hvar *outvar;
    hstmt_with_lhs *rhs;

    hstmt_outvar (hvar *outvar, hstmt_with_lhs *rhs) :
      hstmt (HSTMT_OUTVAR), outvar (outvar), rhs (rhs) { }

    virtual gimple *to_gimple (void) override;

    ~hstmt_outvar () { }
};

/* Hack return stmt.  */

class hstmt_return : public hstmt
{
  public:
    hstmt_with_lhs *retval;

    hstmt_return (void) :
      hstmt (HSTMT_RETURN), retval (NULL) { }

    hstmt_return (hstmt_with_lhs *retval) :
      hstmt (HSTMT_RETURN), retval (retval) { }

    virtual gimple *to_gimple (void) override;

    ~hstmt_return () { }
};

/* Hack function call stmt.  */

class hstmt_call : public hstmt_with_lhs
{
  public:
    hack_tuple_fn *val;

    hstmt_call (hvar *var, hack_tuple_fn *val)
      : hstmt_with_lhs (HSTMT_CALL, var), val (val) { }

    virtual gimple *to_gimple (void) override;
    virtual void replace_op_by (hstmt_with_lhs *op, hstmt_with_lhs *replace_by)
      override;

    ~hstmt_call () { }
};

/* Hack internal tuple.

   Right hand side of assign statements. I may merge this into 'hstmt_assign'
   later.  */

struct hack_tuple_internal
{
  enum tree_code code;
  unsigned num_ops;
  hstmt_with_lhs *op[3];
};

/* Hack internal tuple for function calls.

   Right hand side of function call statements.  */

struct hack_tuple_fn
{
  tree fn;
  unsigned num_ops;
  hstmt_with_lhs *op[1];  /* Trailing array idiom.  */
};

/* Hash traits on which redundancy elimination is based.  */

template<>
struct default_hash_traits<hack_tuple_internal>
  : typed_noop_remove <hack_tuple_internal>
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
	h.add_ptr (v.op[i]);
      }
    return h.end ();
  }

  static inline bool
  equal (value_type existing, value_type candidate)
  {
    bool b = true;
    b &= existing.code == candidate.code;
    b &= existing.num_ops == candidate.num_ops;
    unsigned i;
    for (i = 0; i < existing.num_ops; i++)
      {
	b &= existing.op[i] == candidate.op[i];
      }
    return b;
  }

  static inline void mark_deleted (value_type &) { }
  static inline bool is_deleted (const value_type &) { return false; }
  static inline void remove (value_type &) { }
  static inline bool is_empty (const value_type &v) { return v.code == ERROR_MARK; }
  static inline void mark_empty (value_type &v) { v.code = ERROR_MARK; }
  static const bool empty_zero_p = true;
};

/* Hack basic block record

   Contains data structures related to a basic block.  */

class hack_bb
{
  public:
    vec<hstmt *> stmt_list = vNULL;
    vec<hphi *> phi_list = vNULL;
    hash_map<hvar *, hstmt_with_lhs *> curr_def;  /* See the Braun alg paper for
						     what 'curr_def' means.  */
    hash_set<hphi *> incomplete_phis; /* See Braun alg paper for explanation of
					 incomplete PHIs.  */

    /* Remembers seen tuples (rhs of assigns) and which statements contain
       them. Used for build-time redundancy elimination.

       Should probably get renamed.  */
    hash_map<hack_tuple_internal, hstmt_assign *> tuple_provider;
};

// -- SSA BUILDER --

/* Hack SSA builder

   Main class. Usage of the API: Create CFG structure, initialize builder, add
   statements, finalize, extract SSA names, release.  */

class hack_ssa_builder
{
 public:
  hvar *new_local (tree type);
  hvar *new_invar (tree ssa);
  hvar *new_param (tree var);
  void append_assign (basic_block bb, enum tree_code code, hvar *left,
		      hvar *op1);
  void append_assign (basic_block bb, enum tree_code code, hvar *left,
		      hvar *op1, hvar *op2);
  void append_assign (basic_block bb, enum tree_code code, hvar *left,
		      hvar *op1, hvar *op2, hvar *op3);
  void append_cond (basic_block bb, enum tree_code pred_code,
		    hvar *left, hvar *right);
  void append_return (basic_block bb);
  void append_return (basic_block bb, hvar *retval);
  void append_call_vec (basic_block bb, tree fn, hvar *left,
		 	const vec<hvar *> &args);
  void append_call_vec (basic_block bb, tree fn, const vec<hvar *> &args);
  hvar *append_outvar (basic_block bb, hvar *local);
  hvar *append_handled_component (basic_block bb, tree ref,
				  vec<hvar *> &operands);

  void set_block_sealed (basic_block bb);
  void set_block_filled (basic_block bb);

  void finalize (void);
  void release (void);
  tree ssa_from_outvar (hvar *var);

 private:
  bool finalized = false;  /* Have all hack stmts been placed yet?  */
  unsigned next_index = 0;  /* Incrementing counter of LOCAL and OUTVAR
			       indexes. These indexes currently aren't
			       used.  */
  unsigned next_const_index = 0; /* Incrementing counter of INVAR indexes.
				    These indexes currently aren't used.  */

  /* Data structures used to keep track of structs to eventually free.
     Will become obsolete when allocation on obstack is implemented.  */
  vec<hvar *> allocated_hvars = vNULL;
  vec<hack_tuple_internal *> allocated_internal = vNULL;
  vec<hack_tuple_fn *> allocated_tuples_fn = vNULL;

  hash_set<basic_block> seen_bbs;
  hash_map<basic_block, hack_bb *> bb_record_map;
  hash_set<basic_block> sealed_bbs;
  hash_set<basic_block> filled_bbs;

  vec<hstmt_with_lhs *> const_stmts = vNULL;

  void append_assign1 (basic_block bb, enum tree_code code, hvar *left,
		       hvar *op1, hvar *op2, hvar *op3,
		       unsigned num_ops);

  hack_bb *get_bb_record (basic_block bb);
  hack_tuple_internal *tuple_alloc (enum tree_code code, unsigned num_ops);
  hack_tuple_fn *tuple_alloc_fn (tree fn, unsigned num_ops);
  void tuple_set_operand (unsigned op_num, hack_tuple_internal *tuple,
			  hstmt_with_lhs *op);
  void tuple_set_operand_fn (unsigned op_num, hack_tuple_fn *tuple,
			     hstmt_with_lhs *op);
  void append_stmt (basic_block bb, hstmt *stmt);
  hphi *add_empty_phi (basic_block bb, hvar *var);

  void commit_ssa_name (hstmt_with_lhs *s);
  void commit_phi (basic_block bb, hphi *hp);
  void commit_stmt (gimple_stmt_iterator *gsi, hstmt *hs);

  void complete_phi (basic_block bb, hphi *phi);
  void replace_uses (hstmt_with_lhs *to_replace, hstmt_with_lhs *replace_by);
  void try_remove_trivial_phi (hphi *phi);

  void write_variable (basic_block bb, hvar *var, hstmt_with_lhs *stmt);
  hstmt_with_lhs *read_variable (basic_block bb, hvar *var);
  hstmt_with_lhs *read_variable_recursive (basic_block bb, hvar *var);

  void tuple_register (basic_block bb, hstmt_assign *stmt);
  hstmt_assign *tuple_lookup (basic_block bb, hack_tuple_internal *val);

  void run_final_optimizations (void);
};

// -- dyn_cast STUFF --

template<>
struct is_a_helper<hstmt_with_lhs *> : static_is_a_helper<hstmt_with_lhs *>
{
  static inline bool test (const hstmt *p)
    {
      hstmt_code code = p->code;
      switch (code)
	{
	case HSTMT_ASSIGN:
	case HPHI:
	case HSTMT_CONST:
	case HSTMT_CALL:
	case HSTMT_HANDLED_COMPONENT:
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

template<>
struct is_a_helper<hstmt_const *> : static_is_a_helper<hstmt_const *>
{
  static inline bool test (const hstmt *p)
    {
      hstmt_code code = p->code;
      switch (code)
	{
	case HSTMT_CONST:
	  return true;
	default:
	  return false;
	}
    }
};

// -- UTILITY FUNCTIONS --

vec<tree> extract_operands_to_be_renamed (tree ref);

#endif /* GCC_INSERT_GIMPLE_SSA_H */
