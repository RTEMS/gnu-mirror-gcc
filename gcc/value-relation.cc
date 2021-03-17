/* Header file for the value range relational processing.
   Copyright (C) 2020 Free Software Foundation, Inc.
   Contributed by Andrew MacLeod <amacleod@redhat.com>

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"

#include "gimple-range.h"
#include "value-relation.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "alloc-pool.h"
#include "dominance.h"


// These VREL codes are arranged such that VREL_NONE is the first
// code, and all the rest are contiguous up to and including VREL_LAST.

#define VREL_FIRST              VREL_NONE
#define VREL_LAST               NE_EXPR
#define VREL_COUNT              (VREL_LAST - VREL_FIRST + 1)

// vrel_range_assert will either assert that the tree code passed is valid,
// or mark invalid codes as unreachable to help with table optimation.
#if CHECKING_P
  #define vrel_range_assert(c) 			\
    gcc_checking_assert ((c) >= VREL_FIRST && (c) <= VREL_LAST)
#else
  #define vrel_range_assert(c)			\
    if ((c) < VREL_FIRST || (c) > VREL_LAST)	\
      gcc_unreachable ();
#endif

static const char *kind_string[VREL_COUNT] =
{ "none", "<", "<=", ">", ">=", "empty", "==", "!=" };

// Print a relation_kind REL to file F.

void
print_relation (FILE *f, relation_kind rel)
{
  vrel_range_assert (rel);
  fprintf (f, " %s ", kind_string[rel - VREL_FIRST]);
}

// This table is used to negate the operands.  op1 REL op2 -> !(op1 REL op2).
relation_kind rr_negate_table[VREL_COUNT] = {
//     NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY,      EQ_EXPR, NE_EXPR
  VREL_NONE, GE_EXPR, GT_EXPR, LE_EXPR, LT_EXPR, VREL_EMPTY, NE_EXPR, EQ_EXPR };

// Negate the relation, as in true vs false.

relation_kind
relation_negate (relation_kind r)
{
  vrel_range_assert (r);
  return rr_negate_table [r - VREL_FIRST];
}

// This table is used to swap the operands.  op1 REL op2 -> op2 REL op1.
relation_kind rr_swap_table[VREL_COUNT] = {
//     NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY,      EQ_EXPR, NE_EXPR
  VREL_NONE, GT_EXPR, GE_EXPR, LT_EXPR, LE_EXPR, VREL_EMPTY, EQ_EXPR, NE_EXPR };

// Return the relation as if the operands were swapped.

relation_kind
relation_swap (relation_kind r)
{
  vrel_range_assert (r);
  return rr_swap_table [r - VREL_FIRST];
}

// This table is used to perform an intersection between 2 relations.

relation_kind rr_intersect_table[VREL_COUNT][VREL_COUNT] = {
//   NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY, EQ_EXPR, NE_EXPR
// VREL_NONE
  { VREL_NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, VREL_EMPTY, EQ_EXPR, NE_EXPR },
// LT_EXPR
  { LT_EXPR, LT_EXPR, LT_EXPR, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, LT_EXPR },
// LE_EXPR
  { LE_EXPR, LT_EXPR, LE_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY, EQ_EXPR, LT_EXPR },
// GT_EXPR
  { GT_EXPR, VREL_EMPTY, VREL_EMPTY, GT_EXPR, GT_EXPR, VREL_EMPTY, VREL_EMPTY, GT_EXPR },
// GE_EXPR
  { GE_EXPR, VREL_EMPTY, EQ_EXPR, GT_EXPR, GE_EXPR, VREL_EMPTY, EQ_EXPR, GT_EXPR },
// VREL_EMPTY
  { VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY, VREL_EMPTY },
// EQ_EXPR
  { EQ_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY, EQ_EXPR, VREL_EMPTY },
// NE_EXPR
  { NE_EXPR, LT_EXPR, LT_EXPR, GT_EXPR, GT_EXPR, VREL_EMPTY, VREL_EMPTY, NE_EXPR } };


// Intersect THIS relation with relation P if possible.  Return TRUE if
// this relation changes at all.

relation_kind
relation_intersect (relation_kind r1, relation_kind r2)
{
  vrel_range_assert (r1);
  vrel_range_assert (r2);
  return rr_intersect_table[r1 - VREL_FIRST][r2 - VREL_FIRST];
}


// This table is used to perform a union between 2 relations.

relation_kind rr_union_table[VREL_COUNT][VREL_COUNT] = {
//    	 NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, EMPTY, EQ_EXPR, NE_EXPR
// VREL_NONE
  { VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE, VREL_NONE },
// LT_EXPR
  { VREL_NONE, LT_EXPR, LE_EXPR, NE_EXPR, VREL_NONE, LT_EXPR, LE_EXPR, NE_EXPR },
// LE_EXPR
  { VREL_NONE, LE_EXPR, LE_EXPR, VREL_NONE, VREL_NONE, LE_EXPR, LE_EXPR, VREL_NONE },
// GT_EXPR
  { VREL_NONE, NE_EXPR, VREL_NONE, GT_EXPR, GE_EXPR, GT_EXPR, GE_EXPR, NE_EXPR },
// GE_EXPR
  { VREL_NONE, VREL_NONE, VREL_NONE, GE_EXPR, GE_EXPR, GE_EXPR, GE_EXPR, VREL_NONE },
// VREL_EMPTY
  { VREL_NONE, LT_EXPR, LE_EXPR, GT_EXPR, GE_EXPR, VREL_EMPTY, EQ_EXPR, NE_EXPR },
// EQ_EXPR
  { VREL_NONE, LE_EXPR, LE_EXPR, GE_EXPR, GE_EXPR, EQ_EXPR, EQ_EXPR, VREL_NONE },
// NE_EXPR
  { VREL_NONE, NE_EXPR, VREL_NONE, NE_EXPR, VREL_NONE, NE_EXPR, VREL_NONE, NE_EXPR } };

// Union THIS relation with relation P if possible.  Return TRUE if
// this relation changes at all.

relation_kind
relation_union (relation_kind r1, relation_kind r2)
{
  vrel_range_assert (r1);
  vrel_range_assert (r2);
  return rr_union_table[r1 - VREL_FIRST][r2 - VREL_FIRST];
}


// -------------------------------------------------------------------------

class equiv_chain
{
public:
  bitmap m_names;
  basic_block m_bb;
  class equiv_chain *m_next;
  void dump (FILE *f) const;
};

void
equiv_chain::dump (FILE *f) const
{
  bitmap_iterator bi;
  unsigned i;

  if (!m_names)
    return;
  fprintf (f, "Equivalence set : [");
  unsigned c = 0;
  EXECUTE_IF_SET_IN_BITMAP (m_names, 0, i, bi)
    {
      if (ssa_name (i))
	{
	  if (c++)
	    fprintf (f, ", ");
	  print_generic_expr (f, ssa_name (i), TDF_SLIM);
	}
    }
  fprintf (f, "]\n");
}

equiv_oracle::equiv_oracle ()
{
  bitmap_obstack_initialize (&m_bitmaps);
  m_equiv.create (0);
  m_equiv.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
  m_equiv_set = BITMAP_ALLOC (&m_bitmaps);
  obstack_init (&m_chain_obstack);
}

equiv_oracle::~equiv_oracle ()
{
  obstack_free (&m_chain_obstack, NULL);
  m_equiv.release ();
  bitmap_obstack_release (&m_bitmaps);
}

const_bitmap
equiv_oracle::equiv_set (tree ssa, basic_block bb) const
{
  equiv_chain *equiv = find_equiv_dom (ssa, bb);
  if (equiv)
    return equiv->m_names;

  return NULL;
}



// If SSA has an equivalence in block BB, find and return it.
// Otherwise return NULL.
equiv_chain *
equiv_oracle::find_equiv_block (unsigned ssa, int bb) const
{
  equiv_chain *ptr = NULL;
  if (bb >= (int)m_equiv.length ())
    return NULL;

  if (m_equiv[bb] && bitmap_bit_p (m_equiv[bb]->m_names, ssa))
    {
      for (ptr = m_equiv[bb]->m_next; ptr; ptr = ptr->m_next)
	if (bitmap_bit_p (ptr->m_names, ssa))
	  break;
    }
  return ptr;
}

// Starting at block BB, walk the dominator chain looking for the nearest
// equivalence set containing NAME.
equiv_chain *
equiv_oracle::find_equiv_dom (tree name, basic_block bb) const
{
  unsigned v = SSA_NAME_VERSION (name);
  // Short circuit looking for names which have no equivlanmeces.
  if (!bitmap_bit_p (m_equiv_set, v))
    return NULL;

  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      equiv_chain *ptr = find_equiv_block (v, bb->index);
      if (ptr)
	return ptr;
    }
  return NULL;
}


// Register equivalance between V1 and equivalence set EQUIV in block BB,
bitmap
equiv_oracle::register_equiv (basic_block bb, unsigned v, equiv_chain *equiv)
{
  bitmap_set_bit (m_equiv_set, v);

  // v1 has an equivalance but v2 does not.
  // If that equivalence is in this block, simply use it.
  if (equiv->m_bb == bb)
    {
      bitmap_set_bit (equiv->m_names, v);
      bitmap_set_bit (m_equiv[bb->index]->m_names, v);
      return NULL;
    }

  // Otherwise create an equivalence for this block which is a copy
  // of equiv_1, plus add v1.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  bitmap_copy (b, equiv->m_names);
  bitmap_set_bit (b, v);
  return b;
}

// Register equivalance between equivalency set equiv_1 and equiv_2 in block BB.
bitmap
equiv_oracle::register_equiv (basic_block bb, equiv_chain *equiv_1,
			      equiv_chain *equiv_2)
{
  if (equiv_1->m_bb == bb)
    {
      bitmap_ior_into  (equiv_1->m_names, equiv_2->m_names);
      // Its hard to delete from a single linked list, so
      // just clear the second one.
      if (equiv_2->m_bb == bb)
	bitmap_clear (equiv_2->m_names);
      else
	bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_2->m_names);
      return NULL;
    }

  if (equiv_2->m_bb == bb)
    {
      bitmap_ior_into (equiv_2->m_names, equiv_1->m_names);
      bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_1->m_names);
      return NULL;
    }

  // At this point, neither equivalence is from this block.
  bitmap b = BITMAP_ALLOC (&m_bitmaps);
  bitmap_copy (b, equiv_1->m_names);
  bitmap_ior_into (b, equiv_2->m_names);
  return b;
}


// Register an equivalence between SSA1 and SSA2 in block BB.
void
equiv_oracle::register_equiv (basic_block bb, tree ssa1, tree ssa2)
{
  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);
  equiv_chain *equiv_1 = find_equiv_dom (ssa1, bb);
  equiv_chain *equiv_2 = find_equiv_dom (ssa2, bb);

  // Check if they are the same set
  if (equiv_1 && equiv_1 == equiv_2)
    return;

  bitmap equiv_set;

  if (!equiv_1 && !equiv_2)
    {
      bitmap_set_bit (m_equiv_set, v1);
      bitmap_set_bit (m_equiv_set, v2);

      equiv_set = BITMAP_ALLOC (&m_bitmaps);
      bitmap_set_bit (equiv_set, v1);
      bitmap_set_bit (equiv_set, v2);
    }
  else if (!equiv_1 && equiv_2)
    equiv_set = register_equiv (bb, v1, equiv_2);
  else if (equiv_1 && !equiv_2)
    equiv_set = register_equiv (bb, v2, equiv_1);
  else
    equiv_set = register_equiv (bb, equiv_1, equiv_2);

  // A non-null return is a bitmap that is to be added to the current
  // block as a new equivalence.
  if (!equiv_set)
    return;

  equiv_chain *ptr;

  // Check if this is the first time a block has an equivalence added.
  // and create a header block.
  if (!m_equiv[bb->index])
    {
      ptr = (equiv_chain *) obstack_alloc (&m_chain_obstack,
					   sizeof (equiv_chain));
      ptr->m_names = BITMAP_ALLOC (&m_bitmaps);
      bitmap_copy (ptr->m_names, equiv_set);
      ptr->m_bb = bb;
      ptr->m_next = NULL;
      m_equiv[bb->index] = ptr;
    }

  ptr = (equiv_chain *) obstack_alloc (&m_chain_obstack, sizeof (equiv_chain));
  ptr->m_names = equiv_set;
  ptr->m_bb = bb;
  gcc_checking_assert (bb->index < (int)m_equiv.length ());
  ptr->m_next = m_equiv[bb->index]->m_next;
  m_equiv[bb->index]->m_next = ptr;
  bitmap_ior_into (m_equiv[bb->index]->m_names, equiv_set);
}

void
equiv_oracle::limit_check (basic_block bb)
{
  int i = (bb) ? bb->index : last_basic_block_for_fn (cfun);
  if (i >= (int)m_equiv.length ())
    m_equiv.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
}

void
equiv_oracle::dump (FILE *f, basic_block bb) const
{
  if (bb->index >= (int)m_equiv.length ())
    return;
  if (!m_equiv[bb->index])
    return;

  equiv_chain *ptr = m_equiv[bb->index]->m_next;
  for (; ptr; ptr = ptr->m_next)
    ptr->dump (f);
}

void
equiv_oracle::dump (FILE *f) const
{
  fprintf (f, "Equivalency dump\n");
  for (unsigned i = 0; i < m_equiv.length (); i++)
    if (m_equiv[i])
      {
	fprintf (f, "BB%d\n", i);
	dump (f, BASIC_BLOCK_FOR_FN (cfun, i));
      }
}


// --------------------------------------------------------------------------

class value_relation
{
public:
  value_relation ();
  value_relation (relation_kind kind, tree n1, tree n2);
  void set_relation (relation_kind kind, tree n1, tree n2);

  inline relation_kind kind () const { return related; }
  inline tree op1 () const { return name1; }
  inline tree op2 () const { return name2; }

  bool union_ (value_relation &p);
  bool intersect (value_relation &p);
  void negate ();
  void swap ();

  void dump (FILE *f) const;
protected:
private:
  relation_kind related;
  tree name1, name2;
};

inline void
value_relation::set_relation (relation_kind r, tree n1, tree n2)
{
  gcc_checking_assert (SSA_NAME_VERSION (n1) != SSA_NAME_VERSION (n2));
  related = r;
  name1 = n1;
  name2 = n2;
}

inline
value_relation::value_relation ()
{
  related = VREL_NONE;
  name1 = NULL_TREE;
  name2 = NULL_TREE;
}

inline
value_relation::value_relation (relation_kind kind, tree n1, tree n2)
{
  set_relation (kind, n1, n2);
}

void
value_relation::negate ()
{
  related = relation_negate (related);
}

void
value_relation::swap ()
{
  related = relation_swap (related);
}

bool
value_relation::intersect (value_relation &p)
{
  // Save previous value
  relation_kind old = related;

  if (p.op1 () == op1 () && p.op2 () == op2 ())
    related = relation_intersect (kind (), p.kind ());
  else if (p.op2 () == op1 () && p.op1 () == op2 ())
    related = relation_intersect (kind (), relation_swap (p.kind ()));
  else
    return false;

  return old != related;
}

bool
value_relation::union_ (value_relation &p)
{
  // Save previous value
  relation_kind old = related;

  if (p.op1 () == op1 () && p.op2 () == op2 ())
    related = relation_union (kind(), p.kind());
  else if (p.op2 () == op1 () && p.op1 () == op2 ())
    related = relation_union (kind(), relation_swap (p.kind ()));
  else
    return false;

  return old != related;
}


// Dump the relation to file F.
void
value_relation::dump (FILE *f) const
{
  if (!name1 || !name2)
    {
      fprintf (f, "uninitialized");
      return;
    }
  fputc ('(', f);
  print_generic_expr (f, op1 (), TDF_SLIM);
  print_relation (f, kind ());
  print_generic_expr (f, op2 (), TDF_SLIM);
  fputc(')', f);
}

class relation_chain : public value_relation
{
public:
  relation_chain *m_next;
};

// ------------------------------------------------------------------------

relation_oracle::relation_oracle ()
{
  m_relations.create (0);
  m_relations.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);
  m_relation_set = BITMAP_ALLOC (&m_bitmaps);
  m_tmp = BITMAP_ALLOC (&m_bitmaps);
}

relation_oracle::~relation_oracle ()
{
  m_relations.release ();
}


void
relation_oracle::register_relation (gimple *stmt, relation_kind k, tree op1,
				    tree op2)
{
  gcc_checking_assert (TREE_CODE (op1) == SSA_NAME);
  gcc_checking_assert (TREE_CODE (op2) == SSA_NAME);
  gcc_checking_assert (stmt && gimple_bb (stmt));

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, op1, op2);
      fprintf (dump_file, " Registering value_relation ");
      vr.dump (dump_file);
      fprintf (dump_file, " (bb%d) at ", gimple_bb (stmt)->index);
      print_gimple_stmt (dump_file, stmt, 0, TDF_SLIM);
    }

  if (k == EQ_EXPR)
    register_equiv (gimple_bb (stmt), op1, op2);
  else
    register_relation (gimple_bb (stmt), k, op1, op2);
}

void
relation_oracle::register_relation (edge e, relation_kind k, tree op1,
				    tree op2)
{
  gcc_checking_assert (TREE_CODE (op1) == SSA_NAME);
  gcc_checking_assert (TREE_CODE (op2) == SSA_NAME);

  if (k == VREL_NONE || !single_pred_p (e->dest))
    return;

  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      value_relation vr (k, op1, op2);
      fprintf (dump_file, " Registering value_relation ");
      vr.dump (dump_file);
      fprintf (dump_file, " on (%d->%d)\n", e->src->index, e->dest->index);
    }

  if (k == EQ_EXPR)
    register_equiv (e->dest, op1, op2);
  else
    register_relation (e->dest, k, op1, op2);
}

void
relation_oracle::register_relation (basic_block bb, relation_kind k, tree op1,
				    tree op2)
{
  value_relation vr(k, op1, op2);
  int bbi = bb->index;

  if (bbi >= (int)m_relations.length())
    m_relations.safe_grow_cleared (last_basic_block_for_fn (cfun) + 1);

  bitmap bm = m_relations[bbi].m_names;
  if (!bm)
    bm = m_relations[bbi].m_names = BITMAP_ALLOC (&m_bitmaps);
  unsigned v1 = SSA_NAME_VERSION (op1);
  unsigned v2 = SSA_NAME_VERSION (op2);

  relation_kind curr;
  relation_chain *ptr;
  curr = find_relation_block (bbi, v1, v2, &ptr);
  // There is an existing relation in this block, just intersect with it.
  if (curr != VREL_NONE)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "    Intersecting with existing ");
	  ptr->dump (dump_file);
	}
      ptr->intersect (vr);
//    Unreachable blocks can cause an empty relation to be registered.
//    gcc_checking_assert (ptr->kind () != VREL_EMPTY);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, " to produce ");
	  ptr->dump (dump_file);
	  fprintf (dump_file, "\n");
	}
      return;
    }

  // See if there is an existing relation anywhere
  curr = find_relation_dom (bb, v1, v2);
  if (curr != VREL_NONE)
    k = relation_intersect (curr, k);

  bitmap_set_bit (bm, v1);
  bitmap_set_bit (bm, v2);
  bitmap_set_bit (m_relation_set, v1);
  bitmap_set_bit (m_relation_set, v2);

  ptr = (relation_chain *) obstack_alloc (&m_chain_obstack,
					  sizeof (relation_chain));
  ptr->set_relation (k, op1, op2);
  ptr->m_next = m_relations[bbi].m_head;
  m_relations[bbi].m_head = ptr;;
}


relation_kind
relation_oracle::find_relation_block (unsigned bb, const_bitmap b1,
				      const_bitmap b2)
{
  const_bitmap bm;
  if (bb >= m_relations.length())
    return VREL_NONE;

  bm = m_relations[bb].m_names;
  if (!bm)
    return VREL_NONE;

  // If both b1 and b2 aren't referenced in thie block, cant be a relation
  if (!bitmap_intersect_p (bm, b1) || !bitmap_intersect_p (bm, b2))
    return VREL_NONE;

  relation_chain *ptr;
  for (ptr = m_relations[bb].m_head; ptr ; ptr = ptr->m_next)
    {
      unsigned op1 = SSA_NAME_VERSION (ptr->op1 ());
      unsigned op2 = SSA_NAME_VERSION (ptr->op2 ());
      if (bitmap_bit_p (b1, op1) && bitmap_bit_p (b1, op2))
	return ptr->kind ();
      if (bitmap_bit_p (b1, op2) && bitmap_bit_p (b1, op1))
	return relation_swap (ptr->kind ());
    }

  return VREL_NONE;
}

relation_kind
relation_oracle::find_relation_dom (basic_block bb, const_bitmap b1,
				    const_bitmap b2)
{
  relation_kind r;
  // IF either name does not occur in a relation anywhere, there isnt one.
  if (!bitmap_intersect_p (m_relation_set, b1)
      || !bitmap_intersect_p (m_relation_set, b2))
    return VREL_NONE;

  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      r = find_relation_block (bb->index, b1, b2);
      if (r != VREL_NONE)
	return r;
    }
  return VREL_NONE;

}

relation_kind
relation_oracle::find_relation_block (int bb, unsigned v1, unsigned v2,
				     relation_chain **obj)
{
  if (bb >= (int)m_relations.length())
    return VREL_NONE;

  const_bitmap bm = m_relations[bb].m_names;
  if (!bm)
    return VREL_NONE;

  // If both b1 and b2 aren't referenced in thie block, cant be a relation
  if (!bitmap_bit_p (bm, v1) || !bitmap_bit_p (bm, v2))
    return VREL_NONE;

  relation_chain *ptr;
  for (ptr = m_relations[bb].m_head; ptr ; ptr = ptr->m_next)
    {
      unsigned op1 = SSA_NAME_VERSION (ptr->op1 ());
      unsigned op2 = SSA_NAME_VERSION (ptr->op2 ());
      if (v1 == op1 && v2 == op2)
	{
	  if (obj)
	    *obj = ptr;
	  return ptr->kind ();
	}
      if (v1 == op2 && v2 == op1)
	{
	  if (obj)
	    *obj = ptr;
	  return relation_swap (ptr->kind ());
	}
    }

  return VREL_NONE;
}

relation_kind
relation_oracle::find_relation_dom (basic_block bb, unsigned v1, unsigned v2)
{
  relation_kind r;
  // IF either name does not occur in a relation anywhere, there isnt one.
  if (!bitmap_bit_p (m_relation_set, v1) || !bitmap_bit_p (m_relation_set, v2))
    return VREL_NONE;

  for ( ; bb; bb = get_immediate_dominator (CDI_DOMINATORS, bb))
    {
      r = find_relation_block (bb->index, v1, v2);
      if (r != VREL_NONE)
	return r;
    }
  return VREL_NONE;

}



relation_kind
relation_oracle::query_relation (basic_block bb, tree ssa1, tree ssa2)
{
  relation_kind kind;
  unsigned v1 = SSA_NAME_VERSION (ssa1);
  unsigned v2 = SSA_NAME_VERSION (ssa2);
  if (v1 == v2)
    return EQ_EXPR;

  const_bitmap equiv1 = equiv_set (ssa1, bb);
  if (equiv1 && bitmap_bit_p (equiv1, v2))
    return EQ_EXPR;

  // Initially look for a direct relationship and just return that.
  kind = find_relation_dom (bb, v1, v2);
  if (kind != VREL_NONE)
    return kind;

  // If one is not found, see if there is a relationship between equivalences.

  const_bitmap equiv2 = equiv_set (ssa2, bb);
  // If v2 isn't in v1s equiv set, then v1 shouldn't be in v2's set either.
  gcc_checking_assert (!equiv2 || !bitmap_bit_p (equiv2, v1));

  if (!equiv1 && !equiv2)
    kind = VREL_NONE;
  else if (!equiv1)
    {
      bitmap_clear (m_tmp);
      bitmap_set_bit (m_tmp, v1);
      kind = find_relation_dom (bb, m_tmp, equiv2);
    }
  else if (!equiv2)
    {
      bitmap_clear (m_tmp);
      bitmap_set_bit (m_tmp, v2);
      kind = find_relation_dom (bb, equiv1, m_tmp);
    }
  else
    kind = find_relation_dom (bb, equiv1, equiv2);
  return kind;
}

void
relation_oracle::dump (FILE *f, basic_block bb) const
{
  equiv_oracle::dump (f,bb);

  if (bb->index >= (int)m_relations.length ())
    return;
  if (!m_relations[bb->index].m_names)
    return;

  relation_chain *ptr = m_relations[bb->index].m_head;
  for (; ptr; ptr = ptr->m_next)
    {
      fprintf (f, "Relational : ");
      ptr->dump (f);
      fprintf (f, "\n");
    }
}


void
relation_oracle::dump (FILE *f) const
{
  equiv_oracle::dump (f);
}


