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


#ifndef GCC_VALUE_RELATION_H
#define GCC_VALUE_RELATION_H


// Rather than introduce a new enumerated type for relations, we can use the
// existing tree_codes for relations, plus add a couple of #defines for 
// the other cases.  These codes are arranged such that VREL_NONE is the first
// code, and all the rest are contiguous.

typedef enum tree_code relation_kind;

#define VREL_NONE		TRUTH_NOT_EXPR
#define VREL_EMPTY		LTGT_EXPR

relation_kind relation_union (relation_kind r1, relation_kind r2);
relation_kind relation_intersect (relation_kind r1, relation_kind r2);
relation_kind relation_negate (relation_kind r);
relation_kind relation_swap (relation_kind r);
void print_relation (FILE *f, relation_kind rel);

class equiv_chain;

class equiv_oracle
{
public:
  equiv_oracle ();
  ~equiv_oracle ();

  const_bitmap equiv_set (tree ssa, basic_block bb) const;
  void register_equiv (basic_block bb, tree ssa1, tree ssa2);

  void dump (FILE *f, basic_block bb) const;
  void dump (FILE *f) const;

protected:
  bitmap_obstack m_bitmaps;
  struct obstack m_chain_obstack;
private:
  bitmap m_equiv_set;	// Index by ssa-name. true if an equivalence exists
  vec <equiv_chain *> m_equiv;	// Index by BB.  list of equivalences.

  void limit_check (basic_block bb = NULL);
  equiv_chain *find_equiv_block (unsigned ssa, int bb) const;
  equiv_chain *find_equiv_dom (tree name, basic_block bb) const;

  bitmap register_equiv (basic_block bb, unsigned v, equiv_chain *equiv_1);
  bitmap register_equiv (basic_block bb, equiv_chain *equiv_1,
			 equiv_chain *equiv_2);

};


class relation_chain_head
{
public:
  bitmap m_names;
  class relation_chain *m_head;
};

class relation_oracle : public equiv_oracle
{
public:
  relation_oracle ();
  ~relation_oracle ();

  void register_relation (gimple *stmt, relation_kind k, tree op1, tree op2);
  void register_relation (edge e, relation_kind k, tree op1, tree op2);

  relation_kind query_relation (basic_block bb, tree ssa1, tree ssa2);

  void dump (FILE *f, basic_block bb) const;
  void dump (FILE *f) const;
private:
  bitmap m_tmp;
  bitmap m_relation_set;  // Index by ssa-name. True if a relation exists
  vec <relation_chain_head> m_relations;  // Index by BB, list of relations.
  relation_kind find_relation_block (unsigned bb, const_bitmap b1,
				     const_bitmap b2);
  relation_kind find_relation_dom (basic_block bb, const_bitmap b1,
				   const_bitmap b2);
  relation_kind find_relation_block (int bb, unsigned v1, unsigned v2,
				     relation_chain **obj = NULL);
  relation_kind find_relation_dom (basic_block bb, unsigned v1, unsigned v2);
  void register_relation (basic_block bb, relation_kind k, tree op1, tree op2);

};

#endif  /* GCC_VALUE_RELATION_H */
