/* Operations with affine combinations of trees.
   Copyright (C) 2005-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 3, or (at your option) any
later version.

GCC is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "gimple.h"
#include "ssa.h"
#include "tree-pretty-print.h"
#include "fold-const.h"
#include "tree-affine.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "cfgexpand.h"

/* Extends CST as appropriate for the affine combinations COMB.  */

static widest_int
wide_int_ext_for_comb (const widest_int &cst, tree type)
{
  return wi::sext (cst, TYPE_NONCAP_PRECISION (type));
}

/* Likewise for polynomial offsets.  */

static poly_widest_int
wide_int_ext_for_comb (const poly_widest_int &cst, tree type)
{
  return wi::sext (cst, TYPE_NONCAP_PRECISION (type));
}

/* Initializes affine combination COMB so that its value is zero in TYPE.  */

static void
aff_combination_zero (aff_tree *comb, tree type)
{
  int i;
  comb->type = type;
  comb->offset = 0;
  comb->n = 0;
  for (i = 0; i < MAX_AFF_ELTS; i++)
    comb->elts[i].coef = 0;
  comb->rest = NULL_TREE;
}

/* Sets COMB to CST.  */

void
aff_combination_const (aff_tree *comb, tree type, const poly_widest_int &cst)
{
  aff_combination_zero (comb, type);
  comb->offset = wide_int_ext_for_comb (cst, comb->type);;
}

/* Sets COMB to single element ELT.  */

void
aff_combination_elt (aff_tree *comb, tree type, tree elt)
{
  aff_combination_zero (comb, type);

  comb->n = 1;
  gcc_assert (capability_type_p (type) || !tree_is_capability_value (elt));
  comb->elts[0].val = elt;
  comb->elts[0].coef = 1;
}

/* Scales COMB by SCALE.  */

void
aff_combination_scale (aff_tree *comb, const widest_int &scale_in)
{
  unsigned i, j;

  widest_int scale = wide_int_ext_for_comb (scale_in, comb->type);
  if (scale == 1)
    return;

  if (scale == 0)
    {
      aff_combination_zero (comb, comb->type);
      return;
    }

  comb->offset = wide_int_ext_for_comb (scale * comb->offset, comb->type);
  for (i = 0, j = 0; i < comb->n; i++)
    {
      widest_int new_coef
	= wide_int_ext_for_comb (scale * comb->elts[i].coef, comb->type);
      /* A coefficient may become zero due to overflow.  Remove the zero
	 elements.  */
      if (new_coef == 0)
	continue;
      comb->elts[j].coef = new_coef;
      comb->elts[j].val = comb->elts[i].val;
      j++;
    }
  comb->n = j;

  if (comb->rest)
    {
      tree type = comb->type;
      if (POINTER_TYPE_P (type))
	type = sizetype;
      if (comb->n < MAX_AFF_ELTS)
	{
	  comb->elts[comb->n].coef = scale;
	  comb->elts[comb->n].val = comb->rest;
	  comb->rest = NULL_TREE;
	  comb->n++;
	}
      else
	comb->rest = fold_build2 (MULT_EXPR, type, comb->rest,
				  wide_int_to_tree (type, scale));
    }
}

/* Adds ELT * SCALE to COMB.  */

void
aff_combination_add_elt (aff_tree *comb, tree elt, const widest_int &scale_in)
{
  unsigned i;
  tree type;

  widest_int scale = wide_int_ext_for_comb (scale_in, comb->type);
  if (scale == 0)
    return;

  for (i = 0; i < comb->n; i++)
    if (operand_equal_p (comb->elts[i].val, elt, 0))
      {
	widest_int new_coef
	  = wide_int_ext_for_comb (comb->elts[i].coef + scale, comb->type);
	if (new_coef != 0)
	  {
	    comb->elts[i].coef = new_coef;
	    return;
	  }

	comb->n--;
	comb->elts[i] = comb->elts[comb->n];

	if (comb->rest)
	  {
	    gcc_assert (comb->n == MAX_AFF_ELTS - 1);
	    comb->elts[comb->n].coef = 1;
	    comb->elts[comb->n].val = comb->rest;
	    comb->rest = NULL_TREE;
	    comb->n++;
	  }
	return;
      }

  if (CHECKING_P && !capability_type_p (comb->type))
    {
      gcc_assert (!tree_is_capability_value (elt));
      for (i = 0; i < comb->n; i++)
	gcc_assert (!tree_is_capability_value (comb->elts[i].val));
    }
  else if (CHECKING_P)
    {
      for (i = 1; i < comb->n; i++)
	gcc_assert (!tree_is_capability_value (comb->elts[i].val));
    }

  /* We really don't want to be having an affine tree with multiple
     capabilities to derive provenance from.
     The fact that all capabilities have a single provenance in the
     original code should mean that we never end up attempting to add
     two capabilities into a single affine tree.
     Whenever an affine combination has a capability element we want it stored
     in the first `elt`.  */
  if (capability_type_p (comb->type) && tree_is_capability_value (elt))
    {
      gcc_assert (!aff_cap_provenance_p (comb));
      class aff_comb_elt temp = comb->elts[0];
      comb->elts[0].val = elt;
      comb->elts[0].coef = scale;
      if (comb->n == 0)
	{
	  comb->n++;
	  return;
	}
      elt = temp.val;
      scale = temp.coef;
    }

  if (comb->n < MAX_AFF_ELTS)
    {
      comb->elts[comb->n].coef = scale;
      comb->elts[comb->n].val = elt;
      comb->n++;
      return;
    }

  type = comb->type;
  if (POINTER_TYPE_P (type))
    type = sizetype;
  else if (INTCAP_TYPE_P (type))
    type = noncapability_type (type);

  if (scale == 1)
    elt = fold_convert (type, elt);
  else
    elt = fold_build2 (MULT_EXPR, type,
		       fold_convert (type, elt),
		       wide_int_to_tree (type, scale));

  if (comb->rest)
    comb->rest = fold_build2 (PLUS_EXPR, type, comb->rest,
			      elt);
  else
    comb->rest = elt;
}

/* Adds CST to C.  */

static void
aff_combination_add_cst (aff_tree *c, const poly_widest_int &cst)
{
  c->offset = wide_int_ext_for_comb (c->offset + cst, c->type);
}

/* Adds COMB2 to COMB1.  */

void
aff_combination_add (aff_tree *comb1, aff_tree *comb2)
{
  unsigned i;

  aff_combination_add_cst (comb1, comb2->offset);
  for (i = 0; i < comb2->n; i++)
    aff_combination_add_elt (comb1, comb2->elts[i].val, comb2->elts[i].coef);
  if (comb2->rest)
    aff_combination_add_elt (comb1, comb2->rest, 1);
}

/* Drops the provenance-providing capability from an affine tree.  */
void
aff_combination_drop_capability (aff_tree *comb1)
{
  if (!capability_type_p (comb1->type))
    return;
  comb1->type = noncapability_type (comb1->type);
  if (aff_cap_provenance_p (comb1))
    comb1->elts[0].val = fold_drop_capability (comb1->elts[0].val);
}

/* Some affine trees have a logical capability provenance, while others do not.
   This function extracts the capability from which the provided affine tree
   takes its provenance and returns it.  If there is no capability in this
   affine tree return NULL_TREE.  */
tree
aff_combination_extract_provenance_cap (aff_tree *comb)
{
  if (!aff_cap_provenance_p (comb))
    return NULL_TREE;

  tree ret = comb->elts[0].val;
  if (POINTER_TYPE_P (comb->type) && INTCAP_TYPE_P (TREE_TYPE (ret)))
    ret = fold_convert (comb->type, ret);

  if (comb->elts[0].coef == 1)
    aff_combination_remove_elt (comb, 0);
  else
    comb->elts[0].coef -= 1;
  aff_combination_drop_capability (comb);
  return ret;
}

/* Converts affine combination COMB to TYPE.  */

void
aff_combination_convert (aff_tree *comb, tree type)
{
  unsigned i, j;
  tree comb_type = comb->type;

  /* Should not be converting from non-capability type to capability.  */
  gcc_assert (!capability_type_p (type) || capability_type_p (comb_type));

  if  (TYPE_NONCAP_PRECISION (type) > TYPE_NONCAP_PRECISION (comb_type))
    {
      tree val = fold_convert (type, aff_combination_to_tree (comb));
      tree_to_aff_combination (val, type, comb);
      return;
    }

  comb->type = type;
  if (comb->rest && !POINTER_TYPE_P (type) && !capability_type_p (type))
    comb->rest = fold_convert (type, comb->rest);

  if (TYPE_NONCAP_PRECISION (type) == TYPE_NONCAP_PRECISION (comb_type)
      && capability_type_p (type) == capability_type_p (comb_type))
    return;

  comb->offset = wide_int_ext_for_comb (comb->offset, comb->type);
  for (i = j = 0; i < comb->n; i++)
    {
      if (comb->elts[i].coef == 0)
	continue;
      comb->elts[j].coef = comb->elts[i].coef;
      comb->elts[j].val = fold_convert (type, comb->elts[i].val);
      j++;
    }

  comb->n = j;
  if (comb->n < MAX_AFF_ELTS && comb->rest)
    {
      comb->elts[comb->n].coef = 1;
      comb->elts[comb->n].val = comb->rest;
      comb->rest = NULL_TREE;
      comb->n++;
    }
}

/* Tries to handle OP0 CODE OP1 as affine combination of parts.  Returns
   true when that was successful and returns the combination in COMB.  */

static bool
expr_to_aff_combination (aff_tree *comb, tree_code code, tree type,
			 tree op0, tree op1 = NULL_TREE)
{
  aff_tree tmp;
  poly_int64 bitpos, bitsize, bytepos;

  switch (code)
    {
    case POINTER_PLUS_EXPR:
      tree_to_aff_combination (op0, type, comb);
      tree_to_aff_combination (op1, sizetype, &tmp);
      aff_combination_add (comb, &tmp);
      return true;

    case PLUS_EXPR:
    case MINUS_EXPR:
      tree_to_aff_combination (op0, type, comb);
      tree_to_aff_combination (op1, type, &tmp);
      if (code == MINUS_EXPR)
	aff_combination_scale (&tmp, -1);
      aff_combination_add (comb, &tmp);
      return true;

    case MULT_EXPR:
      if (TREE_CODE (op1) != INTEGER_CST)
	break;
      tree_to_aff_combination (op0, type, comb);
      aff_combination_scale (comb, wi::to_widest (op1));
      return true;

    case NEGATE_EXPR:
      tree_to_aff_combination (op0, type, comb);
      aff_combination_scale (comb, -1);
      return true;

    case BIT_NOT_EXPR:
      /* ~x = -x - 1 */
      tree_to_aff_combination (op0, type, comb);
      aff_combination_scale (comb, -1);
      aff_combination_add_cst (comb, -1);
      return true;

    CASE_CONVERT:
      {
	tree otype = type;
	tree inner = op0;
	tree itype = TREE_TYPE (inner);
	enum tree_code icode = TREE_CODE (inner);

	/* STRIP_NOPS  */
	if (tree_nop_conversion_p (otype, itype))
	  {
	    tree_to_aff_combination (op0, type, comb);
	    return true;
	  }

	/* In principle this is a valid folding, but it isn't necessarily
	   an optimization, so do it here and not in fold_unary.  */
	if ((icode == PLUS_EXPR || icode == MINUS_EXPR || icode == MULT_EXPR)
	    && TREE_CODE (itype) == INTEGER_TYPE
	    && TREE_CODE (otype) == INTEGER_TYPE
	    && TYPE_PRECISION (otype) > TYPE_PRECISION (itype))
	  {
	    tree op0 = TREE_OPERAND (inner, 0), op1 = TREE_OPERAND (inner, 1);

	    /* If inner type has undefined overflow behavior, fold conversion
	       for below two cases:
		 (T1)(X *+- CST) -> (T1)X *+- (T1)CST
		 (T1)(X + X)     -> (T1)X + (T1)X.  */
	    if (TYPE_OVERFLOW_UNDEFINED (itype)
		&& (TREE_CODE (op1) == INTEGER_CST
		    || (icode == PLUS_EXPR && operand_equal_p (op0, op1, 0))))
	      {
		op0 = fold_convert (otype, op0);
		op1 = fold_convert (otype, op1);
		return expr_to_aff_combination (comb, icode, otype, op0, op1);
	      }
	    wide_int minv, maxv;
	    /* If inner type has wrapping overflow behavior, fold conversion
	       for below case:
		 (T1)(X *+- CST) -> (T1)X *+- (T1)CST
	       if X *+- CST doesn't overflow by range information.  */
	    if (TYPE_UNSIGNED (itype)
		&& TYPE_OVERFLOW_WRAPS (itype)
		&& TREE_CODE (op1) == INTEGER_CST
		&& determine_value_range (op0, &minv, &maxv) == VR_RANGE)
	      {
		wi::overflow_type overflow = wi::OVF_NONE;
		signop sign = UNSIGNED;
		if (icode == PLUS_EXPR)
		  wi::add (maxv, wi::to_wide (op1), sign, &overflow);
		else if (icode == MULT_EXPR)
		  wi::mul (maxv, wi::to_wide (op1), sign, &overflow);
		else
		  wi::sub (minv, wi::to_wide (op1), sign, &overflow);

		if (overflow == wi::OVF_NONE)
		  {
		    op0 = fold_convert (otype, op0);
		    op1 = fold_convert (otype, op1);
		    return expr_to_aff_combination (comb, icode, otype, op0,
						    op1);
		  }
	      }
	  }
      }
      break;

    default:;
    }

  return false;
}

/* Splits EXPR into an affine combination of parts.  */

void
tree_to_aff_combination (tree expr, tree type, aff_tree *comb)
{
  aff_tree tmp;
  enum tree_code code;
  tree core, toffset;
  poly_int64 bitpos, bitsize, bytepos;
  machine_mode mode;
  int unsignedp, reversep, volatilep;

  STRIP_NOPS (expr);

  code = TREE_CODE (expr);
  switch (code)
    {
    case POINTER_PLUS_EXPR:
    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
      if (expr_to_aff_combination (comb, code, type, TREE_OPERAND (expr, 0),
				   TREE_OPERAND (expr, 1)))
	return;
      break;

    case NEGATE_EXPR:
    case BIT_NOT_EXPR:
      if (expr_to_aff_combination (comb, code, type, TREE_OPERAND (expr, 0)))
	return;
      break;

    CASE_CONVERT:
      /* ???  TREE_TYPE (expr) should be equal to type here, but IVOPTS
	 calls this with not showing an outer widening cast.  */
      if (expr_to_aff_combination (comb, code,
				   TREE_TYPE (expr), TREE_OPERAND (expr, 0)))
	{
	  aff_combination_convert (comb, type);
	  return;
	}
      break;

    CASE_ADDR_EXPR:
      /* Handle &MEM[ptr + CST] which is equivalent to POINTER_PLUS_EXPR.  */
      if (TREE_CODE (TREE_OPERAND (expr, 0)) == MEM_REF)
	{
	  expr = TREE_OPERAND (expr, 0);
	  tree_to_aff_combination (TREE_OPERAND (expr, 0), type, comb);
	  tree_to_aff_combination (TREE_OPERAND (expr, 1), sizetype, &tmp);
	  aff_combination_add (comb, &tmp);
	  return;
	}
      core = get_inner_reference (TREE_OPERAND (expr, 0), &bitsize, &bitpos,
				  &toffset, &mode, &unsignedp, &reversep,
				  &volatilep);
      if (!multiple_p (bitpos, BITS_PER_UNIT, &bytepos))
	break;
      aff_combination_const (comb, type, bytepos);
      if (TREE_CODE (core) == MEM_REF)
	{
	  tree mem_offset = TREE_OPERAND (core, 1);
	  aff_combination_add_cst (comb, wi::to_poly_widest (mem_offset));
	  core = TREE_OPERAND (core, 0);
	}
      else
	core = build_fold_addr_expr (code, core);

      if (ADDR_EXPR_P (core))
	{
	  if (!capability_type_p (type))
	    core = fold_drop_capability (core);
	  aff_combination_add_elt (comb, core, 1);
	}
      else
	{
	  tree_to_aff_combination (core, type, &tmp);
	  aff_combination_add (comb, &tmp);
	}
      if (toffset)
	{
	  tree_to_aff_combination (toffset, noncapability_type (type), &tmp);
	  aff_combination_add (comb, &tmp);
	}
      return;

    default:
      {
	/* Whenever we know where the capability metadata comes from, we want
	   that element included as the first element in our affine tree.  */
	if (code == INTEGER_CST && capability_type_p (type))
	  return aff_combination_elt (comb, type, expr);
	if (poly_int_tree_p (expr))
	  {
	    aff_combination_const (comb, type, wi::to_poly_widest (expr));
	    return;
	  }
	break;
      }
    }

  if (!capability_type_p (type) && tree_is_capability_value (expr))
    expr = fold_drop_capability (expr);
  aff_combination_elt (comb, type, expr);
}

/* Creates EXPR + ELT * SCALE in TYPE.  EXPR is taken from affine
   combination COMB.  */

static tree
add_elt_to_tree (tree expr, tree type, tree elt, const widest_int &scale_in)
{
  enum tree_code code;

  widest_int scale = wide_int_ext_for_comb (scale_in, type);

  elt = fold_convert (type, elt);
  if (scale == 1)
    {
      if (!expr)
	return elt;

      return fold_build2 (PLUS_EXPR, type, expr, elt);
    }

  if (scale == -1)
    {
      if (!expr)
	return fold_build1 (NEGATE_EXPR, type, elt);

      return fold_build2 (MINUS_EXPR, type, expr, elt);
    }

  if (!expr)
    return fold_build2 (MULT_EXPR, type, elt, wide_int_to_tree (type, scale));

  if (wi::neg_p (scale))
    {
      code = MINUS_EXPR;
      scale = -scale;
    }
  else
    code = PLUS_EXPR;

  elt = fold_build2 (MULT_EXPR, type, elt, wide_int_to_tree (type, scale));
  return fold_build2 (code, type, expr, elt);
}

/* Makes tree from the affine combination COMB.  */

tree
aff_combination_to_tree (aff_tree *comb)
{
  tree type = comb->type, base = NULL_TREE, expr = NULL_TREE;
  unsigned i;
  poly_widest_int off;
  int sgn;

  gcc_assert (comb->n == MAX_AFF_ELTS || comb->rest == NULL_TREE);

  i = 0;
  if (capability_type_p (type) || POINTER_TYPE_P (type))
    {
      tree off_type = POINTER_TYPE_P (type) ? sizetype
	: noncapability_type (type);
      if (comb->n > 0 && comb->elts[0].coef == 1
	  && POINTER_TYPE_P (TREE_TYPE (comb->elts[0].val)))
	{
	  base = comb->elts[0].val;
	  ++i;
	}
      else if (aff_cap_provenance_p (comb))
	{
	  gcc_assert (capability_type_p (type));
	  base = comb->elts[0].val;
	  if (comb->elts[0].coef != 1)
	    expr = add_elt_to_tree (expr, off_type,
				    fold_drop_capability (comb->elts[0].val),
				    comb->elts[0].coef-1);
	  ++i;
	}
      else if (capability_type_p (type))
	base = build_zero_cst (type);

      /* Options are:
	 - comb->elts[0].val is a capability and we want to its provenance.
	 - comb->elts[0].val is not a capability, affine tree is a capability,
	   want NULL provenance.
	 - affine tree is not a capability, is a pointer, comb->elts[0].val is
	   a pointer, want to use POINTER_PLUS_EXPR.
	 - affine tree is not a capability, is a pointer type, do not have
	   single pointer to use as base.  Want to use sizetype instead.  */
      type = off_type;
    }

  for (; i < comb->n; i++)
    expr = add_elt_to_tree (expr, type, comb->elts[i].val, comb->elts[i].coef);

  if (comb->rest)
    expr = add_elt_to_tree (expr, type, comb->rest, 1);

  /* Ensure that we get x - 1, not x + (-1) or x + 0xff..f if x is
     unsigned.  */
  if (known_lt (comb->offset, 0))
    {
      off = -comb->offset;
      sgn = -1;
    }
  else
    {
      off = comb->offset;
      sgn = 1;
    }
  expr = add_elt_to_tree (expr, type, wide_int_to_tree (type, off), sgn);

  if (base)
    {
      /* If the base is of pointer type and the combination was of pointer
	 type, then may as well generate our TREE expression in the same type
	 as the base (all pointers have the same range of defined behaviour).

	 If the base is not of pointer type then it must be of INTCAP_TYPE.  In
	 that case stay with the pointer type of the affine tree.  The pointer
	 type of the affine tree is more limited than the INTCAP type of the
	 base, but the code that we represent it with is something that the
	 rest of the compiler can recognise better.  The restrictions on
	 overflow are known to not be a problem because this type was what the
	 ivopts machinery and ivopts have found and used (and did not find any
	 reason to need to convert the pointer typed affine tree to an intcap).

	 If the affine tree type is an INTCAP_TYPE then we can't use a pointer
	 type since we can't assume the operations on it are valid for
	 pointers.  */
      if (POINTER_TYPE_P (TREE_TYPE (base)))
	return fold_build_pointer_plus (base, expr);
      else if (POINTER_TYPE_P (comb->type))
	{
	  gcc_assert (INTCAP_TYPE_P (TREE_TYPE (base)));
	  base = fold_convert (comb->type, base);
	  expr = fold_build_pointer_plus (base, expr);
	}
      else if (INTCAP_TYPE_P (TREE_TYPE (base)))
	{
	  /* Currently all affine trees happen to be made from induction
	     variables, and that means they never have side effects.  I believe
	     there is no reason that affine trees *must* only be made from
	     variables without side effects.  If that ever becomes the case
	     then we can not duplicate the evaluation of `base` without using
	     something like `SAVE_EXPR` to ensure we avoid duplicating the side
	     effects.  This assert should alert us when such a problematic case
	     arises.  */
	  gcc_assert (!TREE_SIDE_EFFECTS (base));
	  tree noncap_type = noncapability_type (comb->type);
	  tree base_value = fold_convert (noncap_type, base);
	  tree res_value = fold_build2 (PLUS_EXPR, noncap_type,
					base_value, expr);
	  expr = fold_build_replace_address_value (base, res_value);
	}
      else
	gcc_unreachable ();
    }

  return fold_convert (comb->type, expr);
}

/* Copies the tree elements of COMB to ensure that they are not shared.  */

void
unshare_aff_combination (aff_tree *comb)
{
  unsigned i;

  for (i = 0; i < comb->n; i++)
    comb->elts[i].val = unshare_expr (comb->elts[i].val);
  if (comb->rest)
    comb->rest = unshare_expr (comb->rest);
}

/* Remove M-th element from COMB.  */

void
aff_combination_remove_elt (aff_tree *comb, unsigned m)
{
  comb->n--;
  if (m <= comb->n)
    comb->elts[m] = comb->elts[comb->n];
  if (comb->rest)
    {
      comb->elts[comb->n].coef = 1;
      comb->elts[comb->n].val = comb->rest;
      comb->rest = NULL_TREE;
      comb->n++;
    }
}

/* Adds C * COEF * VAL to R.  VAL may be NULL, in that case only
   C * COEF is added to R.  */


static void
aff_combination_add_product (aff_tree *c, const widest_int &coef, tree val,
			     aff_tree *r)
{
  unsigned i;
  tree aval, type;

  for (i = 0; i < c->n; i++)
    {
      aval = c->elts[i].val;
      if (val)
	{
	  type = TREE_TYPE (aval);
	  aval = fold_build2 (MULT_EXPR, type, aval,
			      fold_convert (type, val));
	}

      aff_combination_add_elt (r, aval, coef * c->elts[i].coef);
    }

  if (c->rest)
    {
      aval = c->rest;
      if (val)
	{
	  type = TREE_TYPE (aval);
	  aval = fold_build2 (MULT_EXPR, type, aval,
			      fold_convert (type, val));
	}

      aff_combination_add_elt (r, aval, coef);
    }

  if (val)
    {
      if (c->offset.is_constant ())
	/* Access coeffs[0] directly, for efficiency.  */
	aff_combination_add_elt (r, val, coef * c->offset.coeffs[0]);
      else
	{
	  /* c->offset is polynomial, so multiply VAL rather than COEF
	     by it.  */
	  tree offset = wide_int_to_tree (TREE_TYPE (val), c->offset);
	  val = fold_build2 (MULT_EXPR, TREE_TYPE (val), val, offset);
	  aff_combination_add_elt (r, val, coef);
	}
    }
  else
    aff_combination_add_cst (r, coef * c->offset);
}

/* Multiplies C1 by C2, storing the result to R  */

void
aff_combination_mult (aff_tree *c1, aff_tree *c2, aff_tree *r)
{
  unsigned i;
  gcc_assert (TYPE_PRECISION (c1->type) == TYPE_PRECISION (c2->type));
  gcc_assert (!aff_cap_provenance_p (c1) || !aff_cap_provenance_p (c2));

  aff_combination_zero (r, c1->type);

  for (i = 0; i < c2->n; i++)
    aff_combination_add_product (c1, c2->elts[i].coef, c2->elts[i].val, r);
  if (c2->rest)
    aff_combination_add_product (c1, 1, c2->rest, r);
  if (c2->offset.is_constant ())
    /* Access coeffs[0] directly, for efficiency.  */
    aff_combination_add_product (c1, c2->offset.coeffs[0], NULL, r);
  else
    {
      /* c2->offset is polynomial, so do the multiplication in tree form.  */
      tree offset = wide_int_to_tree (c2->type, c2->offset);
      aff_combination_add_product (c1, 1, offset, r);
    }
}

/* Returns the element of COMB whose value is VAL, or NULL if no such
   element exists.  If IDX is not NULL, it is set to the index of VAL in
   COMB.  */

static class aff_comb_elt *
aff_combination_find_elt (aff_tree *comb, tree val, unsigned *idx)
{
  unsigned i;

  for (i = 0; i < comb->n; i++)
    if (operand_equal_p (comb->elts[i].val, val, 0))
      {
	if (idx)
	  *idx = i;

	return &comb->elts[i];
      }

  return NULL;
}

/* Element of the cache that maps ssa name NAME to its expanded form
   as an affine expression EXPANSION.  */

class name_expansion
{
public:
  aff_tree expansion;

  /* True if the expansion for the name is just being generated.  */
  unsigned in_progress : 1;
};

/* Expands SSA names in COMB recursively.  CACHE is used to cache the
   results.  */

void
aff_combination_expand (aff_tree *comb ATTRIBUTE_UNUSED,
			hash_map<tree, name_expansion *> **cache)
{
  unsigned i;
  aff_tree to_add, current, curre, to_subtract;
  tree e;
  gimple *def;
  widest_int scale;
  class name_expansion *exp;

  aff_combination_zero (&to_add, comb->type);
  aff_combination_zero (&to_subtract, comb->type);
  for (i = 0; i < comb->n; i++)
    {
      tree type, name;
      enum tree_code code;

      e = comb->elts[i].val;
      type = TREE_TYPE (e);
      name = e;
      /* Look through some conversions.  */
      if (CONVERT_EXPR_P (e)
	  && (capability_type_p (type)
	      == capability_type_p (TREE_TYPE (TREE_OPERAND (e, 0))))
	  && (TYPE_CAP_PRECISION (type)
	      >= TYPE_CAP_PRECISION (TREE_TYPE (TREE_OPERAND (e, 0)))))
	name = TREE_OPERAND (e, 0);
      if (TREE_CODE (name) != SSA_NAME)
	continue;
      def = SSA_NAME_DEF_STMT (name);
      if (!is_gimple_assign (def) || gimple_assign_lhs (def) != name)
	continue;

      code = gimple_assign_rhs_code (def);
      if (code != SSA_NAME
	  && !IS_EXPR_CODE_CLASS (TREE_CODE_CLASS (code))
	  && (get_gimple_rhs_class (code) != GIMPLE_SINGLE_RHS
	      || !is_gimple_min_invariant (gimple_assign_rhs1 (def))))
	continue;

      /* We do not know whether the reference retains its value at the
	 place where the expansion is used.  */
      if (TREE_CODE_CLASS (code) == tcc_reference)
	continue;

      name_expansion **slot = NULL;
      if (*cache)
	slot = (*cache)->get (name);
      exp = slot ? *slot : NULL;
      if (!exp)
	{
	  /* Only bother to handle cases tree_to_aff_combination will.  */
	  switch (code)
	    {
	    case POINTER_PLUS_EXPR:
	    case PLUS_EXPR:
	    case MINUS_EXPR:
	    case MULT_EXPR:
	      if (!expr_to_aff_combination (&current, code, TREE_TYPE (name),
					    gimple_assign_rhs1 (def),
					    gimple_assign_rhs2 (def)))
		continue;
	      break;
	    case NEGATE_EXPR:
	    case BIT_NOT_EXPR:
	      if (!expr_to_aff_combination (&current, code, TREE_TYPE (name),
					    gimple_assign_rhs1 (def)))
		continue;
	      break;
	    CASE_CONVERT:
	      if (!expr_to_aff_combination (&current, code, TREE_TYPE (name),
					    gimple_assign_rhs1 (def)))
		/* This makes us always expand conversions which we did
		   in the past and makes gcc.dg/tree-ssa/ivopts-lt-2.c
		   PASS, eliminating one induction variable in IVOPTs.
		   ???  But it is really excessive and we should try
		   harder to do without it.  */
		aff_combination_elt (&current, TREE_TYPE (name),
				     fold_convert (TREE_TYPE (name),
						   gimple_assign_rhs1 (def)));
	      break;
	    CASE_ADDR_EXPR:
	    case INTEGER_CST:
	    case POLY_INT_CST:
	      tree_to_aff_combination (gimple_assign_rhs1 (def),
				       TREE_TYPE (name), &current);
	      break;
	    default:
	      continue;
	    }
	  exp = XNEW (class name_expansion);
	  exp->in_progress = 1;
	  if (!*cache)
	    *cache = new hash_map<tree, name_expansion *>;
	  (*cache)->put (name, exp);
	  aff_combination_expand (&current, cache);
	  exp->expansion = current;
	  exp->in_progress = 0;
	}
      else
	{
	  /* Since we follow the definitions in the SSA form, we should not
	     enter a cycle unless we pass through a phi node.  */
	  gcc_assert (!exp->in_progress);
	  current = exp->expansion;
	}
      tree elt_type = (i == 0 && capability_type_p (type))
	? comb->type : noncapability_type (comb->type);
      if (!useless_type_conversion_p (elt_type, current.type))
	aff_combination_convert (&current, elt_type);

      /* Accumulate the new terms to TO_ADD, so that we do not modify
	 COMB while traversing it.  We could include the term -coef * E, to
	 remove it from COMB, but that would mean that this temporary affine
	 tree could break capability invariants (that there is a maximum of one
	 provenance-bearing capability in any affine tree).  Hence we
	 accumulate these negative coefficients in a separate affine tree
	 TO_SUBTRACT.  */
      scale = comb->elts[i].coef;
      aff_combination_zero (&curre, comb->type);
      aff_combination_add_elt (&curre, e, -scale);
      aff_combination_scale (&current, scale);
      aff_combination_add (&to_add, &current);
      aff_combination_add (&to_subtract, &curre);
    }
  aff_combination_add (comb, &to_subtract);
  aff_combination_add (comb, &to_add);
}

/* Similar to tree_to_aff_combination, but follows SSA name definitions
   and expands them recursively.  CACHE is used to cache the expansions
   of the ssa names, to avoid exponential time complexity for cases
   like

   a1 = a0 + a0;
   a2 = a1 + a1;
   a3 = a2 + a2;
   ...  */

void
tree_to_aff_combination_expand (tree expr, tree type, aff_tree *comb,
				hash_map<tree, name_expansion *> **cache)
{
  tree_to_aff_combination (expr, type, comb);
  aff_combination_expand (comb, cache);
}

/* Frees memory occupied by struct name_expansion in *VALUE.  Callback for
   hash_map::traverse.  */

bool
free_name_expansion (tree const &, name_expansion **value, void *)
{
  free (*value);
  return true;
}

/* Frees memory allocated for the CACHE used by
   tree_to_aff_combination_expand.  */

void
free_affine_expand_cache (hash_map<tree, name_expansion *> **cache)
{
  if (!*cache)
    return;

  (*cache)->traverse<void *, free_name_expansion> (NULL);
  delete (*cache);
  *cache = NULL;
}

/* If VAL != CST * DIV for any constant CST, returns false.
   Otherwise, if *MULT_SET is true, additionally compares CST and MULT,
   and if they are different, returns false.  Finally, if neither of these
   two cases occur, true is returned, and CST is stored to MULT and MULT_SET
   is set to true.  */

static bool
wide_int_constant_multiple_p (const poly_widest_int &val,
			      const poly_widest_int &div,
			      bool *mult_set, poly_widest_int *mult)
{
  poly_widest_int rem, cst;

  if (known_eq (val, 0))
    {
      if (*mult_set && maybe_ne (*mult, 0))
	return false;
      *mult_set = true;
      *mult = 0;
      return true;
    }

  if (maybe_eq (div, 0))
    return false;

  if (!multiple_p (val, div, &cst))
    return false;

  if (*mult_set && maybe_ne (*mult, cst))
    return false;

  *mult_set = true;
  *mult = cst;
  return true;
}

/* Returns true if VAL = X * DIV for some constant X.  If this is the case,
   X is stored to MULT.  */

bool
aff_combination_constant_multiple_p (aff_tree *val, aff_tree *div,
				     poly_widest_int *mult)
{
  bool mult_set = false;
  unsigned i;

  if (val->n == 0 && known_eq (val->offset, 0))
    {
      *mult = 0;
      return true;
    }
  if (val->n != div->n)
    return false;

  if (val->rest || div->rest)
    return false;

  if (!wide_int_constant_multiple_p (val->offset, div->offset,
				     &mult_set, mult))
    return false;

  for (i = 0; i < div->n; i++)
    {
      class aff_comb_elt *elt
	      = aff_combination_find_elt (val, div->elts[i].val, NULL);
      if (!elt)
	return false;
      if (!wide_int_constant_multiple_p (elt->coef, div->elts[i].coef,
					 &mult_set, mult))
	return false;
    }

  gcc_assert (mult_set);
  return true;
}

/* Prints the affine VAL to the FILE. */

static void
print_aff (FILE *file, aff_tree *val)
{
  unsigned i;
  signop sgn = TYPE_SIGN (val->type);
  if (POINTER_TYPE_P (val->type))
    sgn = SIGNED;
  fprintf (file, "{\n  type = ");
  print_generic_expr (file, val->type, TDF_VOPS|TDF_MEMSYMS);
  fprintf (file, "\n  offset = ");
  print_dec (val->offset, file, sgn);
  if (val->n > 0)
    {
      fprintf (file, "\n  elements = {\n");
      for (i = 0; i < val->n; i++)
	{
	  fprintf (file, "    [%d] = ", i);
	  print_generic_expr (file, val->elts[i].val, TDF_VOPS|TDF_MEMSYMS);

	  fprintf (file, " * ");
	  print_dec (val->elts[i].coef, file, sgn);
	  if (i != val->n - 1)
	    fprintf (file, ", \n");
	}
      fprintf (file, "\n  }");
  }
  if (val->rest)
    {
      fprintf (file, "\n  rest = ");
      print_generic_expr (file, val->rest, TDF_VOPS|TDF_MEMSYMS);
    }
  fprintf (file, "\n}");
}

/* Prints the affine VAL to the standard error, used for debugging.  */

DEBUG_FUNCTION void
debug_aff (aff_tree *val)
{
  print_aff (stderr, val);
  fprintf (stderr, "\n");
}

/* Computes address of the reference REF in ADDR.  The size of the accessed
   location is stored to SIZE.  Returns the ultimate containing object to
   which REF refers.  */

tree
get_inner_reference_aff (tree ref, aff_tree *addr, poly_widest_int *size)
{
  poly_int64 bitsize, bitpos;
  tree toff;
  machine_mode mode;
  int uns, rev, vol;
  aff_tree tmp;
  tree base = get_inner_reference (ref, &bitsize, &bitpos, &toff, &mode,
				   &uns, &rev, &vol);
  tree base_addr = build_fold_addr_expr (base);

  /* ADDR = &BASE + TOFF + BITPOS / BITS_PER_UNIT.  */

  tree_to_aff_combination (base_addr, sizetype, addr);

  if (toff)
    {
      tree_to_aff_combination (toff, sizetype, &tmp);
      aff_combination_add (addr, &tmp);
    }

  aff_combination_const (&tmp, sizetype, bits_to_bytes_round_down (bitpos));
  aff_combination_add (addr, &tmp);

  *size = bits_to_bytes_round_up (bitsize);

  return base;
}

/* Returns true if a region of size SIZE1 at position 0 and a region of
   size SIZE2 at position DIFF cannot overlap.  */

bool
aff_comb_cannot_overlap_p (aff_tree *diff, const poly_widest_int &size1,
			   const poly_widest_int &size2)
{
  /* Unless the difference is a constant, we fail.  */
  if (diff->n != 0)
    return false;

  if (!ordered_p (diff->offset, 0))
    return false;

  if (maybe_lt (diff->offset, 0))
    {
      /* The second object is before the first one, we succeed if the last
	 element of the second object is before the start of the first one.  */
      return known_le (diff->offset + size2, 0);
    }
  else
    {
      /* We succeed if the second object starts after the first one ends.  */
      return known_le (size1, diff->offset);
    }
}

