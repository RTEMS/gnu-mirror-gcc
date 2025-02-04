/* Callback attribute handling
   Copyright (C) 2025 Free Software Foundation, Inc.
   Contributed by Josef Melcr <melcrjos@fit.cvut.cz>

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   GCC is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef ATTR_CALLBACK_H
#define ATTR_CALLBACK_H
#include "attribs.h"
#include "system.h"
#include "tree.h"
#include "function.h"
#include "basic-block.h"
#include "coretypes.h"
#include "is-a.h"
#include "predict.h"
#include "internal-fn.h"
#include "tree-ssa-alias.h"
#include "gimple-expr.h"
#include "gimple.h"
#include "vec.h"

enum callback_position
{
  /* Value used when an argument of a callback function
     is unknown or when multiple values may be used. */
  CB_UNKNOWN_POS = 0
};

/* Given an instance of callback attribute, return the 0-based
   index of the called function in question. */
inline int
callback_get_fn_index (tree cb_attr)
{
  tree args = TREE_VALUE (cb_attr);
  int idx = TREE_INT_CST_LOW (TREE_VALUE (args)) - 1;
  return idx;
}

/* Given an instance of callback attribute, return the 0-base indices
   of arguments passed to the callback. For a callback function taking
   n parameters, returns a vector of n indices of their values in the parameter
   list of it's caller. Indices with unknown positions will be filled with
   an identity. */
inline auto_vec<int>
callback_get_arg_mapping (tree decl)
{
  tree attr = lookup_attribute ("callback", DECL_ATTRIBUTES (decl));
  gcc_checking_assert (attr);
  tree args = TREE_VALUE (attr);
  auto_vec<int> res;
  tree it;

  /* Skip over the first argument, which denotes
     which argument is the called function. */
  for (it = TREE_CHAIN (args); it != NULL_TREE; it = TREE_CHAIN (it))
    {
      int idx = TREE_INT_CST_LOW (TREE_VALUE (it));

      /* CB_UNKNOWN_POS signifies an unknown argument,
	 replace it with identity for convenience */
      if (idx == CB_UNKNOWN_POS)
	idx = res.length ();
      /* arguments use 1-based indexing, so we have
	 to subtract 1 */
      else
	idx -= 1;

      res.safe_push (idx);
    }

  return res;
}

/* Given a call statement of the parent, it's attribute list and
   a decl of the callback, returns a 0-based index of the callback
   function in the parameters of it's caller function. Arguments
   are extracted from the call statement. If kernel_decl is a decl
   of a clone, it's parent decl will be considered as well. */
inline int
callback_fetch_fn_position (gcall *call, tree attr_list, tree kernel_decl)
{
  tree original_decl = DECL_ORIGIN (kernel_decl);
  tree cb_attr = lookup_attribute ("callback", attr_list);
  gcc_checking_assert (cb_attr);
  int res = -1;
  for (; cb_attr; cb_attr = lookup_attribute ("callback", TREE_CHAIN (cb_attr)))
    {
      int idx = callback_get_fn_index (cb_attr);
      tree arg = gimple_call_arg (call, idx);
      if (TREE_CODE (arg) == ADDR_EXPR)
	{
	  tree pointee = TREE_OPERAND (arg, 0);
	  if (pointee != NULL_TREE
	      && (pointee == kernel_decl || pointee == original_decl))
	    {
	      res = idx;
	      break;
	    }
	}
    }
  gcc_checking_assert (res != -1);
  return res;
}

/* Returns the element at index idx in the list or NULL_TREE if
   the list isn't long enough. NULL_TREE is used as the endpoint. */
static tree
get_nth_list_elem (tree list, unsigned idx)
{
  tree res = NULL_TREE;
  unsigned i = 0;
  tree it;
  for (it = list; it != NULL_TREE; it = TREE_CHAIN (it), i++)
    {
      if (i == idx)
	{
	  res = TREE_VALUE (it);
	  break;
	}
    }
  return res;
}

/* Handle a "callback" attribute; arguments as in
   struct attribute_spec.handler. */
inline tree
handle_callback_attribute (tree *node, tree name, tree args,
			   int ARG_UNUSED (flags), bool *no_add_attrs)
{
  tree decl = *node;
  if (TREE_CODE (decl) != FUNCTION_DECL)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"%qE attribute can only be used on functions", name);
      *no_add_attrs = true;
    }

  tree cb_fn_idx_node = TREE_VALUE (args);
  if (TREE_CODE (cb_fn_idx_node) != INTEGER_CST)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"argument specifying callback function position is not an "
		"integer constant");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  /* We have to use the function type for validation, as
     DECL_ARGUMENTS returns NULL at this point. */
  unsigned callback_fn_idx = TREE_INT_CST_LOW (cb_fn_idx_node) - 1;
  tree decl_type_args = TYPE_ARG_TYPES (TREE_TYPE (decl));
  unsigned decl_nargs = list_length (decl_type_args);
  if (callback_fn_idx >= decl_nargs)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"callback function position out of range");
      *no_add_attrs = true;
      return NULL_TREE;
    }

  /* Search for the type of the callback function
     in parameters of the original function. */
  tree cfn = get_nth_list_elem(decl_type_args, callback_fn_idx);
  if (cfn == NULL_TREE)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"could not retrieve callback function from arguments");
      *no_add_attrs = true;
      return NULL_TREE;
    }
  tree cfn_pointee_type = TREE_TYPE (cfn);
  if (TREE_CODE (cfn) != POINTER_TYPE
      || TREE_CODE (cfn_pointee_type) != FUNCTION_TYPE)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"argument no. %d is not an address of a function",
		callback_fn_idx);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  tree it;
  tree type_args = TYPE_ARG_TYPES (cfn_pointee_type);
  /* Compare the length of the list of argument indices
     and the real number of parameters the callback takes. */
  unsigned cfn_nargs = list_length (TREE_CHAIN (args));
  unsigned type_nargs = list_length (type_args);
  for (it = type_args; it != NULL_TREE; it = TREE_CHAIN (it))
    if (it == void_list_node)
      {
	--type_nargs;
	break;
      }
  if (cfn_nargs != type_nargs)
    {
      error_at (DECL_SOURCE_LOCATION (decl),
		"argument number mismatch, %d expected, got %d", type_nargs,
		cfn_nargs);
      *no_add_attrs = true;
      return NULL_TREE;
    }

  unsigned curr = 0;
  tree cfn_it;
  /* Validate type compatibility of the arguments passed
     from caller function to callback. "it" is used to step
     through the parameters of the caller, "cfn_it" is
     stepping through the parameters of the callback. */
  for (it = type_args, cfn_it = TREE_CHAIN (args); curr < type_nargs;
       it = TREE_CHAIN (it), cfn_it = TREE_CHAIN (cfn_it), curr++)
    {
      if (TREE_CODE (TREE_VALUE (cfn_it)) != INTEGER_CST)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "argument no. %d is not an integer constant", curr + 1);
	  *no_add_attrs = true;
	  continue;
	}

      unsigned arg_idx = TREE_INT_CST_LOW (TREE_VALUE (cfn_it));

      /* No need to check for type compatibility,
	 if we don't know what we are passing. */
      if (arg_idx == CB_UNKNOWN_POS)
	{
	  continue;
	}

      arg_idx -= 1;
      /* Report an error if the position is out of bounds,
	 but we can still check the rest of the arguments. */
      if (arg_idx >= decl_nargs)
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "callback argument index %d is out of range", arg_idx + 1);
	  *no_add_attrs = true;
	  continue;
	}

      tree arg_type = get_nth_list_elem (decl_type_args, arg_idx);
      tree expected_type = TREE_VALUE (it);
      /* Check the type of the value we are about to pass ("arg_type")
	 for compatibility with the actual type the callback function
	 expects ("expected_type"). */
      if (!types_compatible_p (expected_type, arg_type))
	{
	  error_at (DECL_SOURCE_LOCATION (decl),
		    "argument type at index %d is not compatible with callback "
		    "argument type at index %d",
		    curr, arg_idx);
	  *no_add_attrs = true;
	  continue;
	}
    }

  return NULL_TREE;
}

#endif /* ATTR_CALLBACK_H  */
