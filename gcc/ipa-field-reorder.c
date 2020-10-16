/* IPA Type Escape Analysis and Dead Field Elimination
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

  Contributed by Erick Ochoa <erick.ochoa@theobroma-systems.com>

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

/* Interprocedural field reordering
   
   The goal of this transformation is to re-order fields in structures
   at link time. 

   The field reordering transformation allows to reduce the memory
   footprint of structs, which not only improves performance, but also memory
   consumption.  So this is an interesting feature on embedded systems with
   tight memory constraints.

   First stage - DFA
   =================

   Use DFA to compute the set of FIELD_DECLs which can be reordered.
   This is different from IPA-DFE in that all reads do not prevent reordering
   of fields, with the exception of those which take the address of a field
   or those in MEM_REF. Therefore ExprEscaper remains the same, but
   GimpleCaster is modified.

   Second stage - Reorder fields
   =============================

   Use TypeReconstructorFieldReordering to reorder fields.

   Third stage - Substitute Types and Relayout
   ===========================================

   Change all types changed and references to FIELD_DECLs
 */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple-expr.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "tree-pretty-print.h"
#include "tree-inline.h"
#include "ipa-fnsummary.h"
#include "ipa-utils.h"
#include "tree-ssa-ccp.h"
#include "stringpool.h"
#include "attribs.h"
#include "basic-block.h" //needed for gimple.h
#include "function.h"    //needed for gimple.h
#include "gimple.h"
#include "stor-layout.h"
#include "cfg.h" // needed for gimple-iterator.h
#include "gimple-iterator.h"
#include "gimplify.h"      //unshare_expr
#include "value-range.h"   // make_ssa_name dependency
#include "tree-ssanames.h" // make_ssa_name
#include "ssa.h"
#include "tree-into-ssa.h"
#include "gimple-ssa.h" // update_stmt
#include "tree.h"
#include "gimple-expr.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "tree-pretty-print.h"
#include "tree-inline.h"
#include "ipa-fnsummary.h"
#include "ipa-utils.h"
#include "tree-ssa-ccp.h"
#include "stringpool.h"
#include "attribs.h"
#include "tree-ssa-alias.h"
#include "tree-ssanames.h"
#include "gimple.h"
#include "cfg.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include "gimple-pretty-print.h"
#include <vector>
#include <set>
#include <map>
#include <stack>
#include <algorithm>

#include "ipa-type-escape-analysis.h"
#include "ipa-dfe.h"

// TODO:
// This was copy pasted from tree-ssa-structalias.c
// Maybe delete this and make the function visible?
static HOST_WIDE_INT
bitpos_of_field (const tree fdecl)
{
  if (!tree_fits_shwi_p (DECL_FIELD_OFFSET (fdecl))
      || !tree_fits_shwi_p (DECL_FIELD_BIT_OFFSET (fdecl)))
    return -1;

  return (tree_to_shwi (DECL_FIELD_OFFSET (fdecl)) * BITS_PER_UNIT
	  + tree_to_shwi (DECL_FIELD_BIT_OFFSET (fdecl)));
}

/* Basically we are creating a specialized GimpleAccesser for FieldReordering.
 * Here instead of marking fields as "READ" or "WRITE", we are marking them as
 * "READ" via pointer arithmetic.  Other "READS" and "WRITES" are ignored since
 * it would be possible to reorder the fields.
 */
class GimpleAccesserFieldReordering : public GimpleAccesser
{
public:
  GimpleAccesserFieldReordering (){};

private:
  /* Mark all RHS expressions reachable from S as Read.
     all all LHS expressions reachable from S as Written.  */
  virtual void _walk_pre_gcall (gcall *s);

  /* Mark all RHS expressions reachable from S as Read.
     Mark all LHS expressions reachable from S as Written.  */
  virtual void _walk_pre_gassign (gassign *s);

  /* Mark all all expressions reachable from S as read.  */
  virtual void _walk_pre_greturn (greturn *s);

  /* Mark all expressions reachable from S as read.  */
  virtual void _walk_pre_gcond (gcond *s);

  // Do we need a glabel? I don't think so...
  // But we might need a gswitch.
};

/* Class used to create new types derived from types that might be
 * reordered.
 */
class TypeReconstructorFieldReordering : public TypeReconstructor
{
public:
  TypeReconstructorFieldReordering (record_field_offset_map_t records,
				    const char *suffix)
    : TypeReconstructor (records, suffix)
  {};

private:
  // Compute new RECORD_TYPE.
  virtual void _walk_RECORD_TYPE_post (const_tree);
};

/* Compare FIELD_DECL tree based on TYPE_SIZE unit */
static bool
compare_FIELD_DECLs_TYPE_SIZE (const_tree _l, const_tree _r)
{
  gcc_assert (_l && _r);

  tree l = const_tree_to_tree (_l);
  tree r = const_tree_to_tree (_r);

  const enum tree_code code_l = TREE_CODE (l);
  const enum tree_code code_r = TREE_CODE (r);
  const bool is_left_field_decl = code_l == FIELD_DECL;
  const bool is_right_field_decl = code_r == FIELD_DECL;
  bool is_valid = is_left_field_decl && is_right_field_decl;
  gcc_assert (is_valid);

  tree type_l = TREE_TYPE (l);
  tree type_r = TREE_TYPE (r);
  const bool is_type_l = TYPE_P (type_l);
  const bool is_type_r = TYPE_P (type_r);
  is_valid = is_type_l && is_type_r;
  gcc_assert (is_valid);

  tree size_l = TYPE_SIZE_UNIT (type_l);
  tree size_r = TYPE_SIZE_UNIT (type_r);
  is_valid = size_l && size_r;
  gcc_assert (is_valid);

  int int_l = tree_to_shwi (size_l);
  int int_r = tree_to_shwi (size_r);
  const bool is_gte_l = int_l >= 0;
  const bool is_gte_r = int_r >= 0;
  is_valid = is_gte_l && is_gte_r;
  gcc_assert (is_valid);

  return int_l > int_r;
}

void
TypeReconstructorFieldReordering::_walk_RECORD_TYPE_post (const_tree t)
{
  const_tree t2 = for_reference.top ();
  gcc_assert (t2 == t);
  for_reference.pop ();

  tree copy = in_progress.top ();
  in_progress.pop ();
  field_tuple_list_t field_tuple_list = field_list_stack.top ();
  field_list_stack.pop ();

  // So, here all the work has been done to make sure
  // that the fields produced a field_tuple_list_t
  // with old fields and pointers to new fields.
  // There might be NULL values if new fields are that can be resorted..
  // So, now we want to do a couple of things.
  // First, collect all fields in a struct and make a copy of them
  bool is_modified = get_is_modified (t);
  std::vector<const_tree> to_reorder;
  is_modified = true;
  for (field_tuple_list_t::iterator i = field_tuple_list.begin (), e = field_tuple_list.end ();
       i != e; ++i)
    {
      field_tuple_t field_tuple = *i;
      tree modified_field = field_tuple.second;

      if (!modified_field)
	{
	  // This means that the field can be re-ordered...
	  is_modified = true;
	}

      log ("we can reorder %s ? %s\n",
	   TypeStringifier::get_field_identifier (field_tuple.first).c_str (),
	   !modified_field ? "true" : "false");
      to_reorder.push_back (
	(const_tree) copy_node (const_tree_to_tree (field_tuple.first)));
    }

  if (is_modified)
    {
      tree prev_field = NULL;
      bool entered_loop = false;
      // Sort them
      std::sort (to_reorder.begin (), to_reorder.end (),
		 compare_FIELD_DECLs_TYPE_SIZE);
      is_modified = false;

      for (field_tuple_list_t::iterator i = field_tuple_list.begin (), e = field_tuple_list.end ();
	   i != e; ++i)
	{
	  field_tuple_t field_tuple = *i;
	  tree modified_field = field_tuple.second;

	  if (!modified_field)
	    {
	      // This means that the field can be re-ordered...
	      is_modified = true;
	    }

	  tree current_field = modified_field;
	  if (!is_modified)
	    {
	      prev_field = current_field;
	      continue;
	    }

	  gcc_assert (!modified_field && is_modified);
	  // Create new TYPE_FIELDS with the order we want
	  for (std::vector<const_tree>::iterator j = to_reorder.begin (), f = to_reorder.end (); j != f; ++j)
	    {
	      entered_loop = true;
	      const_tree current_field_inner = *j;
	      if (!prev_field)
		{
		  TYPE_FIELDS (copy) = const_tree_to_tree (current_field_inner);
		}
	      else
		{
		  DECL_CHAIN (prev_field)
		    = const_tree_to_tree (current_field_inner);
		}

	      prev_field = const_tree_to_tree (current_field_inner);
	    }

	  if (entered_loop)
	    break;
	}

      // Modify _reorg_fields map
      for (std::vector<const_tree>::iterator i = to_reorder.begin (), e = to_reorder.end (); i != e; ++i)
	{
	  const_tree to_find = *i;
	  unsigned to_find_i = bitpos_of_field (const_tree_to_tree (to_find));
	  const char *to_find_str
	    = TypeStringifier::get_field_identifier (to_find).c_str ();
	  // O^2 for now but an improvement can be to change this
	  for (tree field = TYPE_FIELDS (t); field; field = DECL_CHAIN (field))
	    {
	      // safe for anonymous structs
	      const char *haystack
		= TypeStringifier::get_field_identifier (field).c_str ();
	      unsigned haystack_i = bitpos_of_field (field);
	      if (haystack_i == to_find_i)
		{
		  _reorg_fields[field]
		    = std::make_pair (const_tree_to_tree (to_find), false);
		  log ("substituting %s for %s\n", to_find_str, haystack);
		}
	    }
	}
    }

  const bool is_main_variant = TYPE_MAIN_VARIANT (t) == t;
  // We already must have done the main variant...
  if (!is_main_variant)
    {
      tree main = TYPE_MAIN_VARIANT (t);
      tree main_reorg = _reorg_map[main];
      tree copy_variant = build_distinct_type_copy (main_reorg);
      TYPE_NAME (copy_variant)
	= get_new_identifier (copy, this->get_new_suffix ());
      TYPE_SIZE (copy_variant) = NULL;
      //TYPE_SIZE (main_reorg) = NULL;
      TypeStringifier stringifier;
      std::string name_o = stringifier.stringify(main_reorg);
      std::string name_r = stringifier.stringify(copy_variant);
      log ("before layout main_reorged %s\n", name_o.c_str());
      log ("there is a type alias set %s\n", TYPE_ALIAS_SET_KNOWN_P(main_reorg) == 0 ? "T" : "F");
      log ("before layout reorg %s\n", name_r.c_str());
      TYPE_ALIAS_SET(copy_variant) = -1;
      log ("there is a type alias set %s\n", TYPE_ALIAS_SET_KNOWN_P(copy_variant) == 0 ? "T" : "F");
      layout_type (copy_variant);
      TYPE_MAIN_VARIANT (copy_variant) = main_reorg;
      _reorg_map[t] = copy_variant;
    }
  else
    {
      // Ok, so now that we have fixed the TYPE_FIELDS of the copy...
      // We need to call layout_type
      copy = is_modified ? build_distinct_type_copy (copy) : copy;
      TYPE_NAME (copy) = is_modified
			   ? get_new_identifier (copy, this->get_new_suffix ())
			   : TYPE_NAME (copy);
      // This is useful so that we go again through type layout
      TYPE_SIZE (copy) = is_modified ? NULL : TYPE_SIZE (copy);
      TYPE_MAIN_VARIANT (copy) = is_modified ? copy : TYPE_MAIN_VARIANT (copy);
      if (is_modified)
	layout_type (copy);
      tree _t = const_tree_to_tree (t);
      _reorg_map[t] = is_modified ? copy : _t;
    }

  tree record = _reorg_map[t];
  for (tree field = TYPE_FIELDS (record); field; field = DECL_CHAIN (field))
    {
      relayout_decl (field);
      log ("new offset for %s %d\n",
	   TypeStringifier::get_field_identifier (field).c_str (),
	   bitpos_of_field (field));
    }
}

/* Mark all as empty (a.k.a. we can sort) */
void
GimpleAccesserFieldReordering::_walk_pre_gassign (gassign *s)
{
  // There seems to be quite a bit of code duplication here...
  const enum gimple_rhs_class code = gimple_assign_rhs_class (s);
  switch (code)
    {
    case GIMPLE_TERNARY_RHS:
      {
	const_tree rhs3 = gimple_assign_rhs3 (s);
	gcc_assert (rhs3);
	exprAccessor.update (rhs3, Empty);
      }
    /* fall-through */
    case GIMPLE_BINARY_RHS:
      {
	const_tree rhs2 = gimple_assign_rhs2 (s);
	gcc_assert (rhs2);
	exprAccessor.update (rhs2, Empty);
      }
    /* fall-through */
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
      {
	const_tree rhs1 = gimple_assign_rhs1 (s);
	exprAccessor.update (rhs1, Empty);
	const_tree lhs = gimple_assign_lhs (s);
	if (!lhs)
	  break;
	exprAccessor.update (lhs, Empty);
	break;
      }
    default:
      gcc_unreachable ();
      break;
    }
}

/* Mark all as empty (a.k.a. we can sort) */
void
GimpleAccesserFieldReordering::_walk_pre_gcall (gcall *s)
{
  unsigned n = gimple_call_num_args (s);
  for (unsigned i = 0; i < n; i++)
    {
      const_tree a = gimple_call_arg (s, i);
      gcc_assert (a);
      exprAccessor.update (a, Empty);
    }

  const_tree lhs = gimple_call_lhs (s);
  if (!lhs)
    return;
  exprAccessor.update (lhs, Empty);
}

/* Mark all as empty (a.k.a. we can sort) */
void
GimpleAccesserFieldReordering::_walk_pre_greturn (greturn *s)
{
  const_tree val = gimple_return_retval (s);
  if (!val)
    return;
  exprAccessor.update (val, Empty);
}

/* Mark all as empty (a.k.a. we can sort) */
void
GimpleAccesserFieldReordering::_walk_pre_gcond (gcond *s)
{
  const_tree lhs = gimple_cond_lhs (s);
  const_tree rhs = gimple_cond_rhs (s);
  gcc_assert (lhs && rhs);
  exprAccessor.update (lhs, Empty);
  exprAccessor.update (rhs, Empty);
}

/* Top level function.  */
static unsigned int
lto_fr_execute ();

static record_field_map_t
find_fields_accessed ();

namespace {
const pass_data pass_data_ipa_field_reorder = {
  SIMPLE_IPA_PASS,
  "field-reorder",
  OPTGROUP_NONE,
  TV_NONE,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  0,
  0,
};

class pass_ipa_field_reorder : public simple_ipa_opt_pass
{
public:
  pass_ipa_field_reorder (gcc::context *ctx)
    : simple_ipa_opt_pass (pass_data_ipa_field_reorder, ctx)
  {}

  virtual bool gate (function *) { return in_lto_p && flag_ipa_field_reorder; }
  virtual unsigned execute (function *) { return lto_fr_execute (); }
};
} // namespace

record_field_map_t static find_fields_accessed ()
{
  GimpleAccesserFieldReordering accesser;
  accesser.walk ();

  // Dump for debugging.
  if (flag_print_access_analysis)
    accesser.print_accesses ();

  // This record_field_map holds
  // RECORD_TYPE -> (FIELD_DECL -> how field is accessed)
  record_field_map_t record_field_map = accesser.get_map ();
  return record_field_map;
}

/* record_field_offset_map holds information on which FIELD_DECLs might be
 * resorted from RECORD_TYPEs.  to_modify holds trees which point to a
 * RECORD_TYPE which might be resorted.  From these two inputs, we need to
 * compute two maps:
 * * a map of RECORD_TYPE (old) -> RECORD_TYPE (new)
 * * a map of FIELD_DECL (old) -> FIELD_DECL (new)

 * The first maps trees in to_modify (which point to RECORD_TYPEs (old)) to
 * trees which have been modified to point to the new definition of
 * RECORD_TYPEs.

 * The second maps old FIELD_DECLs trees to the new FIELD_DECLs.
 */
reorg_maps_t
get_reordered_field_maps (record_field_offset_map_t record_field_offset_map,
			  std::set<const_tree> to_modify)
{
  TypeStringifier stringifier;

  TypeReconstructorFieldReordering reconstructor (record_field_offset_map,
						  "reorder");
  for (std::set<const_tree>::const_iterator i = to_modify.begin (),
					    e = to_modify.end ();
       i != e; ++i)
    {
      const_tree record = *i;
      reconstructor.walk (TYPE_MAIN_VARIANT (record));
    }

  for (std::set<const_tree>::const_iterator i = to_modify.begin (),
					    e = to_modify.end ();
       i != e; ++i)
    {
      const_tree record = *i;
      reconstructor.walk (record);
    }

  reorg_record_map_t map = reconstructor.get_map ();
  reorg_field_map_t field_map = reconstructor.get_field_map ();

  // Here, we are just making sure that we are not doing anything too crazy.
  // Also, we found some types for which TYPE_CACHED_VALUES_P is not being
  // rewritten.  This is probably indicative of a bug in
  // TypeReconstructorFieldReordering.
  for (std::map<const_tree, tree>::const_iterator i = map.begin (),
						  e = map.end ();
       i != e; ++i)
    {
      const_tree o_record = i->first;
      std::string o_name = stringifier.stringify (o_record);
      log ("original: %s\n", o_name.c_str ());
      tree r_record = i->second;
      std::string r_name
	= r_record ? stringifier.stringify (r_record) : std::string ("");
      log ("modified: %s\n", r_name.c_str ());
      if (!r_record)
	continue;
      tree m_record = TYPE_MAIN_VARIANT (r_record);
      // Info: We had a bug where some TYPED_CACHED_VALUES were preserved?
      tree _o_record = const_tree_to_tree (o_record);
      TYPE_CACHED_VALUES_P (_o_record) = false;
      TYPE_CACHED_VALUES_P (m_record) = false;

      bool in_map = map.find (m_record) != map.end ();
      if (!in_map)
	continue;
      tree mm_record = map[m_record];
      // Info: I think this is no longer needed...
      // Please verify
      TYPE_MAIN_VARIANT (r_record) = mm_record;
    }

  return std::make_pair (map, field_map);
}

/* Top level function.  */
static unsigned int
lto_fr_execute ()
{
  log ("here in field reordering \n");
  // Analysis.
  detected_incompatible_syntax = false;
  std::map<tree, bool> whitelisted = get_whitelisted_nodes();
  tpartitions_t escaping_nonescaping_sets
    = partition_types_into_escaping_nonescaping (whitelisted);
  record_field_map_t record_field_map = find_fields_accessed ();
  record_field_offset_map_t record_field_offset_map
    = obtain_nonescaping_unaccessed_fields (escaping_nonescaping_sets,
					    record_field_map, 0);

  if (detected_incompatible_syntax || record_field_offset_map.empty ())
    return 0;

  // Prepare for transformation.
  std::set<const_tree> to_modify
    = get_all_types_pointing_to (record_field_offset_map,
				 escaping_nonescaping_sets);

  reorg_maps_t replacements
    = get_reordered_field_maps (record_field_offset_map, to_modify);
  reorg_record_map_t map = replacements.first;
  reorg_field_map_t field_map = replacements.second;
  substitute_types_in_program (map, field_map, false);

  GimpleWalker walker;
  walker.walk ();
  log ("finished!\n");

  return 0;
}

simple_ipa_opt_pass *
make_pass_ipa_field_reorder (gcc::context *ctx)
{
  return new pass_ipa_field_reorder (ctx);
}
