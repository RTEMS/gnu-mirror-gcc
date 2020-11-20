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

/* Interprocedural dead field elimination (IPA-DFE)

   The goal of this transformation is to

   1) Create new types to replace RECORD_TYPEs which hold dead fields.
   2) Substitute instances of old RECORD_TYPEs for new RECORD_TYPEs.
   3) Substitute instances of old FIELD_DECLs for new FIELD_DECLs.
   4) Fix some instances of pointer arithmetic.
   5) Relayout where needed.

   First stage - DFA
   =================

   Use DFA to compute the set of FIELD_DECLs which can be deleted.

   Second stage - Reconstruct Types
   ================================

   This stage is done by two family of classes, the SpecificTypeCollector
   and the TypeReconstructor.

   The SpecificTypeCollector collects all TYPE_P trees which point to
   RECORD_TYPE trees returned by DFA.  The TypeReconstructor will create
   new RECORD_TYPE trees and new TYPE_P trees replacing the old RECORD_TYPE
   trees with the new RECORD_TYPE trees.

   Third stage - Substitute Types and Relayout
   ===========================================

   This stage is handled by ExprRewriter and GimpleRewriter.
   Some pointer arithmetic is fixed here to take into account those eliminated
   FIELD_DECLS.
 */

#include "config.h"

#include <map>
#include <set>
#include <vector>
#include <stack>
#include <string>

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

#include "ipa-type-escape-analysis.h"
#include "ipa-dfe.h"

/*
 * Find all non_escaping types which point to RECORD_TYPEs in
 * record_field_offset_map.
 */
std::set<tree>
get_all_types_pointing_to (record_field_offset_map_t record_field_offset_map,
			   tpartitions_t casting)
{
  const tset_t &non_escaping = casting.non_escaping;

  std::set<tree> specific_types;
  type_stringifier stringifier;

  // Here we are just placing the types of interest in a set.
  for (std::map<tree, field_offsets_t>::const_iterator i
       = record_field_offset_map.begin (),
       e = record_field_offset_map.end ();
       i != e; ++i)
    {
      tree record = i->first;
      std::string name = stringifier.stringify (record);
      specific_types.insert (record);
    }

  specific_type_collector specifier (specific_types);

  // SpecificTypeCollector will collect all types which point to the types in
  // the set.
  for (std::set<tree>::const_iterator i = non_escaping.begin (),
					    e = non_escaping.end ();
       i != e; ++i)
    {
      tree type = *i;
      specifier.walk (type);
    }

  // These are all the types which need modifications.
  std::set<tree> to_modify = specifier.get_set ();
  return to_modify;
}

/* record_field_offset_map holds information on which FIELD_DECLs might be
 * deleted from RECORD_TYPEs.  to_modify holds trees which point to a
 * RECORD_TYPE which might be deleted.  From these two inputs, we need to
 * compute two maps:
 * * a map of RECORD_TYPE (old) -> RECORD_TYPE (new)
 * * a map of FIELD_DECL (old) -> FIELD_DECL (new)

 * The first maps trees in to_modify (which point to RECORD_TYPEs (old)) to
 * trees which have been modified to point to the new definition of
 * RECORD_TYPEs.

 * The second maps old FIELD_DECLs trees to the new FIELD_DECLs.
 */
reorg_maps_t
get_types_replacement (record_field_offset_map_t record_field_offset_map,
		       std::set<tree> to_modify)
{
  type_stringifier stringifier;

  type_reconstructor reconstructor (record_field_offset_map, "reorg");
  for (std::set<tree>::const_iterator i = to_modify.begin (),
					    e = to_modify.end ();
       i != e; ++i)
    {
      tree record = *i;
      reconstructor.walk (TYPE_MAIN_VARIANT (record));
    }

  for (std::set<tree>::const_iterator i = to_modify.begin (),
					    e = to_modify.end ();
       i != e; ++i)
    {
      tree record = *i;
      reconstructor.walk (record);
    }

  reorg_record_map_t map = reconstructor.get_map ();
  reorg_field_map_t field_map = reconstructor.get_field_map ();

  // Here, we are just making sure that we are not doing anything too crazy.
  // Also, we found some types for which TYPE_CACHED_VALUES_P is not being
  // rewritten.  This is probably indicative of a bug in TypeReconstructor.
  for (std::map<tree, tree>::const_iterator i = map.begin (),
						  e = map.end ();
       i != e; ++i)
    {
      tree o_record = i->first;
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
      tree _o_record = tree_to_tree (o_record);
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

/* Walk the gimple program and substitute
 * * the trees in map with map's values.
 * * the trees in field_map with field_map's values.
 */
void
substitute_types_in_program (reorg_record_map_t map,
			     reorg_field_map_t field_map, bool _delete)
{
  gimple_type_rewriter rewriter (map, field_map, _delete);
  rewriter.walk ();
  rewriter._rewrite_function_decl ();
}

/* Return a set of trees which point to the set of trees
 * that can be modified.
 */
std::set<tree>
specific_type_collector::get_set ()
{
  return to_return;
}

void
specific_type_collector::_walk_POINTER_TYPE_pre (tree t)
{
  path.insert (t);
}

void
specific_type_collector::_walk_POINTER_TYPE_post (tree t)
{
  path.erase (t);
}

void
specific_type_collector::_walk_ARRAY_TYPE_pre (tree t)
{
  path.insert (t);
}

void
specific_type_collector::_walk_ARRAY_TYPE_post (tree t)
{
  path.erase (t);
}

void
specific_type_collector::_walk_UNION_TYPE_pre (tree t)
{
  path.insert (t);
}

void
specific_type_collector::_walk_UNION_TYPE_post (tree t)
{
  path.erase (t);
}

/* If we find a RECORD_TYPE which is of interest, place
 * all types which we are currently keeping track of in TO_RETURN.
 */
void
specific_type_collector::_walk_RECORD_TYPE_pre (tree t)
{
  const bool in_set
    = _collect_these_types.find (t) != _collect_these_types.end ();
  const bool must_collect = in_set;
  path.insert (t);
  if (!must_collect)
    return;

  for (std::set<tree>::const_iterator i = path.begin (),
					    e = path.end ();
       i != e; ++i)
    {
      tree type = *i;
      to_return.insert (type);
    }
}

void
specific_type_collector::_walk_RECORD_TYPE_post (tree t)
{
  path.erase (t);
}

/*
 * old RECORD_TYPE -> new RECORD_TYPE.
 */
reorg_record_map_t
type_reconstructor::get_map ()
{
  return _reorg_map;
}

/*
 * old FIELD_DECL -> new FIELD_DECL.
 */
reorg_field_map_t
type_reconstructor::get_field_map ()
{
  return _reorg_fields;
}

void
type_reconstructor::set_is_not_modified_yet (tree t)
{
  gcc_assert (t);
  const bool is_in_reorg_map = _reorg_map.find (t) != _reorg_map.end ();
  modified_map[t] = false;
  if (is_in_reorg_map)
    mark_all_pointing_here_as_modified ();

  tree tt = TREE_TYPE (t);
  if (!tt)
    return;

  const bool is_in_reorg_map_2 = _reorg_map.find (tt) != _reorg_map.end ();
  if (!is_in_reorg_map_2)
    return;

  tree type = _reorg_map[tt];
  bool is_modified
    = strstr (type_stringifier::get_type_identifier (type).c_str (), ".reorg");
  is_modified
    |= (bool) strstr (type_stringifier::get_type_identifier (type).c_str (),
		      ".reorder");
  if (!is_modified)
    return;

  mark_all_pointing_here_as_modified ();
}

void
type_reconstructor::mark_all_pointing_here_as_modified ()
{
  for (is_modified_map_t::iterator i = modified_map.begin (),
	e = modified_map.end (); i != e; ++i)
    {
      i->second = true;
    }
}

bool
type_reconstructor::get_is_modified (tree t)
{
  gcc_assert (t);
  const bool in_map = modified_map.find (t) != modified_map.end ();
  gcc_assert (in_map);
  bool retval = modified_map[t];
  modified_map.erase (t);

  bool points_to_record = false;
  tree _t = tree_to_tree (t);
  tree tt = _t;
  while (TREE_TYPE (tt))
    {
      tt = TREE_TYPE (tt);
    }
  points_to_record = TREE_CODE (tt) == RECORD_TYPE;

  return retval && points_to_record;
}

bool
type_reconstructor::is_memoized (tree t)
{
  const bool already_changed = _reorg_map.find (t) != _reorg_map.end ();
  mark_all_pointing_here_as_modified ();
  return already_changed;
}

const char *
type_reconstructor::get_new_suffix ()
{
  return _suffix;
}

tree
get_new_identifier (tree type, const char *suffix)
{
  const char *identifier = type_stringifier::get_type_identifier (type).c_str ();
  const bool is_new_type = strstr (identifier, suffix);
  gcc_assert (!is_new_type);
  char *new_name;
  asprintf (&new_name, "%s.%s", identifier, suffix);
  return get_identifier (new_name);
}

// Invariant for all _pre functions:
// _reorg_map[t] == NULL (due to memoization)
//
// Invariant for all _post functions:
// _reorg_map[TREE_TYPE (t)] != NULL
// unless TREE_TYPE (t) is not a type which points to a record
// a.k.a. no modifications
//
// To preserve invariant, we must include
// the following at the end of all _post functions:
// _reorg_map[t] =  reorg type
//
// How information is passed?
// To further _post functions via stacks
//
// To previous _post functions via _reorg_map (and maybe others?)
//
//
void
type_reconstructor::_walk_ARRAY_TYPE_pre (tree t)
{
  for_reference.push (t);
  set_is_not_modified_yet (t);

  tree _t = tree_to_tree (t);
  tree copy = build_variant_type_copy (_t);
  tree domain = TYPE_DOMAIN (t);
  if (domain)
    {
      tree copy_domain = copy_node (domain);
      tree min = TYPE_MIN_VALUE (domain);
      tree max = TYPE_MAX_VALUE (domain);
      TYPE_MIN_VALUE (copy_domain) = copy_node (min);
      TYPE_MAX_VALUE (copy_domain) = copy_node (max);
    }
  in_progress.push (copy);
}

void
type_reconstructor::_walk_ARRAY_TYPE_post (tree t)
{
  tree t2 = for_reference.top ();
  gcc_assert (t2 == t);
  for_reference.pop ();
  tree copy = in_progress.top ();
  in_progress.pop ();

  bool is_modified = get_is_modified (t);

  TREE_TYPE (copy) = build_variant_type_copy (TREE_TYPE (copy));
  copy = is_modified ? build_distinct_type_copy (copy) : copy;
  TREE_TYPE (copy) = is_modified ? _reorg_map[TREE_TYPE (t)] : TREE_TYPE (copy);
  TYPE_NAME (copy) = is_modified
		       ? get_new_identifier (copy, this->get_new_suffix ())
		       : TYPE_NAME (copy);
  // This is useful so that we go again through type layout
  TYPE_SIZE (copy) = is_modified ? NULL : TYPE_SIZE (copy);
  tree domain = TYPE_DOMAIN (t);
  if (domain)
    {
      tree copy_domain = copy_node (domain);
      tree min = TYPE_MIN_VALUE (domain);
      tree max = TYPE_MAX_VALUE (domain);
      TYPE_MIN_VALUE (copy_domain) = copy_node (min);
      TYPE_MAX_VALUE (copy_domain) = copy_node (max);
    }
  if (is_modified)
    layout_type (copy);
  TYPE_CACHED_VALUES_P (copy) = false;
  tree _t = tree_to_tree (t);
  tree tt = _t;
  while (TREE_TYPE (tt))
    {
      tt = TREE_TYPE (tt);
    };

  const bool points_to_record = TREE_CODE (tt) == RECORD_TYPE;
  if (!points_to_record)
    return;

  _reorg_map[t] = is_modified ? copy : _t;
}

void
type_reconstructor::_walk_POINTER_TYPE_pre (tree t)
{
  for_reference.push (t);
  set_is_not_modified_yet (t);

  tree _t = tree_to_tree (t);
  tree copy = build_variant_type_copy (_t);
  in_progress.push (copy);
}

void
type_reconstructor::_walk_POINTER_TYPE_post (tree t)
{
  tree t2 = for_reference.top ();
  gcc_assert (t2 == t);
  for_reference.pop ();
  tree copy = in_progress.top ();
  in_progress.pop ();

  bool is_modified = get_is_modified (t);

  copy = is_modified ? build_variant_type_copy (copy) : copy;
  TREE_TYPE (copy) = is_modified ? _reorg_map[TREE_TYPE (t)] : TREE_TYPE (copy);
  TYPE_NAME (copy) = is_modified
		       ? get_new_identifier (copy, this->get_new_suffix ())
		       : TYPE_NAME (copy);
  TYPE_CACHED_VALUES_P (copy) = false;

  tree _t = tree_to_tree (t);
  tree tt = _t;
  while (TREE_TYPE (tt))
    {
      tt = TREE_TYPE (tt);
    };
  const bool points_to_record = TREE_CODE (tt) == RECORD_TYPE;
  if (!points_to_record)
    return;

  _reorg_map[t] = is_modified ? copy : _t;
}

void
type_reconstructor::_walk_RECORD_TYPE_pre (tree t)
{
  const bool is_main_variant = TYPE_MAIN_VARIANT (t) == t;
  if (!is_main_variant)
    {
      tree main_variant = TYPE_MAIN_VARIANT (t);
      _walk_RECORD_TYPE_pre (main_variant);
      type_walker::_walk_RECORD_TYPE (main_variant);
      _walk_RECORD_TYPE_post (main_variant);
    }

  set_is_not_modified_yet (t);
  for_reference.push (t);
  // We don't know if we will modify this type t
  // So, let's make a copy.  Just in case.
  tree _t = tree_to_tree (t);
  tree copy = build_variant_type_copy (_t);
  in_progress.push (copy);
  field_list_stack.push (field_tuple_list_t ());
}

void
type_reconstructor::_walk_RECORD_TYPE_post (tree t)
{
  tree t2 = for_reference.top ();
  gcc_assert (t2 == t);
  for_reference.pop ();

  tree copy = in_progress.top ();
  in_progress.pop ();
  field_tuple_list_t field_tuple_list = field_list_stack.top ();
  field_list_stack.pop ();

  // So, here all the work has been done to make sure
  // that the fields produced a field_tuple_list_t
  // with old fields and pointers to new fields.
  // There might be NULL values if new fields are eliminated.
  // So, now we want to do a couple of things.
  // First, is we need to change the TYPE_FIELDS
  // of the copy
  bool is_modified = get_is_modified (t);
  tree prev_field = NULL;
  for (field_tuple_list_t::iterator i = field_tuple_list.begin (),
	e = field_tuple_list.end (); i != e; ++i)
    {
      field_tuple_t field_tuple = *i;
      tree modified_field = field_tuple.second;
      if (!modified_field)
	{
	  is_modified = true;
	  continue;
	}

      tree current_field = modified_field;
      if (!prev_field)
	{
	  TYPE_FIELDS (copy) = current_field;
	}
      else
	{
	  DECL_CHAIN (prev_field) = current_field;
	}
      prev_field = current_field;
    }

  // We only had one field
  if (!prev_field && is_modified)
    {
      TYPE_FIELDS (copy) = NULL;
    }

  const bool is_main_variant = TYPE_MAIN_VARIANT (t) == t;
  // We already must have done the main variant...
  if (!is_main_variant)
    {
      tree main = TYPE_MAIN_VARIANT (t);
      tree main_reorg = _reorg_map[main];
      tree copy_variant = build_variant_type_copy (main_reorg);
      TYPE_NAME (copy_variant)
	= get_new_identifier (copy, this->get_new_suffix ());
      TYPE_SIZE (copy_variant) = NULL;
      TYPE_MAIN_VARIANT (copy_variant) = main_reorg;
      TYPE_SIZE (main_reorg) = NULL;
      layout_type (copy_variant);
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
      tree _t = tree_to_tree (t);
      _reorg_map[t] = is_modified ? copy : _t;
    }

  tree record = _reorg_map[t];
  for (tree field = TYPE_FIELDS (record); field; field = DECL_CHAIN (field))
    {
      relayout_decl (field);
    }
}

void
type_reconstructor::_walk_field_pre (tree t)
{
  for_reference.push (t);
  // We don't know if we will rewrite the field
  // that we are working on.  So proactively, let's make
  // a copy.
  tree _t = tree_to_tree (t);
  tree copy = copy_node (_t);
  tree type_copy = build_variant_type_copy ((TREE_TYPE (_t)));
  TREE_TYPE (copy) = type_copy;
  // To communicate this field to the other methods,
  // let's put it in the "in_progress" stack.
  in_progress.push (copy);
}

void
type_reconstructor::_walk_field_post (tree t)
{
  tree t2 = for_reference.top ();
  gcc_assert (t2 == t);
  for_reference.pop ();

  // Let's get the copy we were working on.
  tree copy = in_progress.top ();
  // Let's put the stack in the same position...
  in_progress.pop ();

  // What record does this field belongs to?
  tree record = for_reference.top ();

  field_offsets_t field_offsets = _records[record];
  // What's the field offset?
  unsigned f_byte_offset = tree_to_uhwi (DECL_FIELD_OFFSET (t));
  unsigned f_bit_offset = tree_to_uhwi (DECL_FIELD_BIT_OFFSET (t));
  unsigned f_offset = 8 * f_byte_offset + f_bit_offset;

  const bool can_field_be_deleted
    = field_offsets.find (f_offset) != field_offsets.end ();
  if (can_field_be_deleted)
    mark_all_pointing_here_as_modified ();
  tree original_type = TREE_TYPE (t);
  const bool type_memoized = is_memoized (original_type);

  TREE_TYPE (copy)
    = type_memoized ? _reorg_map[original_type] : TREE_TYPE (copy);

  field_tuple_t tuple = std::make_pair (t, can_field_be_deleted ? NULL : copy);

  // Put the field into the vector
  field_tuple_list_t &field_tuple_list = field_list_stack.top ();
  field_tuple_list.push_back (tuple);
  const bool already_has_field = _reorg_fields.find (t) != _reorg_fields.end ();
  if (already_has_field)
    return;
  _reorg_fields[t] = std::make_pair (copy, can_field_be_deleted);
}

// Relayout parameters
void
expr_type_rewriter::_walk_PARM_DECL_post (tree t)
{
  tree temp = tree_to_tree (t);
  tree ttemp = TREE_TYPE (temp);
  type_stringifier stringifier;
  const char *name = stringifier.stringify (ttemp).c_str ();
  log ("relayout parameter declaration %s\n", name);
  const bool is_interesting = is_interesting_type (ttemp);
  if (!is_interesting)
    return;
  relayout_decl (temp);
}

// Update return types
void
expr_type_rewriter::_walk_FUNCTION_DECL_post (tree t)
{
  tree fn_type = TREE_TYPE (t);
  // This is saying that we cannot have indirect functions
  // such as the ones found in structs.
  gcc_assert (fn_type);
  tree ret_type = TREE_TYPE (fn_type);
  if (!ret_type)
    return;

  // WARNING: You cannot use is interesting here because you haven't
  // changed the return type
  // This is because the return type is not an expression.
  // Therefore it is awkward to do this in the expr-walker...
  // const bool is_interesting = is_interesting_type (ret_type);
  // Instead use the following map
  const bool is_interesting = _map.find (ret_type) != _map.end ();
  if (!is_interesting)
    return;

  tree r_t = _map[ret_type];
  TREE_TYPE (fn_type) = r_t;
}

// Rewrite MEM_REF operand 1
void
expr_type_rewriter::_walk_MEM_REF_post (tree e)
{
  tree op0 = TREE_OPERAND (e, 0);
  tree t2 = TREE_TYPE (op0);
  const bool in_map2 = _map.find (t2) != _map.end ();

  log ("trying out substituting expression in component_Ref directly\n");
  type_stringifier stringifier;
  log ("mem_ref doing weird things maybe %s\n",
       stringifier.stringify (t2).c_str ());
  if (in_map2)
    {
      log ("success\n");
      tree r_t = _map[t2];
      tree _e = tree_to_tree (op0);
      TREE_TYPE (_e) = r_t;
    }
  // The second operand is a pointer constant.
  // Its type specifying the type used for type based alias analysis
  tree op1 = TREE_OPERAND (e, 1);
  gcc_assert (TREE_CODE (op1) == INTEGER_CST);

  tree t = TREE_TYPE (op1);
  const bool already_rewritten = is_interesting_type (t);

  // This is where we do the transformation
  if (!already_rewritten)
    return;

  tree old_type = _imap[t];
  assert_is_type (old_type, POINTER_TYPE);
  tree old_base_type = TREE_TYPE (old_type);
  tree old_type_size_tree = TYPE_SIZE_UNIT (old_base_type);
  int old_type_size_int = tree_to_shwi (old_type_size_tree);

  tree reorg_type = t;
  assert_is_type (reorg_type, POINTER_TYPE);
  tree reorg_base_type = TREE_TYPE (reorg_type);
  tree reorg_type_size_tree = TYPE_SIZE_UNIT (reorg_base_type);
  int reorg_type_size_int = tree_to_shwi (reorg_type_size_tree);

  // Let's find out what is the previous offset
  int old_offset = tree_to_uhwi (op1);
  int remainder = old_offset % old_type_size_int;

  int new_offset
    = old_offset / old_type_size_int * reorg_type_size_int + remainder;

  tree new_offset_tree = build_int_cst (TREE_TYPE (op1), new_offset);
  tree _e = tree_to_tree (e);
  TREE_OPERAND (_e, 1) = new_offset_tree;
}

// TODO:
// Change name of this method...
bool
expr_type_rewriter::is_interesting_type (tree t)
{
  const bool in_imap = _imap.find (t) != _imap.end ();
  bool interesting = in_imap;
  if (!interesting)
    return false;

  tree const_possibly_copy = _imap[t];
  tree possibly_copy = tree_to_tree (const_possibly_copy);
  const bool is_copy = possibly_copy == t;
  interesting = !is_copy;
  if (!interesting)
    return false;

  // Let's just do a quick sanity check
  tree interesting_type = t;
  bool has_valid_suffix
    = strstr (type_stringifier::get_type_identifier (interesting_type).c_str (),
	      ".reorg");
  has_valid_suffix |= (bool)
    strstr (type_stringifier::get_type_identifier (interesting_type).c_str (),
	    ".reorder");
  gcc_assert (has_valid_suffix);
  return true;
}

/* Rewrite POINTER_DIFF expr.  */
void
expr_type_rewriter::handle_pointer_arithmetic_diff (gimple *s, tree op_0)
{
  // lhs = op0 - op1 // <-- we are here
  // <SNIP>
  // var = lhs / [ex] old_struct_size // <-- we want to be here
  //
  // Let's explore the uses of lhs.
  tree lhs = gimple_assign_lhs (s);

  tree reorg_type = TREE_TYPE (op_0);
  const enum tree_code code = TREE_CODE (reorg_type);
  const bool is_pointer = POINTER_TYPE == code;
  const bool is_array = ARRAY_TYPE == code;
  const bool is_valid_input = is_pointer != is_array;
  gcc_assert (is_valid_input);

  tree inner_reorg_type = TREE_TYPE (reorg_type);
  gcc_assert (inner_reorg_type);
  tree reorg_type_size_tree = TYPE_SIZE_UNIT (inner_reorg_type);
  int reorg_type_size_int = tree_to_shwi (reorg_type_size_tree);

  tree const_old_type = _imap[reorg_type];
  tree old_type = tree_to_tree (const_old_type);
  tree inner_old_type = TREE_TYPE (old_type);
  gcc_assert (old_type);
  tree old_type_size_tree = TYPE_SIZE_UNIT (inner_old_type);
  int old_type_size_int = tree_to_shwi (old_type_size_tree);

  gimple *stmt;
  imm_use_iterator iterator;
  FOR_EACH_IMM_USE_STMT (stmt, iterator, lhs)
    {
      // stmt is a use of lhs
      // gimple_expr_code is only valid for non-debug statements
      bool is_debug = is_gimple_debug (stmt);
      if (is_debug)
	continue;

      enum tree_code code = gimple_expr_code (stmt);
      bool is_exact_div = code == EXACT_DIV_EXPR;
      if (!is_exact_div)
	continue;

      tree divisor = gimple_op (stmt, 2);
      enum tree_code divisor_code = TREE_CODE (divisor);
      bool is_constant = divisor_code == INTEGER_CST;
      if (!is_constant)
	continue;

      int divisor_int = tree_to_shwi (divisor);
      bool is_same_size = divisor_int == old_type_size_int;
      if (!is_same_size)
	continue;

      tree new_integer_cst_tree
	= build_int_cst (TREE_TYPE (divisor), reorg_type_size_int);
      gimple_set_op (stmt, 2, new_integer_cst_tree);
    }
}

/* Rewrite pointer arithmetic for non constant cases.  */
void
expr_type_rewriter::handle_pointer_arithmetic_nonconstant (gimple *s, tree op_0,
							 tree op_1,
							 bool is_pointer_plus)
{
  if (!is_pointer_plus)
    {
      handle_pointer_arithmetic_diff (s, op_0);
      return;
    }
  //   _1 = _0 * 72
  //   <SNIP>
  //   _2 = _1 + CONSTANT;
  //   <SNIP>
  //   _3 = &array + _2;  < -- this is where we are
  tree new_type = TREE_TYPE (gimple_assign_lhs (s));

  gimple *def_for_variable = SSA_NAME_DEF_STMT (op_1);
  // It is possible that we are in a negation statement...
  // Example:
  //   _2 = _1 * 72;
  //   <SNIP>
  //   _3 = -_2;  < -- def_for_variable **might** be this stmt.
  //   <SNIP>
  //   _4 = &array + _3;
  // Let's find out how many operands we have
  unsigned num_operands = gimple_num_ops (def_for_variable);
  // Here operands is kind of a minomer.
  // operand 0 is the lhs
  // operand 1 is the rhs
  // I.e. lhs = (unary_operator) rhs;
  bool get_another_definition = num_operands == 2;
  tree possibly_not_needed
    = get_another_definition ? gimple_op (def_for_variable, 1) : NULL;
  def_for_variable = get_another_definition
		       ? SSA_NAME_DEF_STMT (possibly_not_needed)
		       : def_for_variable;

  // Example:
  //   _2 = _1 * 72; <-- Now we are here...
  //   <SNIP>
  //   _3 = -_2;
  //   <SNIP>
  //   _4 = &array + _3;

  enum gimple_code gcode = gimple_code (def_for_variable);
  switch (gcode)
    {
      // TODO: FIXME:
      // This is unsafe, waiting for the sizeof solution
    case GIMPLE_COND:
    case GIMPLE_CALL:
    case GIMPLE_ASSIGN:
      break;
    default:
      // TODO: FIXME:
      // Can other cases land here? If so,
      // shouldn't I use an assertion here to make sure we catch them
      // and understand what is happening?
      return;
      break;
    }
  enum tree_code code = gimple_expr_code (def_for_variable);
  const bool is_plus_expr = PLUS_EXPR == code;

  // op_0 is the variable
  // That means that the reorg_type is
  // But the truth is that op_0 might not have the correct type.
  // So the correct type is the one used for the LHS.
  // Which was obtained above.
  tree reorg_type_tree = new_type;
  tree reorg_inner_type = TREE_TYPE (reorg_type_tree);
  tree reorg_type_size_tree = TYPE_SIZE_UNIT (reorg_inner_type);
  int reorg_type_size_int = tree_to_shwi (reorg_type_size_tree);
  // That means that the old type is
  tree const_old_type_tree = _imap[reorg_type_tree];
  tree old_type_tree = tree_to_tree (const_old_type_tree);
  tree old_inner_type = TREE_TYPE (old_type_tree);
  tree old_type_size_tree = TYPE_SIZE_UNIT (old_inner_type);
  int old_type_size_int = tree_to_shwi (old_type_size_tree);

  if (is_plus_expr)
    {
      // If we are here it is because we are adding an offset.
      // It is usually whenever we do somehting like
      //   _2 = _1 + CONSTANT; <-- to change
      //   _3 = &array + _2;
      tree constant_plus = gimple_op (def_for_variable, 2);
      assert_is_type (constant_plus, INTEGER_CST);

      int old_integer_cst_int = tree_to_uhwi (constant_plus);
      int modulo = old_integer_cst_int % old_type_size_int;
      int new_integer_cst_int
	= old_integer_cst_int / old_type_size_int * reorg_type_size_int
	  + modulo;

      tree new_integer_cst_tree
	= build_int_cst (TREE_TYPE (constant_plus), new_integer_cst_int);
      gimple_set_op (def_for_variable, 2, new_integer_cst_tree);

      tree variable = gimple_op (def_for_variable, 1);
      def_for_variable = SSA_NAME_DEF_STMT (variable);
      num_operands = gimple_num_ops (def_for_variable);
      get_another_definition = num_operands == 2;
      def_for_variable = get_another_definition
			   ? SSA_NAME_DEF_STMT (gimple_op (def_for_variable, 1))
			   : def_for_variable;
      code = gimple_expr_code (def_for_variable);
    }

  if (code == MULT_EXPR)
    {
      tree op_1_earlier = gimple_assign_rhs2 (def_for_variable);

      // We should be able to just call the constant implementation
      // handle_pointer_arithmetic_constants (def_for_variable, op_0, op_1);
      // However...
      // these variables no longer hold the type needed for them to change
      // correctly so, let's do it from here...

      assert_is_type (op_1_earlier, INTEGER_CST);

      tree old_integer_cst_tree = op_1_earlier;
      int old_integer_cst_int = tree_to_uhwi (old_integer_cst_tree);

      int offset = old_integer_cst_int % old_type_size_int;
      int new_integer_cst_int
	= old_integer_cst_int / old_type_size_int * reorg_type_size_int
	  + offset;

      tree new_integer_cst_tree
	= build_int_cst (TREE_TYPE (old_integer_cst_tree), new_integer_cst_int);
      gimple_set_op (def_for_variable, 2, new_integer_cst_tree);
    }
}

void
expr_type_rewriter::handle_pointer_arithmetic_constants (gimple *s, tree p,
						       tree i)
{
  // So, because we have already changed the type
  // tree p will either be the original type
  // if we do not need to modify this expression
  // How do we know if we have an original type?
  // It is when we don't have a type in our map
  tree possibly_reorged_type = TREE_TYPE (p);
  bool is_interesting_case = is_interesting_type (possibly_reorged_type);
  if (!is_interesting_case)
    return;

  tree reorg_type = possibly_reorged_type; // this is the type of the variable
  tree original_type = _imap[reorg_type];
  // If we are here, that means that our type has the ".reorg" suffix
  // Let's add a sanity check
  bool has_suffix
    = strstr (type_stringifier::get_type_identifier (reorg_type).c_str (),
	      ".reorg");
  has_suffix |= (bool) strstr (
    type_stringifier::get_type_identifier (reorg_type).c_str (), ".reorder");
  bool is_valid_input = has_suffix;
  gcc_assert (is_valid_input);

  // We need to know what size is the previous original type
  tree inner_reorg_type = TREE_TYPE (reorg_type);
  tree inner_orig_type = TREE_TYPE (original_type);
  tree old_size_tree = TYPE_SIZE_UNIT (inner_orig_type);
  int old_size_int = tree_to_shwi (old_size_tree);
  tree new_size_tree = TYPE_SIZE_UNIT (inner_reorg_type);
  int new_size_int = tree_to_shwi (new_size_tree);
  tree old_integer_cst_tree = i;
  int old_integer_cst_int = tree_to_uhwi (old_integer_cst_tree);

  int offset = old_integer_cst_int % old_size_int;
  const bool is_modulo = offset == 0;
  is_valid_input = is_modulo;
  if (!is_valid_input) return;

  int new_integer_cst_int
    = old_integer_cst_int / old_size_int * new_size_int + offset;

  tree new_integer_cst_tree
    = build_int_cst (TREE_TYPE (old_integer_cst_tree), new_integer_cst_int);
  gimple_set_op (s, 2, new_integer_cst_tree);

  if (!is_valid_input)
    {
      if (dump_file)
	print_gimple_expr (dump_file, s, 0);
      log ("\n%d  = %d / %d * %d\n", new_integer_cst_int, old_integer_cst_int,
	   old_size_int, new_size_int);
    }
  gcc_assert (is_valid_input);
}

/* substitute types in post-order visit.  */
void
expr_type_rewriter::_walk_post (tree e)
{
  gcc_assert (e);
  tree t = TREE_TYPE (e);
  const bool in_map = _map.find (t) != _map.end ();
  if (!in_map)
    return;

  tree r_t = _map[t];
  tree _e = tree_to_tree (e);
  TREE_TYPE (_e) = r_t;
}

/* Rewrite Field.  */
void
expr_type_rewriter::_walk_COMPONENT_REF_post (tree e)
{
  gcc_assert (e);

  tree f = TREE_OPERAND (e, 1);
  // So, what we need is a map between this field and the new field
  const bool in_map = _map2.find (f) != _map2.end ();
  if (!in_map)
    return;

  std::pair<tree, bool> p = _map2[f];
  tree n_f = p.first;
  bool is_deleted = p.second;
  tree _e = tree_to_tree (e);
  TREE_OPERAND (_e, 1) = n_f;

  if (!is_deleted)
    return;

  _delete = _can_delete && true;
  log ("are we deleting? %s %s\n", _delete ? "t" : "f", is_deleted ? "t" : "f");
}

void
gimple_type_rewriter::_walk_pre_tree (tree e)
{
  // This is for local variables
  // and other declarations
  exprTypeRewriter.walk (e);
  bool _delete = exprTypeRewriter._delete;
  exprTypeRewriter._delete = false;
  // I don't think it is possible here (local variable delcarations and such);
  gcc_assert (!_delete);
  const bool is_interesting
    = exprTypeRewriter.is_interesting_type (TREE_TYPE (e));

  const bool is_var_decl = TREE_CODE (e) == VAR_DECL;
  const bool is_valid = is_interesting && is_var_decl;
  if (!is_valid)
    return;
  tree _e = tree_to_tree (e);
  relayout_decl (_e);
}

void
gimple_type_rewriter::_walk_pre_greturn (greturn *s)
{
  tree val = gimple_return_retval (s);
  if (!val)
    return;
  exprTypeRewriter.walk (val);
  bool _delete = exprTypeRewriter._delete;
  exprTypeRewriter._delete = false;
  // We can't probably have a write in a return statement.
  gcc_assert (!_delete);
}

/* Prepare operands for fixing pointer arithmetic.  */
void
gimple_type_rewriter::handle_pointer_arithmetic (gimple *s)
{
  const enum tree_code p = POINTER_PLUS_EXPR;
  const enum tree_code d = POINTER_DIFF_EXPR;
  const enum tree_code e = gimple_expr_code (s);
  const bool is_pointer_plus = p == e;
  const bool is_pointer_diff = d == e;
  bool is_valid_input = is_pointer_plus != is_pointer_diff;
  gcc_assert (is_valid_input);

  const enum gimple_rhs_class rhs_class = gimple_assign_rhs_class (s);
  is_valid_input = GIMPLE_BINARY_RHS == rhs_class;
  gcc_assert (is_valid_input);

  tree op_0 = gimple_assign_rhs1 (s);
  tree op_1 = gimple_assign_rhs2 (s);
  tree lhs = gimple_assign_lhs (s);
  tree op_0_t = TREE_TYPE (op_0);
  tree op_1_t = TREE_TYPE (op_1);
  tree lhs_t = TREE_TYPE (lhs);
  const bool is_op_0_t_interesting
    = exprTypeRewriter.is_interesting_type (op_0_t);
  const bool is_op_1_t_interesting
    = exprTypeRewriter.is_interesting_type (op_1_t);
  const bool is_lhs_t_interesting
    = exprTypeRewriter.is_interesting_type (lhs_t);
  bool is_interesting_case
    = is_op_0_t_interesting || is_op_1_t_interesting || is_lhs_t_interesting;
  if (!is_interesting_case)
    return;

  const enum tree_code op_1_code = TREE_CODE (op_1);
  const enum tree_code op_0_code = TREE_CODE (op_0);
  const bool is_op_0_icst = INTEGER_CST == op_0_code;
  const bool is_op_1_icst = INTEGER_CST == op_1_code;
  const bool is_constant_case = is_op_0_icst != is_op_1_icst;
  if (!is_constant_case)
    {
      exprTypeRewriter.handle_pointer_arithmetic_nonconstant (s, op_0, op_1,
							      is_pointer_plus);
      bool _delete = exprTypeRewriter._delete;
      exprTypeRewriter._delete = false;
      // probably no deletion in pointer arithmetic...
      gcc_assert (!_delete);
      return;
    }

  tree integer_constant = is_op_0_icst ? op_0 : op_1;
  tree maybe_pointer = is_op_0_icst ? op_1 : op_0;
  tree maybe_pointer_t = TREE_TYPE (maybe_pointer);
  assert_is_type (maybe_pointer_t, POINTER_TYPE);
  tree pointer_variable = maybe_pointer;

  exprTypeRewriter.handle_pointer_arithmetic_constants (s, pointer_variable,
							integer_constant);
  bool _delete = exprTypeRewriter._delete;
  exprTypeRewriter._delete = false;
  // probably no deletion in pointer arithmetic
  gcc_assert (!_delete);
}

void
gimple_type_rewriter::_walk_pre_gassign (gassign *s)
{
  const enum gimple_rhs_class code = gimple_assign_rhs_class (s);

  switch (code)
    {
    case GIMPLE_TERNARY_RHS:
      {
	tree rhs3 = gimple_assign_rhs3 (s);
	exprTypeRewriter.walk (rhs3);
      }
    /* fall-through */
    case GIMPLE_BINARY_RHS:
      {
	tree rhs2 = gimple_assign_rhs2 (s);
	exprTypeRewriter.walk (rhs2);
      }
    /* fall-through */
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
      {
	tree rhs1 = gimple_assign_rhs1 (s);
	exprTypeRewriter.walk (rhs1);
	tree lhs = gimple_assign_lhs (s);
	if (!lhs)
	  break;
	// Here is the only place where we likely can delete a statement.
	exprTypeRewriter.walk (lhs);
	bool _delete = exprTypeRewriter._delete;
	exprTypeRewriter._delete = false;
	if (_delete)
	  {
	    _deleted = true;
	  }
      }
      break;
    default:
      gcc_unreachable ();
      break;
    }

  const enum tree_code e_code = gimple_expr_code (s);
  switch (e_code)
    {
    case POINTER_PLUS_EXPR:
    case POINTER_DIFF_EXPR:
      log ("am i handling pointer arithmetic?\n");
      if (dump_file)
	print_gimple_stmt (dump_file, s, 0);
      log ("\n");
      handle_pointer_arithmetic (s);
      if (dump_file)
	print_gimple_stmt (dump_file, s, 0);
      log ("\n");
      break;
    default:
      break;
    }
}

void
gimple_type_rewriter::_rewrite_function_decl ()
{
  // NOTE: It seems we only need to rewrite the return type
  // for now...
  cgraph_node *node = NULL;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY (node)
    {
      node->get_untransformed_body ();
      tree fndecl = node->decl;
      gcc_assert (fndecl);
      exprTypeRewriter.walk (fndecl);
      tree decl = DECL_RESULT (fndecl);
      if (!decl)
	continue;

      exprTypeRewriter.walk (decl);
    }
}

void
gimple_type_rewriter::_walk_pre_gphi (gphi *s)
{
  unsigned n = gimple_phi_num_args (s);
  for (unsigned i = 0; i < n; i++)
    {
      tree a = gimple_phi_arg_def (s, i);
      exprTypeRewriter.walk (a);
    }
}
