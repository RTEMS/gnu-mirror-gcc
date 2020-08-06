/* IPA Dead Field Elimination
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

#ifndef GCC_IPA_DFE
#define GCC_IPA_DFE

#include "ipa-type-escape-analysis.h"
#include <map>
#include <set>
#include <vector>

/* Collect all types which point to a specific type set.  */
class SpecificTypeCollector : public TypeWalker
{
public:
  /* C is the set of types that are to be looked for.  */
  SpecificTypeCollector (std::set<const_tree> &c) : _collect_these_types (c)
  {};

  /* Get final result of all types which point to types in C.  */
  std::set<const_tree> get_set ();

private:
  /* _collect_these_types holds the input.  */
  const std::set<const_tree> &_collect_these_types;

  /* Working set that holds final result.  */
  std::set<const_tree> to_return;

  /* Sets which reach current subtype.  */
  std::set<const_tree> path;

  /* Push or pop from path.  */
  virtual void _walk_ARRAY_TYPE_pre (const_tree t);
  virtual void _walk_ARRAY_TYPE_post (const_tree t);
  virtual void _walk_UNION_TYPE_pre (const_tree t);
  virtual void _walk_UNION_TYPE_post (const_tree t);
  virtual void _walk_POINTER_TYPE_pre (const_tree t);
  virtual void _walk_POINTER_TYPE_post (const_tree t);

  /* If in input, place all parent types in to_return.  */
  virtual void _walk_RECORD_TYPE_pre (const_tree t);
  virtual void _walk_RECORD_TYPE_post (const_tree t);
};

/* Map old RECORD_TYPE -> new RECORD_TYPE.  */
typedef std::map<const_tree, tree> reorg_record_map_t;

/* Map RECORD_TYPE -> (FIELD_DECL -> delete).  */
typedef std::map<const_tree, std::pair<tree, bool> > reorg_field_map_t;

/* Class used to create new types derived from types that have fields
 * that can be deleted.  */
class TypeReconstructor : public TypeWalker
{
public:
  TypeReconstructor (record_field_offset_map_t records) : _records (records)
  {};

  /* Whether a type has already been modified.  */
  virtual bool is_memoized (const_tree t);

  // Final result for record map.
  reorg_record_map_t get_map ();

  /* Final result for field map.  */
  reorg_field_map_t get_field_map ();

  /* Map RECORD_TYPE -> is_modified.  */
  typedef std::map<const_tree, bool> is_modified_map_t;

private:

  // Modifications to the current sub_type
  std::stack<tree> in_progress;

  // Path to current subtype
  std::stack<const_tree> for_reference;

  // OLD FIELD -> new FIELD
  typedef std::pair<const_tree, tree> field_tuple_t;

  // list of fields for new type
  typedef std::vector<field_tuple_t> field_tuple_list_t;

  // to deal with nested structures we need to have a stack
  // of field_tuple_list_t
  typedef std::stack<field_tuple_list_t> field_tuple_list_stack_t;

  // Which records can be modified.
  record_field_offset_map_t _records;

  // Which fields will be deleted.
  field_tuple_list_stack_t field_list_stack;

  // old RECORD_TYPE -> new RECORD_TYPE
  reorg_record_map_t _reorg_map;

  // old FIELD_DECL -> new FIELD_DECL
  reorg_field_map_t _reorg_fields;

  // old RECORD_TYPE -> is_modified
  is_modified_map_t modified_map;

  // Keep track of which types may need to be modified
  // defaults to not modified.
  void set_is_not_modified_yet (const_tree);

  // Mark all types reaching here will need to be modified.
  void mark_all_pointing_here_as_modified ();

  // If the type has been modified.
  bool get_is_modified (const_tree);

  // Compute new FIELD_DECL list.
  virtual void _walk_field_pre (const_tree);
  virtual void _walk_field_post (const_tree);

  // Compute new RECORD_TYPE.
  virtual void _walk_RECORD_TYPE_pre (const_tree);
  virtual void _walk_RECORD_TYPE_post (const_tree);

  // Compute new type which points to new record type.
  virtual void _walk_ARRAY_TYPE_pre (const_tree);
  virtual void _walk_ARRAY_TYPE_post (const_tree);
  virtual void _walk_POINTER_TYPE_pre (const_tree);
  virtual void _walk_POINTER_TYPE_post (const_tree);
};

/* Modify expressions to match the new types.
 * Substitute old types with new types.
 * Handle pointer arithmetic.
 * Delete statements if needed.
 */
class ExprTypeRewriter : public ExprWalker
{
public:
  ExprTypeRewriter (reorg_record_map_t map, reorg_field_map_t map2)
    : _delete (false), _map (map), _map2 (map2)
  {
    /* Create an inverse map new RECORD_TYPE -> old RECORD_TYPE.  */
    for (reorg_record_map_t::iterator i = map.begin (), e = map.end (); i != e; ++i)
      {
	const_tree original = i->first;
	tree modified = i->second;
	_imap[modified] = original;
      }
  };

  // Handle pointer arithmetic with constants.
  void handle_pointer_arithmetic_constants (gimple *s, tree p, tree i);

  // Handle pointer POINTER_DIFF_EXPR.
  void handle_pointer_arithmetic_diff (gimple *s, tree p);

  // Handle POINTER_PLUS_EXPR.
  void handle_pointer_arithmetic_nonconstant (gimple *s, tree p, tree i, bool);

  // Find out if this type will be modified.
  bool is_interesting_type (tree);

  // Delete statement.
  bool delete_statement ();
  bool _delete;

private:
  // Old RECORD_TYPE -> new RECORD_TYPE.
  reorg_record_map_t _map;

  // Old FIELD_DECL -> new FIELD_DECL.
  reorg_field_map_t _map2;

  // New RECORD_TYPE -> old RECORD_TYPE.
  std::map<tree, const_tree> _imap;
  void _walk_post (const_tree e);

  // Substitute types and create new offset.
  void _walk_MEM_REF_post (const_tree e);

  // Substitute fields referred.
  void _walk_COMPONENT_REF_post (const_tree e);

  // Relayout parameters which are rewritten.
  void _walk_PARM_DECL_post (const_tree e);

  // Substitute return type.
  void _walk_FUNCTION_DECL_post (const_tree e);
};

// Walk all gimple and substitute types.
class GimpleTypeRewriter : public GimpleWalker
{
public:
  GimpleTypeRewriter (reorg_record_map_t map, reorg_field_map_t map2)
    : exprTypeRewriter (map, map2)
  {};

  void _rewrite_function_decl ();

private:
  // Substitute types in expressions
  ExprTypeRewriter exprTypeRewriter;

  // handle pointer arithmetic
  void handle_pointer_arithmetic (gimple *s);

  // rewrite types in these statements
  virtual void _walk_pre_gphi (gphi *);
  virtual void _walk_pre_tree (const_tree);
  virtual void _walk_pre_greturn (greturn *s);
  virtual void _walk_pre_gassign (gassign *s);
};

// Get a set of all types pointing to types in RECORD_FIELD_OFFSET_MAP.
std::set<const_tree>
get_all_types_pointing_to (record_field_offset_map_t record_field_offset_map,
			   tpartitions_t casting);

typedef std::pair<reorg_record_map_t, reorg_field_map_t> reorg_maps_t;

// Compute the replacement types.
reorg_maps_t
get_types_replacement (record_field_offset_map_t record_field_offset_map,
		       std::set<const_tree> to_modify);

// Substitute types.
void
substitute_types_in_program (reorg_record_map_t map,
			     reorg_field_map_t field_map);

#endif /* GCC_IPA_DFE */
