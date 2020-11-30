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

/* Collect all types which point to a specific type set.  */
class specific_type_collector : public type_walker
{
public:
  /* C is the set of types that are to be looked for.  */
  specific_type_collector (hash_set<tree> *c2) : _collect_these_types2 (c2)
  {};

  /* Get final result of all types which point to types in C.  */
  hash_set<tree> get_set2 ();

private:
  hash_set<tree> *_collect_these_types2;

  /* Working set that holds final result.  */
  hash_set<tree> to_return2;

  /* Sets which reach current subtype.  */
  hash_set<tree> path2;

  /* Push or pop from path.  */
  virtual void _walk_ARRAY_TYPE_pre (tree t);
  virtual void _walk_ARRAY_TYPE_post (tree t);
  virtual void _walk_UNION_TYPE_pre (tree t);
  virtual void _walk_UNION_TYPE_post (tree t);
  virtual void _walk_POINTER_TYPE_pre (tree t);
  virtual void _walk_POINTER_TYPE_post (tree t);

  /* If in input, place all parent types in to_return.  */
  virtual void _walk_RECORD_TYPE_pre (tree t);
  virtual void _walk_RECORD_TYPE_post (tree t);
};

/* Map old RECORD_TYPE -> new RECORD_TYPE.  */
typedef hash_map<tree, tree> reorg_record_map2_t;

/* Map RECORD_TYPE -> (FIELD_DECL -> delete).  */
typedef hash_map<tree, std::pair<tree, bool> > reorg_field_map2_t;

/* Class used to create new types derived from types that have fields
 * that can be deleted.  */
class type_reconstructor : public type_walker
{
public:
  type_reconstructor (record_field_offset_map4_t &records, const char *suffix, reorg_record_map2_t &a, reorg_field_map2_t &b)
    : _records2(records), _suffix (suffix), in_progress2(vNULL), for_reference2(vNULL), field_list2_stack2 (vNULL), _reorg_map2(a), _reorg_fields2(b)
  {
    modified_map2 = new is_modified_map2_t;
  };
  ~type_reconstructor()
  {
    delete modified_map2;
  }

  /* Whether a type has already been modified.  */
  virtual bool is_memoized (tree t);

  /* Map RECORD_TYPE -> is_modified.  */
  typedef hash_map<tree, bool> is_modified_map2_t;

protected:
  // Which records can be modified.
  record_field_offset_map4_t& _records2;

  // The new suffix
  const char *_suffix;

  // Modifications to the current sub_type
  vec<tree> in_progress2;

  // Path to current subtype
  vec<tree> for_reference2;

  // OLD FIELD -> new FIELD
  typedef std::pair<tree, tree> field_tuple_t;

  // list of fields for new type
  typedef vec<field_tuple_t> field_tuple_list2_t;

  // to deal with nested structures we need to have a stack
  // of field_tuple_list_t
  typedef vec<field_tuple_list2_t> field_tuple_list2_stack2_t;

  const char *get_new_suffix ();

  // Which fields will be deleted.
  field_tuple_list2_stack2_t field_list2_stack2;

  // old RECORD_TYPE -> new RECORD_TYPE
  reorg_record_map2_t &_reorg_map2;

  // old FIELD_DECL -> new FIELD_DECL
  reorg_field_map2_t &_reorg_fields2;

  // old RECORD_TYPE -> is_modified
  is_modified_map2_t *modified_map2;

  // Keep track of which types may need to be modified
  // defaults to not modified.
  void set_is_not_modified_yet (tree);

  // Mark all types reaching here will need to be modified.
  void mark_all_pointing_here_as_modified ();

  // If the type has been modified.
  bool get_is_modified (tree);

private:
  // Compute new FIELD_DECL list.
  virtual void _walk_field_pre (tree);
  virtual void _walk_field_post (tree);

  // Compute new RECORD_TYPE.
  virtual void _walk_RECORD_TYPE_pre (tree);
  virtual void _walk_RECORD_TYPE_post (tree);

  // Compute new type which points to new record type.
  virtual void _walk_ARRAY_TYPE_pre (tree);
  virtual void _walk_ARRAY_TYPE_post (tree);
  virtual void _walk_POINTER_TYPE_pre (tree);
  virtual void _walk_POINTER_TYPE_post (tree);
};

/* Modify expressions to match the new types.
 * Substitute old types with new types.
 * Handle pointer arithmetic.
 * Delete statements if needed.
 */
class expr_type_rewriter : public expr_walker
{
public:
  expr_type_rewriter (reorg_record_map2_t &map, reorg_field_map2_t &map2,
		    bool can_delete)
    : _delete (false), _can_delete (can_delete), _map2(map), _fields2(map2)
  {
    /* Create an inverse map new RECORD_TYPE -> old RECORD_TYPE.  */
    for (auto i = map.begin (), e = map.end (); i != e; ++i)
      {
	tree original = (*i).first;
	tree modified = (*i).second;
	_imap2.put (modified, original);
      }
  };

  ~expr_type_rewriter()
  {
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
  bool _can_delete;

private:
  // Old RECORD_TYPE -> new RECORD_TYPE.
  reorg_record_map2_t& _map2;

  // Old FIELD_DECL -> new FIELD_DECL.
  reorg_field_map2_t& _fields2;

  // New RECORD_TYPE -> old RECORD_TYPE.
  hash_map<tree, tree> _imap2;

  void _walk_post (tree e);

  // Substitute types and create new offset.
  void _walk_MEM_REF_post (tree e);

  // Substitute fields referred.
  void _walk_COMPONENT_REF_post (tree e);

  // Relayout parameters which are rewritten.
  void _walk_PARM_DECL_post (tree e);

  // Substitute return type.
  void _walk_FUNCTION_DECL_post (tree e);
};

// Walk all gimple and substitute types.
class gimple_type_rewriter : public gimple_walker
{
public:
  gimple_type_rewriter (reorg_record_map2_t &map, reorg_field_map2_t &map2,
		      bool can_delete)
    : exprTypeRewriter (map, map2, can_delete)
  {};

  void _rewrite_function_decl ();

private:
  // Substitute types in expressions
  expr_type_rewriter exprTypeRewriter;

  // handle pointer arithmetic
  void handle_pointer_arithmetic (gimple *s);

  // rewrite types in these statements
  virtual void _walk_pre_gphi (gphi *);
  virtual void _walk_pre_tree (tree);
  virtual void _walk_pre_greturn (greturn *s);
  virtual void _walk_pre_gassign (gassign *s);
};

// Get a set of all types pointing to types in RECORD_FIELD_OFFSET_MAP.
void
get_all_types_pointing_to (record_field_offset_map4_t &record_field_offset_map,
			   tpartitions2_t casting,
			   hash_set<tree> &to_modify);

typedef std::pair<reorg_record_map2_t*, reorg_field_map2_t*> reorg_maps_t;

// Compute the replacement types.
reorg_maps_t
get_types_replacement (record_field_offset_map4_t &record_field_offset_map,
		       hash_set<tree> &to_modify,
		       reorg_record_map2_t &,
		       reorg_field_map2_t &);

// Substitute types.
void
substitute_types_in_program (reorg_record_map2_t &map,
			     reorg_field_map2_t &field_map, bool _delete);

tree
get_new_identifier (tree type, const char *suffix);

#endif /* GCC_IPA_DFE */
