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
#include "tree-ssa-alias.h"
#include "tree-ssanames.h"
#include "gimple.h"
#include "cfg.h" // needed for gimple-iterator.h
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include <stdbool.h>
#include "types-inlines.h"

#include "type-reconstructor.hpp"
#include "type-stringifier.hpp"
#include "stor-layout.h"


// TODO:
// I think it might be possible that we need to create
// new nodes as opposed to copying the nodes.
// I say this because I think the copy is a shallow copy 
// and it might be making things difficult if we don't
// know everything that is happening...
// so we might want to rewrite this class...
//
//
// TODO:
// Also, there's a bug in which the TYPE_MAIN_VARIANT is an old type...
// which is not a good thing.

void
TypeReconstructor::set_is_not_modified_yet(const_tree t)
{
  gcc_assert(t);
  const bool is_in_reorg_map = _reorg_map.find(t) != _reorg_map.end();
  modified_map[t] = false;
  if (is_in_reorg_map) mark_all_pointing_here_as_modified();

  const_tree tt = TREE_TYPE(t);
  if (!tt) return;

  const bool is_in_reorg_map_2 = _reorg_map.find(tt) != _reorg_map.end();
  log ("is in reorg_map_2 ? %s\n", is_in_reorg_map_2 ? "t" : "f");
  if (!is_in_reorg_map_2) return;

  tree type = _reorg_map[tt];
  const bool is_modified = strstr(TypeStringifier::get_type_identifier(type).c_str(), ".reorg");
  log("is modified %s\n", is_modified ? "t" : "f");
  if (!is_modified) return;

  mark_all_pointing_here_as_modified();

}

void
TypeReconstructor::mark_all_pointing_here_as_modified()
{
  for (auto i = modified_map.begin(), e = modified_map.end(); i != e; ++i)
  {
    const_tree type = i->first;
    i->second = true;
  }
}

bool
TypeReconstructor::get_is_modified(const_tree t)
{
  gcc_assert(t);
  const bool in_map = modified_map.find(t) != modified_map.end();
  gcc_assert(in_map);
  bool retval = modified_map[t];
  modified_map.erase(t);

  bool points_to_record = false;
  tree tt = (tree)t;
  while (TREE_TYPE(tt)) 
  {
     tt = TREE_TYPE(tt);
  }
  points_to_record = TREE_CODE(tt) == RECORD_TYPE;


  return retval && points_to_record;
}

bool
TypeReconstructor::is_memoized(const_tree t)
{
  const bool already_changed = _reorg_map.find(t) != _reorg_map.end();
  mark_all_pointing_here_as_modified();
  const bool has_typed_cached_values = TYPE_CACHED_VALUES_P (t);
  TypeStringifier stringifier;
  std::string name = stringifier.stringify(t);
  return already_changed;
}

static tree
get_new_identifier(const_tree type)
{
  const char* identifier = TypeStringifier::get_type_identifier(type).c_str();
  const bool is_new_type = strstr(identifier, "reorg");
  gcc_assert(!is_new_type);
  char *new_name;
  asprintf(&new_name, "%s.reorg", identifier);
  return get_identifier(new_name);
}

// Invariant for all _pre functions:
// _reorg_map[t] == NULL (due to memoization)
//
// Invariant for all _post functions:
// _reorg_map[TREE_TYPE(t)] != NULL
// unless TREE_TYPE(t) is not a type which points to a record
// a.k.a. no modifications
//
// To preserve invariant, we must include
// the following at the end of all _post functions:
// _reorg_map[t] = /* reorg type */
// 
// How information is passed?
// To further _post functions via stacks
//
// To previous _post functions via _reorg_map (and maybe others?)
//
//
void
TypeReconstructor::_walk_ARRAY_TYPE_pre(const_tree t)
{
  for_reference.push(t);
  set_is_not_modified_yet(t);

  tree copy = build_variant_type_copy((tree) t);
  tree domain = TYPE_DOMAIN(t);
  if (domain) {
    tree copy_domain = copy_node(domain);
    tree min = TYPE_MIN_VALUE(domain);
    tree max = TYPE_MAX_VALUE(domain);
    TYPE_MIN_VALUE(copy_domain) = copy_node(min);
    TYPE_MAX_VALUE(copy_domain) = copy_node(max);
  }
  in_progress.push(copy);

}

void
TypeReconstructor::_walk_ARRAY_TYPE_post(const_tree t)
{
  const_tree t2 = for_reference.top();
  gcc_assert(t2 == t);
  for_reference.pop();
  tree copy = in_progress.top();
  in_progress.pop();

  bool is_modified = get_is_modified(t);

  TREE_TYPE(copy) = build_variant_type_copy(TREE_TYPE(copy));
  copy = is_modified ? build_distinct_type_copy(copy) : copy;
  TREE_TYPE(copy) = is_modified ? _reorg_map[TREE_TYPE(t)] : TREE_TYPE(copy);
  TYPE_NAME(copy) = is_modified ? get_new_identifier(copy) : TYPE_NAME(copy);
  // This is useful so that we go again through type layout
  TYPE_SIZE(copy) = is_modified ? NULL : TYPE_SIZE(copy);
  tree domain = TYPE_DOMAIN(t);
  if (domain) {
    tree copy_domain = copy_node(domain);
    tree min = TYPE_MIN_VALUE(domain);
    tree max = TYPE_MAX_VALUE(domain);
    TYPE_MIN_VALUE(copy_domain) = copy_node(min);
    TYPE_MAX_VALUE(copy_domain) = copy_node(max);
  }
  TypeStringifier stringifier;
  //std::string name = stringifier.stringify(copy);
  log("are we going to crash is modified %s %s\n", is_modified ? "t" : "f", TypeStringifier::get_type_identifier(copy).c_str());
  if (is_modified) layout_type(copy);
  TYPE_CACHED_VALUES_P (copy) = false;
  //TYPE_CACHED_VALUES (copy) = TYPE_CACHED_VALUES(t);
  tree tt = (tree)t;
  while (TREE_TYPE(tt)) { tt = TREE_TYPE(tt); };
  
  const bool points_to_record = TREE_CODE(tt) == RECORD_TYPE;
  if (!points_to_record) return;

  _reorg_map[t] = is_modified ? copy : (tree)t;
}

void
TypeReconstructor::_walk_POINTER_TYPE_pre(const_tree t)
{
  for_reference.push(t);
  set_is_not_modified_yet(t);

  tree copy = build_variant_type_copy((tree) t);
  in_progress.push(copy);
}

void
TypeReconstructor::_walk_POINTER_TYPE_post(const_tree t)
{
  const_tree t2 = for_reference.top();
  gcc_assert(t2 == t);
  for_reference.pop();
  tree copy = in_progress.top();
  in_progress.pop();

  bool is_modified = get_is_modified(t);

  copy = is_modified ? build_variant_type_copy(copy) : copy;
  TREE_TYPE(copy) = is_modified ? _reorg_map[TREE_TYPE(t)] : TREE_TYPE(copy);
  TYPE_NAME(copy) = is_modified ? get_new_identifier(copy) : TYPE_NAME(copy);
  // This is useful so that we go again through type layout
  //TYPE_SIZE(copy) = is_modified ? NULL : TYPE_SIZE(copy);
  //if (is_modified) layout_type(copy);
  TYPE_CACHED_VALUES_P (copy) = false;
  //TYPE_CACHED_VALUES (copy) = TYPE_CACHED_VALUES(t);
  //Let's just make sure that we are pointing to a a struct...

  tree tt = (tree)t;
  while (TREE_TYPE(tt)) { tt = TREE_TYPE(tt); };
  const bool points_to_record = TREE_CODE(tt) == RECORD_TYPE;
  if (!points_to_record) return;
  
  _reorg_map[t] = is_modified ? copy : (tree)t;
}

void
TypeReconstructor::_walk_RECORD_TYPE_pre(const_tree t)
{
  const bool is_main_variant = TYPE_MAIN_VARIANT(t) == t;
  if (!is_main_variant) {
	  const_tree main_variant = TYPE_MAIN_VARIANT(t);
	  _walk_RECORD_TYPE_pre(main_variant);
	  TypeWalker::_walk_RECORD_TYPE(main_variant);
	  _walk_RECORD_TYPE_post(main_variant);
  }

  set_is_not_modified_yet(t);
  for_reference.push(t);
  // We don't know if we will modify this type t
  // So, let's make a copy. Just in case.
  tree copy = build_variant_type_copy((tree) t);
  in_progress.push(copy);
  field_list_stack.push( field_tuple_list_t() );
}

void
TypeReconstructor::_walk_RECORD_TYPE_post(const_tree t)
{
  const_tree t2 = for_reference.top();
  gcc_assert(t2 == t);
  for_reference.pop();

  tree copy = in_progress.top();
  in_progress.pop();
  field_tuple_list_t field_tuple_list = field_list_stack.top();
  field_list_stack.pop();

  // So, here all the work has been done to make sure
  // that the fields produced a field_tuple_list_t
  // with old fields and pointers to new fields.
  // There might be NULL values if new fields are eliminated.
  // So, now we want to do a couple of things.
  // First, is we need to change the TYPE_FIELDS
  // of the copy
  bool is_modified = get_is_modified(t);
  tree prev_field = NULL;
  for (auto i = field_tuple_list.cbegin(), e = field_tuple_list.cend(); i != e; ++i)
  {
    field_tuple_t field_tuple = *i;
    const_tree original_field = field_tuple.first;
    tree modified_field = field_tuple.second;
    if (!modified_field) {
      is_modified = true;
      continue;
    }

    tree current_field = modified_field;
    if (!prev_field) {
      TYPE_FIELDS(copy) = current_field;
    } else {
      DECL_CHAIN(prev_field) = current_field;
    }
    prev_field = current_field;
  }

  // We only had one field
  if (!prev_field && is_modified) {
	  log("deleting all fields for struct %s\n", TypeStringifier::get_type_identifier(copy).c_str());
	  TYPE_FIELDS(copy) = NULL;
  }


  const bool is_main_variant = TYPE_MAIN_VARIANT(t) == t;
  // We already must have done the main variant...
  if (!is_main_variant)
  {
     tree main = TYPE_MAIN_VARIANT(t);
     tree main_reorg = _reorg_map[main];
     TypeStringifier stringifier;
     std::string main_s = stringifier.stringify(main_reorg);
     log("is modified %s main variant reorged build variant type %s\n", is_modified ? "T" : "F", main_s.c_str());
     tree copy_variant = build_variant_type_copy(main_reorg);
     TYPE_NAME(copy_variant) = get_new_identifier(copy);
     TYPE_SIZE(copy_variant) = NULL;
     TYPE_MAIN_VARIANT(copy_variant) = main_reorg;
     TYPE_SIZE(main_reorg) = NULL;
     layout_type(copy_variant);
     _reorg_map[t] = copy_variant;
  } else {
  // Ok, so now that we have fixed the TYPE_FIELDS of the copy...
  // We need to call layout_type
    copy = is_modified ? build_distinct_type_copy(copy) : copy;
    TYPE_NAME(copy) = is_modified ? get_new_identifier(copy) : TYPE_NAME(copy);
    // This is useful so that we go again through type layout
    TYPE_SIZE(copy) = is_modified ? NULL : TYPE_SIZE(copy);
    TYPE_MAIN_VARIANT(copy) = is_modified ? copy : TYPE_MAIN_VARIANT(copy);
    tree main_variant = TYPE_MAIN_VARIANT(copy);
    TypeStringifier stringifier;
    std::string main_s = stringifier.stringify(main_variant);
    log("main variant reorged build distinct %s\n", main_s.c_str());
    if (is_modified) layout_type(copy);
    _reorg_map[t] = is_modified ? copy : (tree)t;
  }

  tree record = _reorg_map[t];
  for (tree field = TYPE_FIELDS(record); field; field = DECL_CHAIN(field))
  {
    relayout_decl(field);
  }

}

void
TypeReconstructor::_walk_UNION_TYPE_pre(const_tree t)
{
}

void
TypeReconstructor::_walk_UNION_TYPE_post(const_tree t)
{
}

void
TypeReconstructor::_walk_field_pre(const_tree t)
{
  for_reference.push(t);
  // We don't know if we will rewrite the field
  // that we are working on. So proactively, let's make
  // a copy
  tree copy = copy_node((tree) t);
  tree type_copy = build_variant_type_copy((tree)(TREE_TYPE(t)));
  TREE_TYPE(copy) = type_copy;
  // To communicate this field to the other methods,
  // let's put it in the "in_progress" stack.
  in_progress.push(copy);
}

void
TypeReconstructor::_walk_field_post(const_tree t)
{
  const_tree t2 = for_reference.top();
  gcc_assert(t2 == t);
  for_reference.pop();

  // Let's get the copy we were working on.
  tree copy = in_progress.top();
  // Let's put the stack in the same position...
  in_progress.pop();

  // What record does this field belongs to?
  const_tree record = for_reference.top();

  field_offsets_t field_offsets = _records[record];
  // What's the field offset?
  unsigned f_byte_offset = tree_to_uhwi(DECL_FIELD_OFFSET(t));
  unsigned f_bit_offset = tree_to_uhwi(DECL_FIELD_BIT_OFFSET(t));
  unsigned f_offset = 8 * f_byte_offset + f_bit_offset;

  const bool can_field_be_deleted = field_offsets.find(f_offset) != field_offsets.end();
  if (can_field_be_deleted) mark_all_pointing_here_as_modified();
  const_tree original_type = TREE_TYPE(t);
  const bool type_memoized = is_memoized(original_type);

  TREE_TYPE(copy) = type_memoized ? _reorg_map[original_type] : TREE_TYPE(copy);

  field_tuple_t tuple = std::make_pair(t, can_field_be_deleted ? NULL : copy);

  // Put the field into the vector
  field_tuple_list_t &field_tuple_list = field_list_stack.top();
  field_tuple_list.push_back(tuple);
  const bool already_has_field = _reorg_fields.find(t) != _reorg_fields.end();
  if (already_has_field) return;
  _reorg_fields[t] = std::make_pair(copy, can_field_be_deleted);
}
