#pragma once

#include <set>
#include <map>
#include <stack>
#include <vector>

#include "type-walker.hpp"

class TypeReconstructor : public TypeWalker
{
public:
  typedef std::map<const_tree, tree> reorg_record_map_t;
  typedef std::map<const_tree, std::pair<tree, bool>> reorg_field_map_t;
  typedef std::map<const_tree, bool> is_modified_map_t;
  typedef std::set<unsigned> field_offsets_t;
  typedef std::map<const_tree, field_offsets_t> record_field_offset_map_t;
private:
  std::stack<tree> in_progress;
  std::stack<const_tree> for_reference;
  typedef std::pair<const_tree, tree> field_tuple_t;
  typedef std::vector<field_tuple_t> field_tuple_list_t;
  typedef std::stack<field_tuple_list_t> field_tuple_list_stack_t;
  record_field_offset_map_t _records;
  field_tuple_list_stack_t field_list_stack;
  reorg_record_map_t _reorg_map;
  reorg_field_map_t _reorg_fields;
  is_modified_map_t modified_map;
  void set_is_not_modified_yet(const_tree);
  void mark_all_pointing_here_as_modified();
  bool get_is_modified(const_tree);
  virtual void _walk_field_pre(const_tree);
  virtual void _walk_field_post(const_tree);
  virtual void _walk_RECORD_TYPE_pre(const_tree);
  virtual void _walk_RECORD_TYPE_post(const_tree);
  virtual void _walk_UNION_TYPE_pre(const_tree);
  virtual void _walk_UNION_TYPE_post(const_tree);
  virtual void _walk_ARRAY_TYPE_pre(const_tree);
  virtual void _walk_ARRAY_TYPE_post(const_tree);
  virtual void _walk_POINTER_TYPE_pre(const_tree);
  virtual void _walk_POINTER_TYPE_post(const_tree);
public:
  virtual bool is_memoized(const_tree t);
  TypeReconstructor(record_field_offset_map_t records) : _records(records) {};
  reorg_record_map_t get_map() { return _reorg_map; };
  reorg_field_map_t get_field_map() { return _reorg_fields; };
};
