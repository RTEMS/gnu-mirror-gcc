#pragma once

#include "type-walker.hpp"
#include "collect-types.h"
#include <map>
#include <stack>

class TypeCollector : public TypeWalker {
public:
  void collect(const_tree t);
  TypeCollector() {};
  ptrset_t get_pointer_set() { _sanity_check(); return ptrset; }
private:
  std::map<const_tree, bool> ptr;
  ptrset_t ptrset;
  void _sanity_check();
  void _collect_simple(const_tree t);
  virtual bool is_memoized(const_tree t);

  virtual void _walk_VOID_TYPE_pre(const_tree t);
  virtual void _walk_VOID_TYPE_post(const_tree t);
  virtual void _walk_INTEGER_TYPE_pre(const_tree t);
  virtual void _walk_INTEGER_TYPE_post(const_tree t);
  virtual void _walk_REAL_TYPE_pre(const_tree t);
  virtual void _walk_REAL_TYPE_post(const_tree t);
  virtual void _walk_FIXED_POINT_TYPE_pre(const_tree t);
  virtual void _walk_FIXED_POINT_TYPE_post(const_tree t);
  virtual void _walk_COMPLEX_TYPE_pre(const_tree t);
  virtual void _walk_COMPLEX_TYPE_post(const_tree t);
  virtual void _walk_ENUMERAL_TYPE_pre(const_tree t);
  virtual void _walk_ENUMERAL_TYPE_post(const_tree t);
  virtual void _walk_BOOLEAN_TYPE_pre(const_tree t);
  virtual void _walk_BOOLEAN_TYPE_post(const_tree t);
  virtual void _walk_ARRAY_TYPE_pre(const_tree t);
  virtual void _walk_ARRAY_TYPE_post(const_tree t);
  virtual void _walk_POINTER_TYPE_pre(const_tree t);
  virtual void _walk_POINTER_TYPE_post(const_tree t);
  virtual void _walk_REFERENCE_TYPE_pre(const_tree t);
  virtual void _walk_REFERENCE_TYPE_post(const_tree t);
  virtual void _walk_RECORD_TYPE_pre(const_tree t);
  virtual void _walk_RECORD_TYPE_post(const_tree t);
  virtual void _walk_UNION_TYPE_pre(const_tree t);
  virtual void _walk_UNION_TYPE_post(const_tree t);
  virtual void _walk_FUNCTION_TYPE_pre(const_tree t);
  virtual void _walk_FUNCTION_TYPE_post(const_tree t);
  virtual void _walk_METHOD_TYPE_pre(const_tree t);
  virtual void _walk_METHOD_TYPE_post(const_tree t);
};

