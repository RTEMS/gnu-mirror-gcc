#pragma once

#include "type-walker.hpp"
#include <string>

class TypeStringifier : public TypeWalker
{
private:
  std::string _stringification;


  void _stringify_simple(const_tree t);
  void _stringify_aggregate_pre(const_tree t);
  void _stringify_aggregate_post(const_tree t);
  void _stringify_fm_pre(const_tree t);
  void _stringify_fm_post(const_tree t);

  virtual void _walk_VOID_TYPE_pre(const_tree t);
  virtual void _walk_INTEGER_TYPE_pre(const_tree t);
  virtual void _walk_REAL_TYPE_pre(const_tree t);
  virtual void _walk_FIXED_POINT_TYPE_pre(const_tree t);
  virtual void _walk_COMPLEX_TYPE_pre(const_tree t);
  virtual void _walk_BOOLEAN_TYPE_pre(const_tree t);
  virtual void _walk_OFFSET_TYPE_pre(const_tree t);
  virtual void _walk_POINTER_TYPE_post(const_tree t);
  virtual void _walk_REFERENCE_TYPE_post(const_tree t);
  virtual void _walk_ARRAY_TYPE_post(const_tree t);
  virtual void _walk_RECORD_TYPE_pre(const_tree t);
  virtual void _walk_RECORD_TYPE_post(const_tree t);
  virtual void _walk_UNION_TYPE_pre(const_tree t);
  virtual void _walk_UNION_TYPE_post(const_tree t);
  virtual void _walk_field_post(const_tree t);
  virtual void _walk_return_pre(const_tree t);
  virtual void _walk_return_post(const_tree t);
  virtual void _walk_args_pre(const_tree t);
  virtual void _walk_args_post(const_tree t);
  virtual void _walk_arg_post(const_tree t);
  virtual void _walk_METHOD_TYPE_pre(const_tree t);
  virtual void _walk_METHOD_TYPE_post(const_tree t);
  virtual void _walk_FUNCTION_TYPE_pre(const_tree t);
  virtual void _walk_FUNCTION_TYPE_post(const_tree t);
public:
  static std::string get_type_identifier(const_tree t);
  static std::string get_field_identifier(const_tree t);
  std::string stringify(const_tree t);
  TypeStringifier() {};
};

