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
#include "cfg.h"
#include "gimple-iterator.h"
#include "gimple-ssa.h"

#include "type-stringifier.hpp"
#include "types-inlines.h"
#include <string>

std::string
TypeStringifier::stringify(const_tree t)
{
  _stringification.clear();
  gcc_assert(t);
  walk(t);
  return _stringification;
}

void
TypeStringifier::_walk_VOID_TYPE_pre(const_tree t)
{
  _stringify_simple(t);
}

void
TypeStringifier::_walk_INTEGER_TYPE_pre(const_tree t)
{
  _stringify_simple(t);
}

void
TypeStringifier::_walk_REAL_TYPE_pre(const_tree t)
{
  _stringify_simple(t);
}

void
TypeStringifier::_walk_FIXED_POINT_TYPE_pre(const_tree t)
{
  _stringify_simple(t);
}

void
TypeStringifier::_walk_COMPLEX_TYPE_pre(const_tree t)
{
  _stringify_simple(t);
}

void
TypeStringifier::_walk_OFFSET_TYPE_pre(const_tree t)
{
  _stringify_simple(t);
}

void
TypeStringifier::_walk_BOOLEAN_TYPE_pre(const_tree t)
{
  _stringify_simple(t);
}

void
TypeStringifier::_stringify_simple(const_tree t)
{
  gcc_assert(t);
  const enum tree_code code = TREE_CODE(t);
  this->_stringification += std::string(get_tree_code_name(code));
}

void
TypeStringifier::_walk_POINTER_TYPE_post(__attribute__((unused))const_tree t)
{
  this->_stringification += std::string("*");
}

void
TypeStringifier::_walk_ARRAY_TYPE_post(__attribute__((unused))const_tree t)
{
  this->_stringification += std::string("[]");
}

void
TypeStringifier::_walk_REFERENCE_TYPE_post(__attribute__((unused))const_tree t)
{
  this->_stringification += std::string("&");
}

void
TypeStringifier::_walk_UNION_TYPE_pre(const_tree t)
{
  this->_stringification += std::string(" union ");
  _stringify_aggregate_pre(t);
}

void
TypeStringifier::_walk_UNION_TYPE_post(const_tree t)
{
  _stringify_aggregate_post(t);
}

void
TypeStringifier::_walk_RECORD_TYPE_pre(const_tree t)
{
  this->_stringification += std::string(" record ");
  _stringify_aggregate_pre(t);
}

void
TypeStringifier::_walk_RECORD_TYPE_post(const_tree t)
{
  _stringify_aggregate_post(t);
}

void
TypeStringifier::_stringify_aggregate_pre(const_tree t)
{
  this->_stringification += TypeStringifier::get_type_identifier(t) + std::string(" {");
}

void
TypeStringifier::_stringify_aggregate_post(__attribute__((unused))const_tree t)
{
  this->_stringification += std::string("}");
}

void
TypeStringifier::_walk_field_post(const_tree t)
{
  this->_stringification += std::string(" ") + TypeStringifier::get_field_identifier(t) + std::string(";");
}

void
TypeStringifier::_walk_METHOD_TYPE_pre(const_tree t)
{
  _stringify_fm_pre(t);
}

void
TypeStringifier::_walk_METHOD_TYPE_post(const_tree t)
{
  _stringify_fm_post(t);
}

void
TypeStringifier::_walk_FUNCTION_TYPE_pre(const_tree t)
{
  _stringify_fm_pre(t);
}

void
TypeStringifier::_walk_FUNCTION_TYPE_post(const_tree t)
{
  _stringify_fm_post(t);
}

void
TypeStringifier::_stringify_fm_pre(__attribute__((unused)) const_tree t)
{
  this->_stringification += std::string("function { ");
}

void
TypeStringifier::_stringify_fm_post(__attribute__((unused))const_tree t)
{
  this->_stringification += std::string("}");
}

void
TypeStringifier::_walk_return_pre(__attribute__((unused)) const_tree t)
{
  this->_stringification += std::string("(");
}

void
TypeStringifier::_walk_return_post(__attribute__((unused)) const_tree t)
{
  this->_stringification += std::string(")");
}

void
TypeStringifier::_walk_args_pre(__attribute__((unused)) const_tree t)
{
  this->_stringification += std::string("(");
}

void
TypeStringifier::_walk_args_post(__attribute__((unused)) const_tree t)
{
  this->_stringification += std::string(")");
}

void
TypeStringifier::_walk_arg_post(__attribute__((unused)) const_tree t)
{
  this->_stringification += std::string(", ");
}

std::string
TypeStringifier::get_type_identifier(const_tree t)
{
  tree name = TYPE_NAME(t);
  const bool no_name = NULL_TREE == name;
  if (no_name) return std::string("");

  const enum tree_code name_code = TREE_CODE(name);
  const bool is_name_type_decl = TYPE_DECL == name_code;
  name = is_name_type_decl ? DECL_NAME(name) : name;
  const char* identifier_ptr = IDENTIFIER_POINTER(name);
  gcc_assert(identifier_ptr);
  return std::string(identifier_ptr);
}

std::string
TypeStringifier::get_field_identifier(const_tree t)
{
  assert_is_type(t, FIELD_DECL);
  const_tree decl_name = DECL_NAME(t);
  if (!decl_name) return std::string("");

  const char* identifier = IDENTIFIER_POINTER(decl_name);
  return std::string(identifier);
}
