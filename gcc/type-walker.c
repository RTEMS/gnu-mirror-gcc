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

#include "type-walker.hpp"
#include "types-inlines.h"

void
TypeWalker::walk(const_tree t)
{
  gcc_assert(t);
  this->tset.clear();
  this->_walk(t);
}

void
TypeWalker::_walk(const_tree type)
{
  if (!type) return;
  gcc_assert(type);

  // This is an optimization
  const bool _is_memoized = is_memoized(type);
  if (_is_memoized) return;

  // This is for correctness...
  const bool in_set = tset.find(type) != tset.end();
  if (in_set) return;

  tset.insert(type);
  const enum tree_code code = TREE_CODE(type);
  switch (code)
  {
    case VOID_TYPE:
      this->walk_VOID_TYPE(type);
    break;
    case INTEGER_TYPE:
      this->walk_INTEGER_TYPE(type);
    break;
    case REAL_TYPE:
      this->walk_REAL_TYPE(type);
    break;
    case FIXED_POINT_TYPE:
      this->walk_FIXED_POINT_TYPE(type);
    break;
    case COMPLEX_TYPE:
      this->walk_COMPLEX_TYPE(type);
    break;
    case ENUMERAL_TYPE:
      this->walk_ENUMERAL_TYPE(type);
    break;
    case BOOLEAN_TYPE:
      this->walk_BOOLEAN_TYPE(type);
    break;
    case OFFSET_TYPE:
      this->walk_OFFSET_TYPE(type);
    break;
    case RECORD_TYPE:
      this->walk_RECORD_TYPE(type);
    break;
    case POINTER_TYPE:
      this->walk_POINTER_TYPE(type);
    break;
    case REFERENCE_TYPE:
      this->walk_REFERENCE_TYPE(type);
    break;
    case ARRAY_TYPE:
      this->walk_ARRAY_TYPE(type);
    break;
    case UNION_TYPE:
      this->walk_UNION_TYPE(type);
    break;
    case FUNCTION_TYPE:
      this->walk_FUNCTION_TYPE(type);
    break;
    case METHOD_TYPE:
      this->walk_METHOD_TYPE(type);
    break;
    case QUAL_UNION_TYPE:
    case LANG_TYPE:
    default:
    {
      log("missing %s\n", get_tree_code_name(code));
      gcc_unreachable();
    }
    break;
  }

  tset.erase(type);
}


#define TypeWalkerFuncDef(code) \
void \
TypeWalker::walk_ ## code (const_tree t) \
{ \
  assert_is_type(t, code); \
  _walk_ ## code ## _pre(t); \
  _walk_ ## code (t); \
  _walk_ ## code ## _post(t); \
}

#define TypeWalkerFuncDefInternal(code) \
void TypeWalker::_walk_ ## code (__attribute__((unused)) const_tree t) {}

TypeWalkerFuncDef(VOID_TYPE)
TypeWalkerFuncDefInternal(VOID_TYPE)
TypeWalkerFuncDef(INTEGER_TYPE)
TypeWalkerFuncDefInternal(INTEGER_TYPE)
TypeWalkerFuncDef(REAL_TYPE)
TypeWalkerFuncDefInternal(REAL_TYPE)
TypeWalkerFuncDef(BOOLEAN_TYPE)
TypeWalkerFuncDefInternal(BOOLEAN_TYPE)
TypeWalkerFuncDef(OFFSET_TYPE)
TypeWalkerFuncDefInternal(OFFSET_TYPE)
TypeWalkerFuncDef(FIXED_POINT_TYPE)
TypeWalkerFuncDefInternal(FIXED_POINT_TYPE)
TypeWalkerFuncDef(COMPLEX_TYPE)
TypeWalkerFuncDefInternal(COMPLEX_TYPE)
TypeWalkerFuncDef(ENUMERAL_TYPE)
TypeWalkerFuncDefInternal(ENUMERAL_TYPE)

void
TypeWalker::_walk_wrapper(const_tree t)
{
  const_tree inner_type = TREE_TYPE(t);
  gcc_assert(inner_type);
  _walk(inner_type);
}

#define TypeWalkerFuncDefWrapper(code) \
void \
TypeWalker::_walk_ ## code (const_tree t) \
{ \
  _walk_wrapper(t); \
}

TypeWalkerFuncDef(POINTER_TYPE)
TypeWalkerFuncDefWrapper(POINTER_TYPE)
TypeWalkerFuncDefWrapper(REFERENCE_TYPE)
TypeWalkerFuncDef(REFERENCE_TYPE)
TypeWalkerFuncDef(ARRAY_TYPE)
TypeWalkerFuncDefWrapper(ARRAY_TYPE)

TypeWalkerFuncDef(RECORD_TYPE)

void
TypeWalker::_walk_RECORD_TYPE(const_tree t)
{
  _walk_record_or_union(t);
}

TypeWalkerFuncDef(UNION_TYPE)

void
TypeWalker::_walk_UNION_TYPE(const_tree t)
{
  _walk_record_or_union(t);
}

void
TypeWalker::_walk_record_or_union(const_tree t)
{
  for (tree field = TYPE_FIELDS(t); field; field = DECL_CHAIN(field))
  {
    gcc_assert(field);
    walk_field(field);
  }
}

void
TypeWalker::walk_field(const_tree t)
{
  _walk_field_pre(t);
  _walk_field(t);
  _walk_field_post(t);
}

void
TypeWalker::_walk_field(const_tree t)
{
  const_tree inner_type = TREE_TYPE(t);
  gcc_assert(inner_type);
  _walk(inner_type);
}

TypeWalkerFuncDef(FUNCTION_TYPE)

void
TypeWalker::_walk_FUNCTION_TYPE(const_tree t)
{
  _walk_function_or_method(t);
}

TypeWalkerFuncDef(METHOD_TYPE)

void
TypeWalker::_walk_METHOD_TYPE(const_tree t)
{
  _walk_function_or_method(t);
}

void
TypeWalker::_walk_function_or_method(const_tree t)
{
  const_tree ret_type = TREE_TYPE(t);
  walk_return(ret_type);
  walk_args(t);
}

void
TypeWalker::walk_return(const_tree t)
{
  _walk_return_pre(t);
  _walk_return(t);
  _walk_return_post(t);
}

void
TypeWalker::_walk_return(const_tree t)
{
  _walk(t);
}

void
TypeWalker::walk_args(const_tree t)
{
  _walk_args_pre(t);
  _walk_args(t);
  _walk_args_post(t);
}

void
TypeWalker::_walk_args(const_tree t)
{
  for (tree arg_node = TYPE_ARG_TYPES(t); NULL_TREE != arg_node; arg_node = TREE_CHAIN(arg_node))
  {
    const_tree arg_node_type = TREE_VALUE(arg_node);
    gcc_assert(arg_node_type);
    walk_arg(arg_node_type);
  }
}

void
TypeWalker::walk_arg(const_tree t)
{
  _walk_arg_pre(t);
  _walk_arg(t);
  _walk_arg_post(t);
}

void
TypeWalker::_walk_arg(const_tree t)
{
  _walk(t);
}
