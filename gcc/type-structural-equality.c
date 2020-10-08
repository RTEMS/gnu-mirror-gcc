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
#include "type-structural-equality.hpp"
#include "type-stringifier.hpp"

bool
TypeStructuralEquality::equal(const_tree l, const_tree r)
{
  return _equal(l, r);
}

bool
TypeStructuralEquality::_equal(const_tree l, const_tree r)
{
  bool valid_inputs = l && r;
  if (!valid_inputs) return l == r;

  bool equal_codes = _equal_code(l, r);
  if (!equal_codes) return equal_codes;

  bool recurse_l = set_l.find(l) != set_l.end();
  bool recurse_r = set_r.find(r) != set_r.end();
  // TODO: Is this the case every time?
  bool recurse = recurse_l || recurse_r;
  if (recurse) return recurse;

  set_l.insert(l);
  set_r.insert(r);
  const enum tree_code code = TREE_CODE(l);
  bool equal_children = false;
  switch(code)
  {
#define TSE_CASE(code) \
    case code: \
      equal_children = _walk_ ## code (l, r); \
    break

    TSE_CASE(VOID_TYPE);
    TSE_CASE(INTEGER_TYPE);
    TSE_CASE(REAL_TYPE);
    TSE_CASE(FIXED_POINT_TYPE);
    TSE_CASE(COMPLEX_TYPE);
    TSE_CASE(ENUMERAL_TYPE);
    TSE_CASE(BOOLEAN_TYPE);
    TSE_CASE(OFFSET_TYPE);
    TSE_CASE(RECORD_TYPE);
    TSE_CASE(POINTER_TYPE);
    TSE_CASE(REFERENCE_TYPE);
    TSE_CASE(ARRAY_TYPE);
    TSE_CASE(UNION_TYPE);
    TSE_CASE(FUNCTION_TYPE);
    TSE_CASE(METHOD_TYPE);
    default:
    gcc_unreachable();
    break;
  }

  set_l.erase(l);
  set_r.erase(r);
  return equal_children;
  
}

bool
TypeStructuralEquality::_equal_code(const_tree l, const_tree r)
{
  const enum tree_code code_l = TREE_CODE(l);
  const enum tree_code code_r = TREE_CODE(r);
  const bool equal = code_l == code_r;
  return equal;
}

#define TSE_FUNC_DEF_SIMPLE(code) \
bool \
TypeStructuralEquality::_walk_ ## code (const_tree l, const_tree r) \
{ \
  return _equal_code(l, r); \
}

TSE_FUNC_DEF_SIMPLE(VOID_TYPE)
TSE_FUNC_DEF_SIMPLE(INTEGER_TYPE)
TSE_FUNC_DEF_SIMPLE(REAL_TYPE)
TSE_FUNC_DEF_SIMPLE(FIXED_POINT_TYPE)
TSE_FUNC_DEF_SIMPLE(ENUMERAL_TYPE)
TSE_FUNC_DEF_SIMPLE(BOOLEAN_TYPE)
TSE_FUNC_DEF_SIMPLE(OFFSET_TYPE)
TSE_FUNC_DEF_SIMPLE(COMPLEX_TYPE)

bool
TypeStructuralEquality::_equal_wrapper(const_tree l, const_tree r)
{
  const_tree inner_l = TREE_TYPE(l);
  const_tree inner_r = TREE_TYPE(r);
  return _equal(inner_l, inner_r);
}

#define TSE_FUNC_DEF_WRAPPER(code) \
bool \
TypeStructuralEquality::_walk_ ## code (const_tree l, const_tree r) \
{ \
  return _equal_wrapper(l, r); \
}

TSE_FUNC_DEF_WRAPPER(REFERENCE_TYPE)
TSE_FUNC_DEF_WRAPPER(ARRAY_TYPE)
TSE_FUNC_DEF_WRAPPER(POINTER_TYPE)

#define TSE_FUNC_DEF_CONTAINER(code) \
bool \
TypeStructuralEquality::_walk_ ## code (const_tree l, const_tree r) \
{ \
  const_tree field_l = TYPE_FIELDS(l); \
  const_tree field_r = TYPE_FIELDS(r); \
  bool efield_l = field_l; \
  bool efield_r = field_r; \
  bool still_equal = efield_l == efield_r; \
  if (!still_equal) return still_equal; \
	\
  while (field_l && field_r && still_equal) \
  { \
    const_tree tfield_l = TREE_TYPE(field_l); \
    const_tree tfield_r = TREE_TYPE(field_r); \
    still_equal &= _equal(tfield_l, tfield_r); \
    field_l = DECL_CHAIN(field_l); \
    field_r = DECL_CHAIN(field_r); \
    efield_l = field_l; \
    efield_r = field_r; \
    still_equal &= efield_l == efield_r; \
  } \
  return still_equal; \
}

TSE_FUNC_DEF_CONTAINER(RECORD_TYPE)
TSE_FUNC_DEF_CONTAINER(UNION_TYPE)

#define TSE_FUNC_DEF_FUNC(code) \
bool \
TypeStructuralEquality::_walk_ ## code (const_tree l, const_tree r) \
{ \
  const_tree tret_l = TREE_TYPE(l); \
  const_tree tret_r = TREE_TYPE(r); \
  bool still_equal = _equal(tret_l, tret_r); \
  if (!still_equal) return still_equal; \
  \
  const_tree arg_l = TYPE_ARG_TYPES(l); \
  const_tree arg_r = TYPE_ARG_TYPES(r); \
  bool earg_l = arg_l; \
  bool earg_r = arg_r; \
  still_equal &= earg_l == earg_r; \
  while (arg_l && arg_r && still_equal) \
  { \
    const_tree targ_l = TREE_VALUE(arg_l); \
    const_tree targ_r = TREE_VALUE(arg_r); \
    still_equal &= _equal(targ_l, targ_r); \
    arg_l = TREE_CHAIN(arg_l); \
    arg_r = TREE_CHAIN(arg_r); \
    earg_l = arg_l; \
    earg_r = arg_r; \
    still_equal &= earg_l == earg_r; \
  } \
  return still_equal; \
}

TSE_FUNC_DEF_FUNC(FUNCTION_TYPE)
TSE_FUNC_DEF_FUNC(METHOD_TYPE)



