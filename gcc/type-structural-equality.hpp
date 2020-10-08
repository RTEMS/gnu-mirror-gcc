#pragma once

#include <set>

class TypeStructuralEquality {
public:
  TypeStructuralEquality() {};
  bool equal(const_tree a, const_tree b);
protected:
  virtual bool _equal(const_tree a, const_tree b);
private:
  typedef std::set<const_tree> tset_t;

  tset_t set_l;
  tset_t set_r;
  bool _equal_code(const_tree a, const_tree b);
  bool _equal_wrapper(const_tree a, const_tree b);

#define TSE_FUNC_DECL(code) \
  virtual bool _walk_ ## code (const_tree l, const_tree r)
  TSE_FUNC_DECL(VOID_TYPE);
  TSE_FUNC_DECL(COMPLEX_TYPE);
  TSE_FUNC_DECL(INTEGER_TYPE);
  TSE_FUNC_DECL(REAL_TYPE);
  TSE_FUNC_DECL(FIXED_POINT_TYPE);
  TSE_FUNC_DECL(POINTER_TYPE);
  TSE_FUNC_DECL(ENUMERAL_TYPE);
  TSE_FUNC_DECL(BOOLEAN_TYPE);
  TSE_FUNC_DECL(OFFSET_TYPE);
  TSE_FUNC_DECL(RECORD_TYPE);
  TSE_FUNC_DECL(REFERENCE_TYPE);
  TSE_FUNC_DECL(ARRAY_TYPE);
  TSE_FUNC_DECL(UNION_TYPE);
  TSE_FUNC_DECL(FUNCTION_TYPE);
  TSE_FUNC_DECL(METHOD_TYPE);

  const_tree left;
};
