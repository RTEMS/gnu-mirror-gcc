#pragma once

#include <set>

class TypeWalker {
public:
  TypeWalker() {};
  void walk(const_tree t);

protected:
  typedef std::set<const_tree> tset_t;

protected:
  tset_t tset;

  void _walk(const_tree t);
  void _walk_wrapper(const_tree t);
  void _walk_record_or_union(const_tree t);
  virtual void _walk_function_or_method(const_tree t);
  virtual bool is_memoized(__attribute__((unused))const_tree t) { return false; };

#define TypeWalkerFuncDecl(code) \
  virtual void _walk_ ## code ## _pre(const_tree t) {}; \
  virtual void walk_ ## code (const_tree t); \
  virtual void _walk_ ## code (const_tree t); \
  virtual void _walk_ ## code ## _post(const_tree t) {}

  TypeWalkerFuncDecl(VOID_TYPE);
  TypeWalkerFuncDecl(INTEGER_TYPE);
  TypeWalkerFuncDecl(REAL_TYPE);
  TypeWalkerFuncDecl(FIXED_POINT_TYPE);
  TypeWalkerFuncDecl(COMPLEX_TYPE);
  TypeWalkerFuncDecl(ENUMERAL_TYPE);
  TypeWalkerFuncDecl(BOOLEAN_TYPE);
  TypeWalkerFuncDecl(OFFSET_TYPE);
  TypeWalkerFuncDecl(RECORD_TYPE);
  TypeWalkerFuncDecl(POINTER_TYPE);
  TypeWalkerFuncDecl(REFERENCE_TYPE);
  TypeWalkerFuncDecl(ARRAY_TYPE);
  TypeWalkerFuncDecl(UNION_TYPE);
  TypeWalkerFuncDecl(FUNCTION_TYPE);
  TypeWalkerFuncDecl(METHOD_TYPE);

  // These are not types...
  TypeWalkerFuncDecl(field);
  TypeWalkerFuncDecl(return);
  TypeWalkerFuncDecl(args);
  TypeWalkerFuncDecl(arg);
};

