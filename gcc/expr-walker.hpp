#pragma once

#include "types-inlines.h"


class ExprWalker 
{
public:
  ExprWalker() {};
  void walk(const_tree e);
private:
  virtual void _walk_pre(__attribute__((unused)) const_tree e) {};
  void _walk(const_tree e);
  virtual void _walk_post(__attribute__((unused)) const_tree e) {};
  inline void _walk_leaf(const_tree e, const enum tree_code c);
  inline void _walk_op_n(const_tree e, unsigned n);
  inline void _walk_op_0(const_tree e, const enum tree_code c);
  inline void _walk_op_1(const_tree e, const enum tree_code c);

#define ExprWalkerFuncDecl(code) \
  virtual void _walk_ ## code ## _pre(__attribute__((unused)) const_tree e) {}; \
  void walk_ ## code (const_tree e); \
  void _walk_ ## code (const_tree e); \
  virtual void _walk_ ## code ## _post(__attribute__((unused)) const_tree e) {}

  ExprWalkerFuncDecl(CONSTRUCTOR);
  ExprWalkerFuncDecl(INTEGER_CST);
  ExprWalkerFuncDecl(REAL_CST);
  ExprWalkerFuncDecl(STRING_CST);
  ExprWalkerFuncDecl(BIT_FIELD_REF);
  ExprWalkerFuncDecl(ARRAY_REF);
  ExprWalkerFuncDecl(MEM_REF);
  ExprWalkerFuncDecl(COMPONENT_REF);
  ExprWalkerFuncDecl(SSA_NAME);
  ExprWalkerFuncDecl(ADDR_EXPR);
  ExprWalkerFuncDecl(VIEW_CONVERT_EXPR);
  ExprWalkerFuncDecl(IMAGPART_EXPR);
  ExprWalkerFuncDecl(FIELD_DECL);
  ExprWalkerFuncDecl(VAR_DECL);
  ExprWalkerFuncDecl(RESULT_DECL);
  ExprWalkerFuncDecl(PARM_DECL);
  ExprWalkerFuncDecl(FUNCTION_DECL);
  ExprWalkerFuncDecl(LE_EXPR);
  ExprWalkerFuncDecl(EQ_EXPR);
  ExprWalkerFuncDecl(GT_EXPR);
};

