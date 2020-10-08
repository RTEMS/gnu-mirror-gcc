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
#include "expr-walker.hpp"
#include "types-inlines.h"

void
ExprWalker::walk(const_tree e)
{
  _walk_pre(e);
  _walk(e);
  _walk_post(e);
}

void
ExprWalker::_walk(const_tree e)
{
  gcc_assert(e);
  const enum tree_code code = TREE_CODE(e);
  switch (code)
  {
    case INTEGER_CST:
      walk_INTEGER_CST(e);
    break;
    case REAL_CST:
      walk_REAL_CST(e);
    break;
    case STRING_CST:
      walk_STRING_CST(e);
    break;
    case BIT_FIELD_REF:
      walk_BIT_FIELD_REF(e);
    break;
    case ARRAY_REF:
      walk_ARRAY_REF(e);
    break;
    case MEM_REF:
      walk_MEM_REF(e);
    break;
    case COMPONENT_REF:
      walk_COMPONENT_REF(e);
    break;
    case SSA_NAME:
      walk_SSA_NAME(e);
    break;
    case ADDR_EXPR:
      walk_ADDR_EXPR(e);
    break;
    case VIEW_CONVERT_EXPR:
      walk_VIEW_CONVERT_EXPR(e);
    break;
    case IMAGPART_EXPR:
      walk_IMAGPART_EXPR(e);
    break;
    case VAR_DECL:
      walk_VAR_DECL(e);
    break;
    case FIELD_DECL:
      walk_FIELD_DECL(e);
    break;
    case RESULT_DECL:
      walk_RESULT_DECL(e);
    break;
    case PARM_DECL:
      walk_PARM_DECL(e);
    break;
    case FUNCTION_DECL:
      walk_FUNCTION_DECL(e);
    break;
    case CONSTRUCTOR:
      walk_CONSTRUCTOR(e);
    break;
    case LE_EXPR:
      walk_LE_EXPR(e);
    break;
    case EQ_EXPR:
      walk_EQ_EXPR(e);
    break;
    case GT_EXPR:
      walk_GT_EXPR(e);
    break;
    default:
    {
      log("missing %s\n", get_tree_code_name(code));
      gcc_unreachable();
    }
    break;
  }
}

#define ExprWalkerFuncDef(code) \
void \
ExprWalker::walk_ ## code (const_tree e) \
{ \
  assert_is_type(e, code); \
  _walk_pre(e); \
  _walk_ ## code ## _pre (e); \
  _walk_ ## code (e); \
  _walk_ ## code ## _post (e); \
  _walk_post(e); \ 
}

ExprWalkerFuncDef(CONSTRUCTOR)
ExprWalkerFuncDef(INTEGER_CST)
ExprWalkerFuncDef(REAL_CST)
ExprWalkerFuncDef(STRING_CST)
ExprWalkerFuncDef(BIT_FIELD_REF)
ExprWalkerFuncDef(ARRAY_REF)
ExprWalkerFuncDef(MEM_REF)
ExprWalkerFuncDef(COMPONENT_REF)
ExprWalkerFuncDef(SSA_NAME)
ExprWalkerFuncDef(ADDR_EXPR)
ExprWalkerFuncDef(VIEW_CONVERT_EXPR)
ExprWalkerFuncDef(IMAGPART_EXPR)
ExprWalkerFuncDef(FIELD_DECL)
ExprWalkerFuncDef(VAR_DECL)
ExprWalkerFuncDef(RESULT_DECL)
ExprWalkerFuncDef(PARM_DECL)
ExprWalkerFuncDef(FUNCTION_DECL)
ExprWalkerFuncDef(LE_EXPR)
ExprWalkerFuncDef(EQ_EXPR)
ExprWalkerFuncDef(GT_EXPR)

void
ExprWalker::_walk_leaf(const_tree e, const enum tree_code c)
{
  assert_is_type(e, c);
}

void
ExprWalker::_walk_op_n(const_tree e, unsigned n)
{
  gcc_assert(e);
  const_tree op_n = TREE_OPERAND(e, n);
  gcc_assert(op_n);
  walk(op_n);
}

void
ExprWalker::_walk_op_0(const_tree e, const enum tree_code c)
{
  assert_is_type(e, c);
  _walk_op_n(e, 0);
}

void
ExprWalker::_walk_op_1(const_tree e, const enum tree_code c)
{
  assert_is_type(e, c);
  _walk_op_n(e, 0);
  _walk_op_n(e, 1);
}

void
ExprWalker::_walk_CONSTRUCTOR(const_tree e)
{
#ifdef FUZZ_MODE
  gcc_unreachable();
#endif
}

void
ExprWalker::_walk_LE_EXPR(const_tree e)
{
  _walk_op_1(e, LE_EXPR);
}

void
ExprWalker::_walk_EQ_EXPR(const_tree e)
{
  _walk_op_1(e, EQ_EXPR);
}

void
ExprWalker::_walk_GT_EXPR(const_tree e)
{
  _walk_op_1(e, GT_EXPR);
}


void
ExprWalker::_walk_INTEGER_CST(const_tree e)
{
  _walk_leaf(e, INTEGER_CST);
}

void
ExprWalker::_walk_REAL_CST(const_tree e)
{
  _walk_leaf(e, REAL_CST);
}

void
ExprWalker::_walk_STRING_CST(const_tree e)
{
  _walk_leaf(e, STRING_CST);
}

void
ExprWalker::_walk_BIT_FIELD_REF(const_tree e)
{
#ifdef FUZZ_MODE
  gcc_unreachable();
#endif
}

void
ExprWalker::_walk_ARRAY_REF(const_tree e)
{
  _walk_op_1(e, ARRAY_REF);
}

void
ExprWalker::_walk_MEM_REF(const_tree e)
{
  _walk_op_1(e, MEM_REF);
}

void
ExprWalker::_walk_COMPONENT_REF(const_tree e)
{
  _walk_op_1(e, COMPONENT_REF);
}

void
ExprWalker::_walk_SSA_NAME(const_tree e)
{
  _walk_leaf(e, SSA_NAME);
}

void
ExprWalker::_walk_ADDR_EXPR(const_tree e)
{
  _walk_op_0(e, ADDR_EXPR);
}

void
ExprWalker::_walk_VIEW_CONVERT_EXPR(const_tree e)
{
#ifdef FUZZ_MODE
  gcc_unreachable();
#endif
}

void
ExprWalker::_walk_IMAGPART_EXPR(const_tree e)
{
#ifdef FUZZ_MODE
  gcc_unreachable();
#endif
}

void
ExprWalker::_walk_FIELD_DECL(const_tree e)
{
  _walk_leaf(e, FIELD_DECL);
}

void
ExprWalker::_walk_VAR_DECL(const_tree e)
{
  _walk_leaf(e, VAR_DECL);
}

void
ExprWalker::_walk_RESULT_DECL(const_tree e)
{
  _walk_leaf(e, RESULT_DECL);
}

void
ExprWalker::_walk_PARM_DECL(const_tree e)
{
  _walk_leaf(e, PARM_DECL);
}

void
ExprWalker::_walk_FUNCTION_DECL(const_tree e)
{
  _walk_leaf(e, FUNCTION_DECL);
  for (tree parm = DECL_ARGUMENTS(e); parm; parm = DECL_CHAIN(parm))
  {
    walk(parm);
  }

}
