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

#include "types-inlines.h"
#include "gimple-accesser.hpp"


void
GimpleAccesser::_walk_pre(gassign *s)
{
  // There seems to be quite a bit of code duplication here...
  const enum gimple_rhs_class code = gimple_assign_rhs_class(s);
  switch (code)
  {
    case GIMPLE_TERNARY_RHS:
    {
      const_tree rhs3 = gimple_assign_rhs3(s);
      gcc_assert(rhs3);
      exprAccessor.update(rhs3, Read);
    }
    /* fall-through */
    case GIMPLE_BINARY_RHS:
    {
      const_tree rhs2 = gimple_assign_rhs2(s);
      gcc_assert(rhs2);
      exprAccessor.update(rhs2, Read);
    }
    /* fall-through */
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
    {
      const_tree rhs1 = gimple_assign_rhs1(s);
      exprAccessor.update(rhs1, Read);
      const_tree lhs = gimple_assign_lhs(s);
      if (!lhs) break;
      exprAccessor.update(lhs, Write);
      break;
    }
    default:
    gcc_unreachable();
    break;
  }
}

void
GimpleAccesser::_walk_pre(gcall *s)
{
  tree fndecl = gimple_call_fndecl(s);
  unsigned n = gimple_call_num_args(s);
  for (unsigned i = 0; i < n; i++)
  {
    const_tree a = gimple_call_arg(s, i);
    gcc_assert(a);
    exprAccessor.update(a, Read);
  }

  const_tree lhs = gimple_call_lhs(s);
  if (!lhs) return;
  exprAccessor.update(lhs, Write);
}

void
GimpleAccesser::_walk_pre(greturn *s)
{
  const_tree val = gimple_return_retval(s);
  if (!val) return;
  exprAccessor.update(val, Read);
}

void
GimpleAccesser::_walk_pre(gcond *s)
{
  const_tree lhs = gimple_cond_lhs(s);
  const_tree rhs = gimple_cond_rhs(s);
  gcc_assert(lhs && rhs);
  exprAccessor.update(lhs, Read);
  exprAccessor.update(rhs, Read);
}
