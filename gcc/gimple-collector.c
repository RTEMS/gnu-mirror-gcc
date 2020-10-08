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
#include <set>
#include <string>
#include <map>

#include "collect-types.h"
#include "type-stringifier.hpp"

#include "type-collector.hpp"
#include "expr-walker.hpp"
#include "expr-collector.hpp"
#include "gimple-collector.hpp"

void
GimpleTypeCollector::_walk_pre(const_tree t)
{
  exprCollector.walk(t);
}

void
GimpleTypeCollector::_walk_pre(gassign *s)
{
  const_tree lhs = gimple_assign_lhs(s);
  exprCollector.walk(lhs);

  const enum gimple_rhs_class gclass = gimple_assign_rhs_class(s);
  switch (gclass)
  {
    case GIMPLE_TERNARY_RHS:
    {
      const_tree rhs = gimple_assign_rhs3(s);  
      exprCollector.walk(rhs);
    }
    /* fall-through */
    case GIMPLE_BINARY_RHS:
    {
      const_tree rhs = gimple_assign_rhs2(s);  
      exprCollector.walk(rhs);
    }
    /* fall-through */
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
    {
      const_tree rhs = gimple_assign_rhs1(s);  
      exprCollector.walk(rhs);
    }
    break;
    default:
    gcc_unreachable();
    break;
  }
}

void
GimpleTypeCollector::_walk_pre(greturn *s)
{
  const_tree retval = gimple_return_retval(s);
  if (!retval) return;

  exprCollector.walk(retval);
}

void
GimpleTypeCollector::_walk_pre(gcond *s)
{
  const_tree lhs = gimple_cond_lhs(s);
  exprCollector.walk(lhs);
  const_tree rhs = gimple_cond_rhs(s);
  exprCollector.walk(rhs);
}

void
GimpleTypeCollector::_walk_pre(gcall *s)
{
  unsigned n = gimple_call_num_args(s);
  for (unsigned i = 0; i < n; i++)
  {
    const_tree a = gimple_call_arg(s, i);
    exprCollector.walk(a);
  }

  const_tree lhs = gimple_call_lhs(s);
  if (!lhs) return;

  exprCollector.walk(lhs);
}

void
GimpleTypeCollector::print_collected()
{
  ptrset_t sets = get_pointer_set();
  sets.print_in_points_to_record();

}
