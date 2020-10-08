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
#include "type-structural-main-variant.hpp"

bool
TypeStructuralEqualityMainVariant::_equal(const_tree l, const_tree r)
{
  bool valid_inputs = l && r;
  if (!valid_inputs) return l == r;

  const_tree mv_l = TYPE_MAIN_VARIANT(l);
  const_tree mv_r = TYPE_MAIN_VARIANT(r);
  const bool mv_equal = mv_l == mv_r;
  if (mv_equal) return mv_equal;

  return TypeStructuralEquality::_equal(l, r);
}
