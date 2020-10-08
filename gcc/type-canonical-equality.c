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
#include "type-canonical-equality.hpp"
#include "type-stringifier.hpp"

bool
TypeCanonicalEquality::_equal(const_tree l, const_tree r)
{
  bool valid_inputs = l && r;
  if (!valid_inputs) return l == r;

  const_tree canonical_l = TYPE_CANONICAL(l);
  const_tree canonical_r = TYPE_CANONICAL(r);
  const bool can_compare_canonical = canonical_l && canonical_r;
  if (!can_compare_canonical) return TypeStructuralEquality::_equal(l, r);

  const bool different = canonical_l != canonical_r;
  const std::string n_l = TypeStringifier::get_type_identifier(l);
  const std::string n_r = TypeStringifier::get_type_identifier(r);
  if (different) return false;

  return TypeStructuralEqualityMainVariant::_equal(l, r);
}
