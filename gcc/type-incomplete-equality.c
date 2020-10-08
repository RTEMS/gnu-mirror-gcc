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
#include "type-incomplete-equality.hpp"
#include "type-stringifier.hpp"

bool
TypeIncompleteEquality::_equal(const_tree l, const_tree r)
{
  bool valid_inputs = l && r;
  if (!valid_inputs) return l == r;

  // if any of these are incomplete, then we can only compare using identifiers...
  const bool complete_l = is_complete(l);
  const bool complete_r = is_complete(r);
  bool can_compare_structurally = complete_l && complete_r;
  if (can_compare_structurally) return TypeStructuralEquality::_equal(l, r);

  const_tree m_l = TYPE_MAIN_VARIANT(l);
  const_tree m_r = TYPE_MAIN_VARIANT(r);
  gcc_assert(m_l && m_r);
  can_compare_structurally = m_l == m_r;
  if (can_compare_structurally) return true;

  const std::string n_l = TypeStringifier::get_type_identifier(m_l);
  const std::string n_r = TypeStringifier::get_type_identifier(m_r);
  return n_l.compare(n_r) == 0;
}
