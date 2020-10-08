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
#include "type-stringifier.hpp"
#include <set>

#include "collect-types.h"


void
points_to_record_sets_s::insert(const_tree type, bool in_points_to_record)
{
  gcc_assert(type);
  this->universe.insert(type);
  in_points_to_record ? this->points_to_record.insert(type) : this->complement.insert(type);
  const bool in_points_to_set = this->in_points_to_record(type);
  const bool in_complement = this->in_complement(type);
  const bool _xor = in_points_to_set != in_complement;
  // sanity check...
  gcc_assert(_xor);
}

bool
points_to_record_sets_s::in_universe(const_tree type) const
{
  gcc_assert(type);
  const bool seen_before = this->universe.find(type) != this->universe.end();
  return seen_before;
}

bool
points_to_record_sets_s::in_points_to_record(const_tree type) const
{
  gcc_assert(type);
  const bool seen_before = this->points_to_record.find(type) != this->points_to_record.end();
  return seen_before;
}

bool
points_to_record_sets_s::in_complement(const_tree type) const
{
  gcc_assert(type);
  const bool seen_before = this->complement.find(type) != this->complement.end();
  return seen_before;
}

void
points_to_record_sets_s::print_in_points_to_record() const
{
  TypeStringifier stringifier;
  for (auto i = this->points_to_record.cbegin(), e = this->points_to_record.cend(); i != e; ++i)
  {
    const_tree t = *i;
    gcc_assert(t);
    std::string name = stringifier.stringify(t);
    log("collected: %s\n", name.c_str());
  }
}
