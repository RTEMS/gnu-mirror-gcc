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

#include "type-collector.hpp"
#include "type-stringifier.hpp"
#include "types-inlines.h"

void
TypeCollector::collect(const_tree t)
{
  const bool in_set = ptrset.in_universe(t);
  // memoization...
  if (in_set) return;
  gcc_assert(t);

  if (!ptr.empty())
  {
    TypeStringifier stringifier;
    std::string in_name = stringifier.stringify(t);
    gcc_unreachable();
  }
  walk(t);
}

void
TypeCollector::_sanity_check()
{
  for (auto i = ptrset.points_to_record.cbegin(), e = ptrset.points_to_record.cend(); i != e; ++i)
  {
    for (auto j = ptrset.complement.cbegin(), f = ptrset.complement.cend(); j != f; ++j)
    {
       const_tree type_ptr = *i;
       gcc_assert(type_ptr);
       const_tree type_com = *j;
       gcc_assert(type_com);
       const bool valid_sets = type_ptr != type_com;
       if (valid_sets) continue;
       // Normally, we want a stronger type comparison
       // that is not just the pointer address
       // but this is the first sanity check and then we will need to determine
       // the stronger type comparison.
       // But first we will need to fix the types...
       TypeStringifier stringifier;
       std::string name_ptr = stringifier.stringify(type_ptr);
       std::string name_com = stringifier.stringify(type_com);
       log("%p %s == %p %s\n", type_ptr, name_ptr.c_str(), type_com, name_com.c_str());
       gcc_unreachable();
    }
  }
}

bool
TypeCollector::is_memoized(const_tree t)
{
  const bool in_set = ptrset.in_universe(t);
  if (!in_set) return false;

  const bool points_to_record = ptrset.in_points_to_record(t);
  for (auto i = ptr.begin(), e = ptr.end(); i != e; ++i)
  {
    i->second |= points_to_record;
  }
  return true;
}

void
TypeCollector::_walk_VOID_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_VOID_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_INTEGER_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_INTEGER_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_REAL_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_REAL_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_FIXED_POINT_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_FIXED_POINT_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_COMPLEX_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_COMPLEX_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_ENUMERAL_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_ENUMERAL_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_BOOLEAN_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_BOOLEAN_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_collect_simple(const_tree t)
{
  ptrset.insert(t, ptr[t]);
  ptr.erase(t);
}

void
TypeCollector::_walk_ARRAY_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_ARRAY_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_POINTER_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_POINTER_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_REFERENCE_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_REFERENCE_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_RECORD_TYPE_post(const_tree t)
{
  // All in ptr point to record
  for (auto i = ptr.begin(), e = ptr.end(); i != e; ++i)
  {
    i->second = true;
  }
  _collect_simple(t);
}

void
TypeCollector::_walk_RECORD_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_UNION_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_UNION_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_FUNCTION_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_FUNCTION_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}

void
TypeCollector::_walk_METHOD_TYPE_post(const_tree t)
{
  _collect_simple(t);
}

void
TypeCollector::_walk_METHOD_TYPE_pre(const_tree t)
{
  ptr[t] = false;
}
