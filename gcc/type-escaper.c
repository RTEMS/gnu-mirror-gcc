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

#include "type-escaper.hpp"
#include "type-stringifier.hpp"

bool
TypeEscaper::is_memoized(const_tree t)
{
  const bool in_set = calc.find(t) != calc.end();
  if (!in_set) return false;

  const bool will_not_escape = !_reason.is_escaping();
  if (will_not_escape) return true;

  const bool already_escaping = in_set && calc[t].is_escaping();
  if (already_escaping) return true;

  return false;
}

static inline void
assert_type_is_in_universe(const_tree type, ptrset_t &types)
{
#ifdef SANITY_CHECKS
  gcc_assert(types.in_universe(type));
#endif
}

ptrset_t
TypeEscaper::get_sets()
{
  place_escaping_types_in_set();
  return _ptrset;
}

void
TypeEscaper::place_escaping_types_in_set()
{
  TypeStringifier stringifier;
  for (auto i = calc.cbegin(), e = calc.cend(); i != e; ++i)
  {
    const_tree type = i->first;
    // We should have seen it before
    assert_type_is_in_universe(type, _ptrset);

    // We should only track interesting types
    // Types which are not in points_to_record are the ones
    // that are pointed to by records.
    // I think it is possible to prune them ahead of time...
    if (!_ptrset.in_points_to_record(type)) continue;

    const Reason reason = i->second;
    std::string name = stringifier.stringify(type);
    reason.is_escaping() ? _ptrset.escaping.insert(type) : _ptrset.non_escaping.insert(type);
  }
}

void
TypeEscaper::update(const_tree t, Reason r)
{
  gcc_assert(t);
  _reason = r;
  walk(t);
}

void
TypeEscaper::update_single_level(const_tree t, Reason r)
{
  gcc_assert(t);
  const bool already_in_typemap = calc.find(t) != calc.end();
  already_in_typemap ? calc[t] |= r : calc[t] = r;
}

void
TypeEscaper::_update(const_tree t)
{
  gcc_assert(t);
  // assert type is in universe
  const bool already_in_typemap = calc.find(t) != calc.end();
  // Do we have to invalidate all types which point to a volatile type?
  // Or do we have to invalidate all types pointed to by a volatile type?
  // Or do we only invalidate all types which are volatile.
  // This is only the third option.
  const bool is_volatile = TYPE_VOLATILE(t);
  Reason _is_volatile;
  _is_volatile.type_is_volatile = is_volatile;
  Reason _inner = _reason | _is_volatile;
  _inner.type_is_casted = _inside_indirect_field > 0 ? false : _inner.type_is_casted;
  if (_inside_function > 0) _inner.type_is_casted = false;
  already_in_typemap ? calc[t] |= _inner : calc[t] = _inner;
}

void
TypeEscaper::_walk_ARRAY_TYPE_pre(const_tree t)
{
  _update(t);  
}

void
TypeEscaper::_walk_POINTER_TYPE_pre(const_tree t)
{
  _inside_indirect_field = _inside_field > 0 ? _inside_indirect_field + 1 : _inside_indirect_field;
  _update(t);
}

void
TypeEscaper::_walk_POINTER_TYPE_post(const_tree t)
{
  _inside_indirect_field = _inside_field > 0 ? _inside_indirect_field - 1 : _inside_indirect_field;
}

void
TypeEscaper::_walk_REFERENCE_TYPE_pre(const_tree t)
{
  _update(t);
}

void
TypeEscaper::_walk_RECORD_TYPE_pre(const_tree t)
{
  _update(t);
}

void
TypeEscaper::_walk_UNION_TYPE_pre(const_tree t)
{
  _inside_union++;
  bool is_escaping = _inside_union > 0;
  _update(t);
  // After us... so that we can see what is our previous value
  _reason.type_is_in_union |= is_escaping;
}

void
TypeEscaper::_walk_field_pre(const_tree t)
{
  _inside_field++;
}

void
TypeEscaper::_walk_field_post(const_tree t)
{
  _inside_field--;
}

void
TypeEscaper::_walk_UNION_TYPE_post(const_tree t)
{
  _inside_union--;
  Reason prev = calc[t];
  _update(t);
  _reason = prev;
}

void
TypeEscaper::_walk_FUNCTION_TYPE_pre(const_tree t)
{
  _inside_function++;
}

void
TypeEscaper::_walk_FUNCTION_TYPE_post(const_tree t)
{
  _inside_function--;
}
void
TypeEscaper::_walk_function_or_method(const_tree t)
{
}


void
TypeEscaper::_walk_FUNCTION_TYPE(const_tree t)
{
}

void
TypeEscaper::_walk_METHOD_TYPE(const_tree t)
{
}


void
TypeEscaper::_walk_METHOD_TYPE_pre(const_tree t)
{
}

void
TypeEscaper::print_reasons()
{
  TypeStringifier stringifier;
  for (auto i = calc.cbegin(), e = calc.cend(); i != e; ++i)
  {
    const_tree t = i->first;
    std::string name = stringifier.stringify(t);
    const bool in_universe = _ptrset.in_universe(t);
    Reason r = i->second;
    log("%s reason: ", name.c_str());
    r.print();
  }
}
