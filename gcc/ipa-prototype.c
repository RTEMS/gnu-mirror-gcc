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
#include "ipa-prototype.h"
#include "type-collector.hpp"
#include "type-stringifier.hpp"
#include <map>
#include <vector>
#include "type-escaper.hpp"
#include "expr-escaper.hpp"
#include "gimple-walker.hpp"
#include "gimple-collector.hpp"
#include "gimple-escaper.hpp"
#include "type-structural-equality.hpp"
#include "type-structural-main-variant.hpp"
#include "type-incomplete-equality.hpp"

//#define OPTIMIZED
#define SANITY_CHECKS

typedef std::set<const_tree> undefset;

void
Reason::print() const
{
  log("e=%d g=%d p=%d r=%d c=%d v=%d u=%d i=%d\n", this->is_escaping(), this->global_is_visible, this->parameter_is_visible, this->return_is_visible, this->type_is_casted, this->type_is_volatile, this->type_is_in_union, this->is_indirect);
}

Reason
Reason::operator|(const Reason &other)
{
  Reason retval {}; 
  retval.global_is_visible = this->global_is_visible | other.global_is_visible;
  retval.parameter_is_visible = this->parameter_is_visible | other.parameter_is_visible;
  retval.return_is_visible = this->return_is_visible | other.return_is_visible;
  retval.type_is_casted = this->type_is_casted | other.type_is_casted;
  retval.type_is_volatile = this->type_is_volatile | other.type_is_volatile;
  retval.type_is_in_union = this->type_is_in_union | other.type_is_in_union;
  retval.is_indirect = this->is_indirect | other.is_indirect;
  return retval;
}

Reason&
Reason::operator|=(const Reason &other)
{
  this->global_is_visible |= other.global_is_visible;
  this->parameter_is_visible |= other.parameter_is_visible;
  this->return_is_visible |= other.return_is_visible;
  this->type_is_casted |= other.type_is_casted;
  this->type_is_volatile |= other.type_is_volatile;
  this->type_is_in_union |= other.type_is_in_union;
  this->is_indirect |= other.is_indirect;
  return *this;
}



static inline void
assert_type_is_in_ptrset(const_tree type, ptrset_t &types)
{
#ifdef SANITY_CHECKS
  gcc_assert(types.in_points_to_record(type));
#endif
}

static inline void
assert_type_is_in_universe(const_tree type, ptrset_t &types)
{
#ifdef SANITY_CHECKS
  gcc_assert(types.in_universe(type));
#endif
}

static bool
is_variable_escaping(varpool_node *vnode)
{
  gcc_assert(vnode);
  return vnode->externally_visible;
}

static void
place_escaping_types_in_set(ptrset_t &types, typemap &calc)
{
  for (auto i = calc.cbegin(), e = calc.cend(); i != e; ++i)
  {
    const_tree type = i->first;
    // We should have seen it before
    assert_type_is_in_universe(type, types);

    // We should only track interesting types
    // Types which are not in points_to_record are the ones
    // that are pointed to by records.
    // I think it is possible to prune them ahead of time...
    if (!types.in_points_to_record(type)) continue;

    const Reason reason = i->second;
    reason.is_escaping() ? types.escaping.insert(type) : types.non_escaping.insert(type);
  }
}

static void
sanity_check_escape_xor_not(ptrset_t &types)
{
  for (auto i = types.escaping.cbegin(), e = types.escaping.cend(); i != e; ++i)
  {
    for (auto j = types.non_escaping.cbegin(), f = types.non_escaping.cend(); j != f; ++j)
    {
       const_tree type_esc = *i;
       gcc_assert(type_esc);
       const_tree type_non = *j;
       gcc_assert(type_non);
       //const bool valid_sets = !eq_type_compare(type_esc, type_non);
       //if (valid_sets) continue;
       //log("comparing %s == %s\n", type_to_string(type_esc).c_str(), type_to_string(type_non).c_str());
       //TODO: Remove this comment once we have restricted the "repairing" of sets a bit more.
       //gcc_assert(valid_sets);
    }
  }
}

static void
sanity_check_escape_union_not_equals_ptrset(ptrset_t &types)
{
  typeset _union;
  for (auto i = types.escaping.cbegin(), e = types.escaping.cend(); i != e; ++i)
  {
    const_tree type = *i;
    _union.insert(type);
  }

  for (auto i = types.non_escaping.cbegin(), e = types.non_escaping.cend(); i != e; ++i)
  {
    const_tree type = *i;
    _union.insert(type);
  }


  for (auto i = types.points_to_record.cbegin(), e = types.points_to_record.cend(); i != e; ++i)
  {
    const_tree type = *i;
    const bool in_union = _union.find(type) != _union.end();
    if (in_union) continue;
    //log("this type was not found in union %s\n", type_to_string(type).c_str());
    //TODO: FIXME: This has to be enabled for the sanity check to work
    //But at the moment there's one type which isn't working correctly :(
    gcc_unreachable();
  }

}

static void
fix_escaping_types_in_set(ptrset_t &types)
{
  bool fixed_point_reached = false;
  TypeIncompleteEquality structuralEquality;
  TypeStringifier stringifier;
  do {
    std::vector<const_tree> fixes;
    fixed_point_reached = true;
    for (auto i = types.escaping.cbegin(), e = types.escaping.cend(); i != e; ++i)
    {
      for (auto j = types.non_escaping.cbegin(), f = types.non_escaping.cend(); j != f; ++j)
      {
       const_tree type_esc = *i;
       gcc_assert(type_esc);
       const_tree type_non = *j;
       gcc_assert(type_non);
       // There can be cases where incomplete types are marked as non-escaping
       // and complete types counter parts are marked as escaping.
       //const bool interesting_case = eq_type_compare(type_esc, type_non);
       //TODO: We are going to need a different type comparison because this one
       //fails to take into account the recursion...
       TypeStringifier stringifier;
       std::string type_esc_name = TypeStringifier::get_type_identifier(type_esc);
       std::string type_non_name = TypeStringifier::get_type_identifier(type_non);
       //std::string file("FILE");
       //bool i_care = type_esc_name.compare(file) == 0;
       //i_care &= type_non_name.compare(file) == 0;

       type_esc_name = stringifier.stringify(type_esc);
       type_non_name = stringifier.stringify(type_non);

       const bool equal = structuralEquality.equal(type_esc, type_non);
       if (!equal) continue;

       log("recalulating %s == %s\n", type_esc_name.c_str(), type_non_name.c_str());
       fixed_point_reached = false;
       // Add incomplete to escaping
       // delete incomplete from non_escaping
       // We shouldn't do that inside our iteration loop.
       fixes.push_back(type_non);
      }
    }

    for (auto i = fixes.cbegin(), e = fixes.cend(); i != e; ++i)
    {
      const_tree escaping_type = *i;
      types.escaping.insert(escaping_type);
      types.non_escaping.erase(escaping_type);
    }
  } while (!fixed_point_reached);
}

static void
print_escaping_types_in_set(ptrset_t &types)
{
  std::vector<const_tree> fixes;
  for (auto i = types.non_escaping.cbegin(), e = types.non_escaping.cend(); i != e; ++i)
  {
    const_tree type_non = *i;
    gcc_assert(type_non);
    const enum tree_code code = TREE_CODE(type_non);
    const bool is_function = FUNCTION_TYPE == code;
    // I just don't want to print out functions.
    if (is_function) continue;
    TypeStringifier stringifier;
    std::string name = stringifier.stringify(type_non);
    log("non_escaping: %s \n", name.c_str());
  }

}

static void
print_sequal_types(ptrset_t &types)
{
  std::vector<const_tree> fixes;
  TypeStructuralEquality structuralEquality;
  TypeStringifier stringifier;
  for (auto i = types.universe.cbegin(), e = types.universe.cend(); i != e; ++i)
  {
    for (auto j = types.universe.cbegin(), e = types.universe.cend(); j != e; ++j)
    {
      const_tree t_i = *i;
      const_tree t_j = *j;
      const bool eq = structuralEquality.equal(t_i, t_j);
      std::string n_i = stringifier.stringify(t_i);
      std::string n_j = stringifier.stringify(t_j);
      log("%s = %s == %s\n", eq ? "t" : "f", n_i.c_str(), n_j.c_str());
    }
  }
}


static unsigned int
iphw_execute()
{

  GimpleTypeCollector collector;
  collector.walk();
  ptrset_t types = collector.get_pointer_set();

  GimpleEscaper gimpleEscaper(types);
  gimpleEscaper.walk();

  typemap eacalc; // Escape Analysis Calculation
  // Intermediate results
  // Do not read escape analysis results from here
  //calculate_escaping_types(types, eacalc);
  //print_sequal_types(types);
  place_escaping_types_in_set(types, gimpleEscaper.exprEscaper.typeEscaper.calc);
  fix_escaping_types_in_set(types);
  // -fipa-protytpe -fdump-ipa-prototype
  print_escaping_types_in_set(types);
  sanity_check_escape_xor_not(types);
  sanity_check_escape_union_not_equals_ptrset(types);
  gcc_unreachable();
  return 0;
}

namespace {
const pass_data pass_data_ipa_prototype =
{
  SIMPLE_IPA_PASS,
  "prototype",
  OPTGROUP_NONE,
  TV_NONE,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  0,
  0,
};

class pass_ipa_prototype : public simple_ipa_opt_pass
{
public:
  pass_ipa_prototype (gcc::context *ctx)
    : simple_ipa_opt_pass(pass_data_ipa_prototype, ctx)
  {}

  virtual bool gate(function*) { return flag_ipa_prototype; }
  virtual unsigned execute (function*) { return iphw_execute(); }
};
} // anon namespace

simple_ipa_opt_pass*
make_pass_ipa_prototype (gcc::context *ctx)
{
  return new pass_ipa_prototype (ctx);
}
