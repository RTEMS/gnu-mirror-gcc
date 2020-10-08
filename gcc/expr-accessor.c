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
#include "type-stringifier.hpp"
#include "expr-accessor.hpp"
#include "expr-walker.hpp"
#include "type-accessor.hpp"

void
ExprAccessor::add_all_fields_in_struct(const_tree t)
{
  // Inefficient
  TypeAccessor typeAccessor(record_field_map);
  typeAccessor.walk(t);
}

void
ExprAccessor::_walk_pre(const_tree e)
{
  const_tree t = TREE_TYPE(e);
  add_all_fields_in_struct(t);
}

void
ExprAccessor::update(const_tree e, unsigned access)
{
  _access = access;
  walk(e);
}

void
ExprAccessor::_walk_COMPONENT_REF_pre(const_tree e)
{
  assert_is_type(e, COMPONENT_REF);
  const_tree op0 = TREE_OPERAND(e, 0);
  gcc_assert(op0);
  const_tree op0_t = TREE_TYPE(op0);
  gcc_assert(op0_t);
  // op0_t can either be a RECORD_TYPE or a UNION_TYPE
  const enum tree_code code = TREE_CODE(op0_t);
  const bool is_record = RECORD_TYPE == code;
  const bool is_union = UNION_TYPE == code;
  const bool valid = is_record != is_union;
  gcc_assert(valid);
 
  const_tree op1 = TREE_OPERAND(e, 1);
  assert_is_type(op1, FIELD_DECL);
  const bool record_already_in_map = record_field_map.find(op0_t) != record_field_map.end();
  field_access_map_t field_map; 
  field_map = record_already_in_map ? record_field_map[op0_t] : field_map;
  const bool field_already_in_map = field_map.find(op1) != field_map.end();
  unsigned prev_access = field_already_in_map ? field_map[op1] : Empty;

  prev_access |= _access;
  field_map[op1] = prev_access;
  add_all_fields_in_struct(op0_t);
  record_field_map[op0_t] = field_map;
}

void
ExprAccessor::print_accesses()
{
  for (auto i = record_field_map.cbegin(), e = record_field_map.cend(); i != e; ++i)
  {
    const_tree record = i->first;
    field_access_map_t field_map = i->second;
    for (auto j = field_map.cbegin(), f = field_map.cend(); j != f; ++j)
    {
      const_tree field = j->first;
      const std::string name_r = TypeStringifier::get_type_identifier(record);
      const std::string name_f = TypeStringifier::get_field_identifier(field);
      unsigned access = j->second;
      log("%s.%s = 0x%04x\n", name_r.c_str(), name_f.c_str(), access);
    }
  }
}
