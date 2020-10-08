#pragma once

#include "ipa-prototype.h"
#include "expr-walker.hpp"
#include "type-escaper.hpp"
#include "collect-types.h"
#include "type-accessor.hpp"
#include <tuple>
#include <map>

constexpr unsigned Empty = 0x0u;
constexpr unsigned Read = 0x01u;
constexpr unsigned Write = 0x02u;

typedef std::map<const_tree, unsigned> field_access_map_t;
typedef std::map<const_tree, field_access_map_t> record_field_map_t;

class ExprAccessor : public ExprWalker
{
public:
  ExprAccessor() {};
  void update(const_tree e, unsigned a);
  void print_accesses();
  void add_all_fields_in_struct(const_tree t);
  record_field_map_t get_map() { return record_field_map; };
private:
  unsigned _access;
  record_field_map_t record_field_map;
  virtual void _walk_COMPONENT_REF_pre(const_tree e);
  virtual void _walk_pre(const_tree t);
};
