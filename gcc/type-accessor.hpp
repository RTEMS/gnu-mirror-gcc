#pragma once

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

#include "expr-accessor.hpp"
#include "type-walker.hpp"

typedef std::map<const_tree, unsigned> field_access_map_t;
typedef std::map<const_tree, field_access_map_t> record_field_map_t;

class TypeAccessor : public TypeWalker
{
public:
  TypeAccessor(record_field_map_t &map) : _map(map) { };
private:
  record_field_map_t &_map;
  virtual void _walk_RECORD_TYPE_pre(const_tree t) final;
  void add_all_fields_in_struct(const_tree t);
};
