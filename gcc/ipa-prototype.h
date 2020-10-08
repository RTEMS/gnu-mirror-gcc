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

#include <map>

struct Reason {
  inline bool is_escaping() const {
    return this->global_is_visible 
	    || this->parameter_is_visible
	    || this->return_is_visible
	    || this->type_is_casted
	    || this->type_is_volatile
	    || this->type_is_in_union
	    || this->is_indirect;
  }
  bool global_is_visible : 1;
  bool parameter_is_visible : 1;
  bool return_is_visible : 1;
  bool type_is_casted : 1;
  bool type_is_volatile : 1;
  bool type_is_in_union : 1;
  bool is_indirect : 1;
  Reason operator|(const Reason &); 
  Reason& operator|=(const Reason &);
  void print() const;
  Reason() : global_is_visible(0), parameter_is_visible(0), return_is_visible(0), type_is_casted(0), type_is_volatile(0), type_is_in_union(0), is_indirect(0) {};
};


typedef std::map<const_tree, Reason> typemap;

