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
#include "cfg.h" // needed for gimple-iterator.h
#include "gimple-iterator.h"
#include "gimple-ssa.h"
#include <stdbool.h>
#include <stdio.h>


inline void
log(const char* const fmt, ...)
{
  if (!dump_file) return;
  
  va_list args;
  va_start(args, fmt);
  vfprintf(dump_file, fmt, args);
  fflush(dump_file);
  va_end(args);
}

inline void
is_gimple_code(gimple *stmt, const enum gimple_code ex_code)
{
  gcc_assert(stmt);
  const enum gimple_code ob_code = gimple_code(stmt);
  const bool succeeds = ex_code == ob_code;
  gcc_assert(succeeds);
}

inline void
assert_is_complete(const_tree a)
{
  gcc_assert(a);
  const_tree type_size = TYPE_SIZE(a);
  gcc_assert(NULL_TREE != type_size);
}

inline bool
is_complete(const_tree a)
{
  gcc_assert(a);
  const_tree type_size = TYPE_SIZE(a);
  const bool _is_complete = NULL_TREE != type_size;
  return _is_complete;
}

inline bool
is_incomplete(const_tree a)
{
  return !is_complete(a);
}

inline void
assert_is_type(const_tree a, const enum tree_code expected_code)
{
  gcc_assert(a);
  const enum tree_code observed_code = TREE_CODE(a);
  const bool eq_codes = observed_code == expected_code;
  gcc_assert(eq_codes);
}

inline void
assert_is_complete_type(const_tree a, const enum tree_code expected_code)
{
  //assert_is_complete(a);
  assert_is_type(a, expected_code);
}

inline void
is_gimple_rhs_class(gimple *stmt, const enum gimple_rhs_class ex_class)
{
  gcc_assert(stmt);
  is_gimple_code(stmt, GIMPLE_ASSIGN);
  const enum gimple_rhs_class ob_class = gimple_assign_rhs_class(stmt);
  const bool succeeds = ex_class == ob_class;
  gcc_assert(succeeds);
}
