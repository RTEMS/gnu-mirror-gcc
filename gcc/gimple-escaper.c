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
#include "gimple-pretty-print.h"
#include <stdbool.h>

#include "gimple-escaper.hpp"
#include "type-stringifier.hpp"
#include "type-incomplete-equality.hpp"


void
GimpleEscaper::_init()
{
  cgraph_node *cnode = NULL;
  FOR_EACH_FUNCTION(cnode)
  {
    gcc_assert(cnode);
    const bool filter = GimpleEscaper::filter_known_function(cnode);
    if (filter) continue;

    const_tree decl = cnode->decl;
    gcc_assert(decl);
    undefined.insert(decl);
  }

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY(cnode)
  {
    gcc_assert(cnode);
    cnode->get_untransformed_body();
    const_tree decl = cnode->decl;
    gcc_assert(decl);
    undefined.erase(decl);
  }
}

bool
GimpleEscaper::is_function_escaping(cgraph_node *cnode)
{
  const bool filter = GimpleEscaper::filter_known_function(cnode);
  if (filter) return false;

  return cnode->externally_visible;
}

bool
GimpleEscaper::is_function_escaping(const_tree fndecl)
{
  if (!fndecl) return true;

  if (!TREE_PUBLIC(fndecl) || DECL_EXTERNAL(fndecl)) return false;

  return true;
}

bool
GimpleEscaper::is_variable_escaping(varpool_node *vnode)
{
  gcc_assert(vnode);
  return vnode->externally_visible;
}

void
GimpleEscaper::_walk_global(varpool_node *vnode)
{
  gcc_assert(vnode);
  const_tree var_decl = vnode->decl;
  Reason reason {} ;
  const bool is_escaping = is_variable_escaping(vnode);
  reason.global_is_visible = is_escaping;

  tree initial = DECL_INITIAL (var_decl);
  const bool constructor = initial ? TREE_CODE (initial) == CONSTRUCTOR : false;
  const bool error_mark = initial ? TREE_CODE (initial) == ERROR_MARK: false;
  reason.global_is_visible |= constructor || error_mark; // static initialization...

  TypeStringifier stringifier;
  std::string name = stringifier.stringify(TREE_TYPE(var_decl));
  log("%s %s\n", vnode->name(), name.c_str());
  exprEscaper.update(var_decl, reason);
  GimpleWalker::_walk_global(vnode);

}

bool
GimpleEscaper::filter_known_function(const_tree fndecl)
{
  assert_is_type(fndecl, FUNCTION_DECL);
  if (fndecl_built_in_p (fndecl))
  {
    switch (DECL_FUNCTION_CODE (fndecl))
    {
      case BUILT_IN_FREE:
      case BUILT_IN_MALLOC:
      case BUILT_IN_REALLOC:
      case BUILT_IN_CALLOC:
      case BUILT_IN_MEMSET:
        return true;
      break;
      default:
      break;
    }
  }


  const_tree identifier_node = DECL_NAME(fndecl);
  gcc_assert(identifier_node);
  bool filter = false;
  const char *_specqsort= "spec_qsort";
  const char *_med3 = "arc_compare";
  const char *_getArcPosition = "getArcPosition";
  const char *_med3_ = "med3.part.0";
  const char *_med3_2 = "med3";
  const char* name = IDENTIFIER_POINTER(identifier_node);
  gcc_assert(name);
  filter |= strcmp(_specqsort, name) == 0;
  filter |= strcmp(_med3, name) == 0;
  filter |= strcmp(_med3_, name) == 0;
  filter |= strcmp(_med3_2, name) == 0;
  filter |= strcmp(_getArcPosition, name) == 0;
  return filter;
}

bool 
GimpleEscaper::filter_known_function(cgraph_node *node)
{
  if (!node) return false;
  return filter_known_function(node->decl);
}

void
GimpleEscaper::_walk_pre(const_tree t)
{
  // Is any global variable escaping?
  Reason reason;
  exprEscaper.update(t, reason);
}

void
GimpleEscaper::_walk_pre(gassign *s)
{
  Reason reason;
  const enum gimple_rhs_class code = gimple_assign_rhs_class(s);
  switch (code)
  {
    case GIMPLE_TERNARY_RHS:
    {
      const_tree rhs3 = gimple_assign_rhs3(s);
      exprEscaper.update(rhs3, reason);
    }
    /* fall-through */
    case GIMPLE_BINARY_RHS:
    {
      const_tree rhs2 = gimple_assign_rhs2(s);
      exprEscaper.update(rhs2, reason);
    }
    /* fall-through */
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
    {
      const_tree rhs1 = gimple_assign_rhs1(s);
      exprEscaper.update(rhs1, reason);
      const_tree lhs = gimple_assign_lhs(s);
      if (!lhs) break;
      exprEscaper.update(lhs, reason);
    }
    break;
    default:
    gcc_unreachable();
    break;
  }
}

void
GimpleEscaper::_walk_pre(greturn *s)
{
  Reason reason;
  const_tree val = gimple_return_retval(s);
  if (!val) return;
  exprEscaper.update(val, reason);
}

void
GimpleEscaper::_walk_pre(gcond *s)
{
  Reason reason;
  const_tree lhs = gimple_cond_lhs(s);
  const_tree rhs = gimple_cond_rhs(s);
  gcc_assert(lhs && rhs);
  exprEscaper.update(lhs, reason);
  exprEscaper.update(rhs, reason);
}

void
GimpleEscaper::_walk_pre(gcall *s)
{
  const_tree fn = gimple_call_fndecl(s);
  // gcc_assert(fn);
  // The above will not always be true
  cgraph_node *node = fn ? cgraph_node::get(fn) : NULL;
  // const bool fn_and_node = fn && node;
  // const bool not_function_and_not_node = !fn && !node;
  // const bool test = fn_and_node ^ not_function_and_not_node;
  // gcc_assert(test);
  // The above is not true...
  // which means that there are functions with function declarations
  // but no corresponding cgraph_node.
  //
  // What does that mean for our analysis?
  // It means that we cannot find out if a function is escaping all the time..?
  // Or at least via the cnode...
  // It seems to me that the correct way to deal with this is saying that 
  // functions which do not have a cgraph_node should be escaping,
  // but this will mark some interesting types as escaping...
  const bool _is_function_escaping = node ? is_function_escaping(node) : is_function_escaping(fn);
  const bool is_undefined = undefined.find(fn) != undefined.end();
  const bool _is_escaping = is_undefined || _is_function_escaping;

  TypeStringifier stringifier;
  Reason arg_reason;
  arg_reason.parameter_is_visible = _is_escaping;
  arg_reason.is_indirect = !fn;
  unsigned n = gimple_call_num_args(s);
  for (unsigned i = 0; i < n; i++)
  {
    const_tree a = gimple_call_arg(s, i);
    gcc_assert(a);
    exprEscaper.update(a, arg_reason);
  }

  const_tree lhs = gimple_call_lhs(s);
  if (!lhs) return;
  Reason return_reason;
  return_reason.return_is_visible = _is_escaping;
  return_reason.is_indirect = !fn;
  exprEscaper.update(lhs, return_reason);
}
