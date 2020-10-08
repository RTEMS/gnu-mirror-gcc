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
#include <set>
#include <string>
#include <map>

#include "collect-types.h"
#include "type-stringifier.hpp"

#include "type-collector.hpp"
#include "expr-walker.hpp"
#include "expr-collector.hpp"
#include "gimple-walker.hpp"
#include "tree-cfg.h"

inline static void
print_function (cgraph_node *cnode)
{
  if (!dump_file)
    return;
  gcc_assert (cnode);
  cnode->get_untransformed_body ();
  dump_function_to_file (cnode->decl, dump_file, TDF_NONE);
}

void
GimpleWalker::walk()
{
  _walk_globals();

  std::set<tree> fndecls;
  cgraph_node *node = NULL;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY(node)
  {
    print_function(node);
    node->get_untransformed_body();
    const bool already_in_set = fndecls.find(node->decl) != fndecls.end();
    if (already_in_set) continue;
    _walk_cnode(node);
    fndecls.insert(node->decl);
  }
}

void
GimpleWalker::_walk_globals()
{
  varpool_node *vnode = NULL;
  FOR_EACH_VARIABLE(vnode)
  {
    _walk_global(vnode);
  }
}

void
GimpleWalker::_walk_global(varpool_node *vnode)
{
  gcc_assert(vnode);
  struct ipa_ref *ref = NULL;
  for (unsigned i = 0; vnode->iterate_referring(i, ref); i++)
  {
    tree var_decl = vnode->decl;
    walk(var_decl);
  }
}

void
GimpleWalker::_walk_ssa_names(cgraph_node *cnode)
{
  const_tree decl = cnode->decl;
  gcc_assert(decl);
  function *func = DECL_STRUCT_FUNCTION(decl);
  gcc_assert(func);
  size_t i = 0;
  tree ssa_name = NULL;
  push_cfun(func);
  FOR_EACH_SSA_NAME(i, ssa_name, cfun)
  {
    gcc_assert(ssa_name);
    walk(ssa_name);
  }
  pop_cfun();
}

void
GimpleWalker::_walk_cnode(cgraph_node *cnode)
{
  gcc_assert(cnode);
  _walk_decl(cnode);
  _walk_locals(cnode);
  _walk_ssa_names(cnode);
  _walk_bb(cnode);
}


void
GimpleWalker::_walk_decl(cgraph_node *cnode)
{
  const_tree decl = cnode->decl;
  gcc_assert(decl);
  walk(decl);
}

void
GimpleWalker::_walk_locals(cgraph_node *cnode)
{
  const_tree decl = cnode->decl;
  gcc_assert(decl);
  function *func = DECL_STRUCT_FUNCTION(decl);
  gcc_assert(func);
  int i = 0;
  tree var_decl = NULL;
  FOR_EACH_LOCAL_DECL(func, i, var_decl)
  {
    gcc_assert(var_decl);
    walk(var_decl);
  }
}


void
GimpleWalker::_walk_bb(cgraph_node* cnode)
{
  gcc_assert(cnode);
  cnode->get_untransformed_body();
  const_tree decl = cnode->decl;
  gcc_assert(decl);
  function *func = DECL_STRUCT_FUNCTION(decl);
  gcc_assert(func);
  basic_block bb = NULL;
  push_cfun(func);
  FOR_EACH_BB_FN(bb, func)
  {
    _walk(bb);
  }
  pop_cfun();
}

void
GimpleWalker::_walk(basic_block bb)
{
  gcc_assert(bb);
  bool first = true;
  gimple_stmt_iterator gsi = gsi_start_bb(bb);

  while (!gsi_end_p(gsi))
  {
    gimple *stmt = gsi_stmt(gsi);
    walk(stmt);
    if (_deleted) unlink_stmt_vdef (stmt);
    if (_deleted) { gsi_remove(&gsi, true); }
    else { gsi_next(&gsi); }
    _deleted = false;
  }


  for (gimple_stmt_iterator gsi = gsi_start_phis(bb); !gsi_end_p(gsi); gsi_next(&gsi))
  {
    gimple *stmt = gsi_stmt(gsi);
    walk(stmt);
  }
}

void
GimpleWalker::walk(gimple *stmt)
{
  _walk_pre(stmt);
  _walk(stmt);
  _walk_post(stmt);
}

void
GimpleWalker::_walk(gimple *stmt)
{
  gcc_assert(stmt);

#define GimpleWalkerWalk(type) \
  if (type s = dyn_cast< type >(stmt)) \
  { \
    _walk_pre(stmt); \
    walk(s); \
    _walk_post(stmt); \
    return; \
  }

  GimpleWalkerWalk(gassign*);
  GimpleWalkerWalk(greturn*);
  GimpleWalkerWalk(gcond*);
  GimpleWalkerWalk(gcall*);
  GimpleWalkerWalk(glabel*);
  GimpleWalkerWalk(gswitch*);
  GimpleWalkerWalk(gphi*);


  const enum gimple_code code = gimple_code (stmt);
  switch (code)
  {
    case GIMPLE_PREDICT: return;
    case GIMPLE_DEBUG: return;
    default: break;
  }
  const char* name = gimple_code_name[code];
  log("gimple code name %s\n", name);
  gcc_unreachable();
}

#define GimpleWalkerFuncDef(type) \
void \
GimpleWalker::walk (type e) \
{ \
  _walk_pre (e); \
  _walk (e); \
  _walk_post (e); \
} \
\
void \
GimpleWalker::_walk (__attribute__((unused)) type e) \
{ \
} 

GimpleWalkerFuncDef(const_tree)
GimpleWalkerFuncDef(gassign *)
GimpleWalkerFuncDef(greturn *)
GimpleWalkerFuncDef(gcond *)
GimpleWalkerFuncDef(gcall *)
GimpleWalkerFuncDef(glabel *)
GimpleWalkerFuncDef(gswitch *)
GimpleWalkerFuncDef(gphi *)

