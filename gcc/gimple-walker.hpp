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

#include <set>

class GimpleWalker
{
public:
  GimpleWalker() : _deleted(false) {};
  void walk();

protected:

  bool _deleted; 
  virtual void _walk_global(varpool_node*);
  void _walk_globals();
  void _walk_ssa_names(cgraph_node *cnode);
  void _walk_cnode(cgraph_node *cnode);
  void _walk_decl(cgraph_node *cnode);
  void _walk_locals(cgraph_node *cnode);
  void _walk_bb(cgraph_node *cnode);
  void _walk(basic_block bb);

#define GimpleWalkerFuncDecl(type) \
  virtual void _walk_pre(type stmt) {}; \
  void walk(type stmt); \
  void _walk(type stmt); \
  virtual void _walk_post(type stmt) {}

  GimpleWalkerFuncDecl(const_tree);
  GimpleWalkerFuncDecl(gimple*); 
  GimpleWalkerFuncDecl(gassign*); 
  GimpleWalkerFuncDecl(greturn*); 
  GimpleWalkerFuncDecl(gcond*); 
  GimpleWalkerFuncDecl(gcall*); 
  GimpleWalkerFuncDecl(glabel*); 
  GimpleWalkerFuncDecl(gswitch*); 
  GimpleWalkerFuncDecl(gphi*); 
};

