#pragma once

#include "gimple-walker.hpp"
#include "expr-rewriter.hpp"
#include "type-reconstructor.hpp"

class GimpleTypeRewriter : public GimpleWalker
{
public:
  GimpleTypeRewriter(TypeReconstructor::reorg_record_map_t map, TypeReconstructor::reorg_field_map_t map2) : exprTypeRewriter(map, map2) {};
  void _rewrite_function_decl();
private:
  ExprTypeRewriter exprTypeRewriter;
  void handle_pointer_arithmetic(gimple *s);
  virtual void _walk_pre(gphi* ) final;
  virtual void _walk_pre(const_tree) final;
  virtual void _walk_pre(gimple*) final;
  virtual void _walk_pre(gcall *s) final;
  virtual void _walk_pre(greturn *s) final;
  virtual void _walk_pre(gassign *s) final;
  virtual void _walk_pre(gcond *s) final;
};
