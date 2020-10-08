#pragma once

#include "gimple-walker.hpp"
#include "expr-collector.hpp"

class GimpleTypeCollector : public GimpleWalker
{
private:
  ExprCollector exprCollector;
public:
  GimpleTypeCollector() {};
  ptrset_t get_pointer_set() { return exprCollector.get_pointer_set(); }
  // TODO: I believe this could be made const
  void print_collected();
private:
  virtual void _walk_pre(const_tree) final;
  virtual void _walk_pre(gassign *s) final;
  virtual void _walk_pre(greturn *s) final;
  virtual void _walk_pre(gcond *s) final;
  virtual void _walk_pre(gcall *s) final;
};

