#pragma once

#include "expr-walker.hpp"
#include "type-collector.hpp"

class ExprCollector : public ExprWalker {
private:
  TypeCollector typeCollector;
public:
  ExprCollector() {};
  ptrset_t get_pointer_set() { return typeCollector.get_pointer_set(); }
private:
  virtual void _walk_pre(const_tree e) final;
};

