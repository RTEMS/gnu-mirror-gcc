#pragma once

#include "ipa-prototype.h"
#include "expr-walker.hpp"
#include "type-escaper.hpp"
#include "collect-types.h"

class ExprEscaper : public ExprWalker
{
public:
  TypeEscaper typeEscaper;
  ExprEscaper(ptrset_t &types) : typeEscaper(types) {};
  ptrset_t get_sets() { return typeEscaper.get_sets(); };
  void update(const_tree t, Reason r);
  void update_single_level(const_tree t, Reason r) { typeEscaper.update_single_level(TREE_TYPE(t), r); };
  void print_reasons() { typeEscaper.print_reasons(); };
private:
  Reason _r;
  virtual void _walk_pre(const_tree e);
  virtual void _walk_CONSTRUCTOR_pre(const_tree e);
};


