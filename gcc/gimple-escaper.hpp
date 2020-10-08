#pragma once

#include "gimple-walker.hpp"
#include "expr-escaper.hpp"
#include "collect-types.h"

class GimpleEscaper : public GimpleWalker
{
public:
  GimpleEscaper(ptrset_t &types) : exprEscaper(types) { _init(); };
  ExprEscaper exprEscaper;
  ptrset_t get_sets() { return exprEscaper.get_sets(); };
  void print_reasons() { exprEscaper.print_reasons(); };
protected:
  typedef std::set<const_tree> undefset;
  undefset undefined;
  void _init();
  static bool filter_known_function(cgraph_node *);
  static bool filter_known_function(const_tree);
  static bool is_function_escaping(cgraph_node *);
  static bool is_function_escaping(const_tree);
  static bool is_variable_escaping(varpool_node *);
  static bool _is_assignment_casted(gassign *s);
  virtual void _walk_global(varpool_node *);
  virtual void _walk_pre(gassign *s) ;
  virtual void _walk_pre(greturn *s) ;
  virtual void _walk_pre(gcond   *s) ;
  virtual void _walk_pre(gcall   *s) ;
  virtual void _walk_pre(const_tree) ;
};
