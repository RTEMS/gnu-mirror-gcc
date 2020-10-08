#pragma once

#include "gimple-walker.hpp"
#include "expr-accessor.hpp"

/*
 * GimpleAccesser is intended to walk gimple
 * and update a map that will hold information
 * on whether a type was casted or not.
 */
class GimpleAccesser : public GimpleWalker
{
public:
  GimpleAccesser() : GimpleWalker() {};
  void print_accesses() { exprAccessor.print_accesses(); };
  record_field_map_t get_map() { return exprAccessor.get_map(); };
private:
  ExprAccessor exprAccessor;
  virtual void _walk_pre(gcall *s) final;
  virtual void _walk_pre(gassign *s) final;
  virtual void _walk_pre(greturn *s) final;
  virtual void _walk_pre(gcond *s) final;
  // Do we need a glabel? I don't think so...
  // But we might need a gswitch.
};
