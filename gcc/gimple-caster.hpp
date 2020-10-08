#pragma once

#include "gimple-escaper.hpp"

/*
 * GimpleCaster is intended to walk gimple
 * and update a map that will hold information
 * on whether a type was casted or not.
 */
class GimpleCaster : public GimpleEscaper
{
public:
  GimpleCaster(ptrset_t &types) : GimpleEscaper(types) {};
private:
  virtual void _walk_pre(gcall *s) final;
  // Find out which structs are casted.
  // Technically we could find this out on parent
  virtual void _walk_pre(gassign *s) final;
};
