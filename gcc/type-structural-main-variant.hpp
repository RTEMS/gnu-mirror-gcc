#pragma once

#include "type-structural-equality.hpp"

class TypeStructuralEqualityMainVariant : public TypeStructuralEquality {
public:
  TypeStructuralEqualityMainVariant() {};
protected:
  virtual bool _equal(const_tree l, const_tree r);
};
