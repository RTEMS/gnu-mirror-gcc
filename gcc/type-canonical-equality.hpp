#pragma once

#include "type-structural-main-variant.hpp"

class TypeCanonicalEquality : public TypeStructuralEqualityMainVariant {
public:
  TypeCanonicalEquality() {};
protected:
  virtual bool _equal(const_tree l, const_tree r);
};
