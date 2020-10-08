#pragma once

#include "type-canonical-equality.hpp"

class TypeIncompleteEquality : public TypeCanonicalEquality {
public:
  TypeIncompleteEquality () {};
protected:
  virtual bool _equal(const_tree l, const_tree r);
};
