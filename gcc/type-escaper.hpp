#pragma once

#include "type-walker.hpp"
#include "ipa-prototype.h"
#include "collect-types.h"
#include <map>

class TypeEscaper : public TypeWalker
{
public:
  TypeEscaper(ptrset_t &p) : _ptrset(p), _inside_union(0), _inside_field(0), _inside_indirect_field(0), _inside_function(0) {};
  void update(const_tree t, Reason r);
  void update_single_level(const_tree t, Reason r);
  ptrset_t get_sets();
  ptrset_t &_ptrset;
  typemap calc;
  void print_reasons();
  virtual void _walk_POINTER_TYPE_pre(const_tree t) final;
  virtual void _walk_POINTER_TYPE_post(const_tree t) final;
  virtual void _walk_REFERENCE_TYPE_pre(const_tree t) final;
  virtual void _walk_ARRAY_TYPE_pre(const_tree t) final;
  virtual void _walk_RECORD_TYPE_pre(const_tree t) final;
  virtual void _walk_UNION_TYPE_pre(const_tree t) final;
  virtual void _walk_UNION_TYPE_post(const_tree t) override final;
  virtual void _walk_METHOD_TYPE_pre(const_tree t) final override;
  virtual void _walk_FUNCTION_TYPE_pre(const_tree t) final override;
  virtual void _walk_FUNCTION_TYPE_post(const_tree t) final override;
  virtual void _walk_METHOD_TYPE(const_tree t) final override;
  virtual void _walk_FUNCTION_TYPE(const_tree t) final override;
  virtual void _walk_function_or_method(const_tree t) final override;
  virtual void _walk_field_pre(const_tree t) final;
  virtual void _walk_field_post(const_tree t) final;
  virtual bool is_memoized(const_tree t);
  unsigned _inside_union;
  unsigned _inside_field;
  unsigned _inside_indirect_field;
  unsigned _inside_function;
  Reason _reason;
  void _update(const_tree t);
  void place_escaping_types_in_set();
};

