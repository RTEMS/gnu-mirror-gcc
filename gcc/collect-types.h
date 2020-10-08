#pragma once

#include "tree.h"
#include <set>


typedef std::set<const_tree> typeset;
struct points_to_record_sets_s {
  typeset universe;  
  typeset points_to_record;
  typeset complement;
  typeset escaping;
  typeset non_escaping;
  bool in_universe(const_tree) const;
  bool in_points_to_record(const_tree) const;
  bool in_complement(const_tree) const;
  void insert(const_tree, bool);
  void print_in_points_to_record() const;
};

typedef struct points_to_record_sets_s ptrset_t;
