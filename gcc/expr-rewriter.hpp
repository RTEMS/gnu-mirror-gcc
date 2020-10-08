#pragma once

#include "expr-walker.hpp"
#include "type-reconstructor.hpp"

class ExprTypeRewriter : public ExprWalker
{
public:
  ExprTypeRewriter(TypeReconstructor::reorg_record_map_t map, TypeReconstructor::reorg_field_map_t map2) : _delete(false), _map(map), _map2(map2) {
    for (auto i = map.cbegin(), e = map.cend(); i != e; ++i)
    {
      const_tree original = i->first;
      tree modified = i->second;
      _imap[modified] = original;
    }
  };
  void handle_pointer_arithmetic_constants(gimple *s, tree p, tree i, bool);
  void handle_pointer_arithmetic_diff(gimple *s, tree p, tree i);
  void handle_pointer_arithmetic_nonconstant(gimple *s, tree p, tree i, bool);
  bool is_interesting_type(tree);
  bool delete_statement();
  bool _delete;
private:
  TypeReconstructor::reorg_record_map_t _map;
  TypeReconstructor::reorg_field_map_t _map2;
  std::map<tree, const_tree> _imap;
  void _walk_post(const_tree e);
  void _walk_MEM_REF_post(const_tree e);
  void _walk_COMPONENT_REF_post(const_tree e);
  void _walk_PARM_DECL_post(const_tree e);
  void _walk_SSA_NAME_post(const_tree e);
  void _walk_FUNCTION_DECL_post(const_tree e);
};
