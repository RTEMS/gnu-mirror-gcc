#include "type-accessor.hpp"
#include "type-stringifier.hpp"

void
TypeAccessor::_walk_RECORD_TYPE_pre(const_tree t)
{
  log("type walking\n");
  add_all_fields_in_struct(t);
}

void
TypeAccessor::add_all_fields_in_struct(const_tree t)
{
  TypeStringifier stringifier;
  std::string name = stringifier.stringify(t);
  log("am i in add all fields ? %s\n", name.c_str());
  const enum tree_code c = TREE_CODE(t);
  const bool is_record = RECORD_TYPE == c;
  if (!is_record) return;

  const bool record_already_in_map = _map.find(t) != _map.end();
  field_access_map_t field_map; 
  field_map = record_already_in_map ? _map[t] : field_map;

  // Let's add all fields to the field map as empty.
  for (tree field = TYPE_FIELDS(t); field; field = DECL_CHAIN(field))
  {
    const bool field_already_in_map_2 = field_map.find(field) != field_map.end();
    if (field_already_in_map_2) continue;
    field_map[field] = Empty;
  }

  _map[t] = field_map;
}
