#include "ipa-str-reorg-utils.h"

// This really should be inaccessible to anyone.
const_tree
get_base_type_from_ptr_or_arr_type (const_tree old_pointer_type,
				    const_tree pointer_type,
				    unsigned int &indirection_level)
{
  if (pointer_type == NULL)
    {
      gcc_assert (TREE_CODE (old_pointer_type) != POINTER_TYPE);
      gcc_assert (TREE_CODE (old_pointer_type) != ARRAY_TYPE);
      return old_pointer_type;
    }
  return get_base_type_from_ptr_or_arr_type (pointer_type,
					     TREE_TYPE (pointer_type),
					     ++indirection_level);
}

const_tree
get_base_type_from_ptr_or_arr_type (const_tree ptr_or_array,
				    unsigned int &indirection_level)
{
  const bool is_array = TREE_CODE (ptr_or_array) == ARRAY_TYPE;
  const bool is_ptr = TREE_CODE (ptr_or_array) == POINTER_TYPE;
  const bool is_array_or_ptr = is_array || is_ptr;
  gcc_assert (is_array_or_ptr);
  indirection_level = 0;
  return get_base_type_from_ptr_or_arr_type (ptr_or_array,
					     TREE_TYPE (ptr_or_array),
					     indirection_level);
}

const_tree
get_base_type_from_array_type (const_tree array_type,
			       unsigned int &indirection_level)
{
  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  return get_base_type_from_ptr_or_arr_type (array_type, indirection_level);
}

const_tree
get_base_type_from_array_type (const_tree array_type)
{
  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  unsigned int indirection_level;
  return get_base_type_from_array_type (array_type, indirection_level);
}

const_tree
get_base_type_from_pointer_type (const_tree pointer_type,
				 unsigned int &indirection_level)
{
  gcc_assert (TREE_CODE (pointer_type) == POINTER_TYPE);
  return get_base_type_from_ptr_or_arr_type (pointer_type, indirection_level);
}

const_tree
get_base_type_from_pointer_type (const_tree pointer_type)
{
  gcc_assert (TREE_CODE (pointer_type) == POINTER_TYPE);
  unsigned int indirection_level;
  return get_base_type_from_pointer_type (pointer_type, indirection_level);
}

const_tree
get_base_type (const_tree type)
{
  enum tree_code tree_code_type = TREE_CODE(type);
  switch (tree_code_type)
  {
    case ARRAY_TYPE: return get_base_type_from_array_type(type); break;
    case POINTER_TYPE: return get_base_type_from_pointer_type(type); break;
    default: return type; break;
  }

  gcc_unreachable();
  return NULL;
}
const char *
make_pointer_name (const_tree pointer)
{
  gcc_assert (TREE_CODE (pointer) == POINTER_TYPE);
  unsigned int indirection_level;
  const_tree base_type
    = get_base_type_from_pointer_type (pointer, indirection_level);
  const char *pointer_name = make_pointer_name (base_type, indirection_level);
  return pointer_name;
}

const char *
make_pointer_or_array_name (const char *base_type, const char *postfix)
{
  char *ptr;
  int calculated_size = strlen (base_type) + strlen (postfix);
  // TODO: Do not use asprintf?
  // We'll let exit() deal with freeing this memory.
  int retval = asprintf (&ptr, "%s%s", base_type, postfix);
  gcc_assert (retval == calculated_size);
  return ptr;
}

const char *
make_array_postfix (unsigned int indirection_level)
{
  gcc_assert (indirection_level > 0);
  static const char *max_indirection_level_str_array
    = "[][][][][][][][][][][][][]";
  static const size_t size_array = strlen (max_indirection_level_str_array);
  static const size_t postfix_size_array = 2;
  static const size_t max_indirection_level_array
    = size_array / postfix_size_array;
  gcc_assert (indirection_level < max_indirection_level_array);
  return max_indirection_level_str_array + size_array
	 - (indirection_level * postfix_size_array);
}

const char *
make_pointer_postfix (unsigned int indirection_level)
{
  gcc_assert (indirection_level > 0);
  static const char *max_indirection_level_str_pointer
    = "************************";
  static const size_t size_pointer = strlen (max_indirection_level_str_pointer);
  static const size_t postfix_size_pointer = 1;
  static const size_t max_indirection_level_pointer
    = size_pointer / postfix_size_pointer;
  gcc_assert (indirection_level < max_indirection_level_pointer);
  return max_indirection_level_str_pointer + size_pointer
	 - (indirection_level * postfix_size_pointer);
}

const char *
make_pointer_name (const char *base_type_name,
		   const unsigned int indirection_level)
{
  const char *postfix = make_pointer_postfix (indirection_level);
  const char *ptr = make_pointer_or_array_name (base_type_name, postfix);
  return ptr;
}

const char *
make_pointer_name (const_tree base_type, const unsigned int indirection_level)
{
  const char *struct_name = get_type_name (base_type);
  return make_pointer_name (struct_name, indirection_level);
}


const char *
make_array_name (const char *base_type_name,
		 const unsigned int indirection_level)
{
  const char *postfix = make_array_postfix (indirection_level);
  const char *ptr = make_pointer_or_array_name (base_type_name, postfix);
  return ptr;
}

const char *
make_array_name (const_tree base_type, const unsigned int indirection_level)
{
  const char *struct_name = get_type_name (base_type);
  return make_array_name (struct_name, indirection_level);
}

// TODO: deal with anonymous structs.
// Some records don't have identifier pointers
const char *
get_record_name (const_tree record)
{
  gcc_assert (record);
  gcc_assert (TREE_CODE (record) == RECORD_TYPE);
  tree name_tree = TYPE_NAME (record);
  // The TYPE_NAME will be NULL_TREE for a type
  // that is not a built-in type, the result of a typedef
  // or a named class type.
  // TODO: verify that we are never changing
  // <name_tree_is_null> types
  if (name_tree == NULL_TREE)
    {
      return "<name_tree_is_null>";
    }

  if (TREE_CODE (name_tree) == TYPE_DECL)
    {
      tree type_name = DECL_NAME (name_tree);
      return IDENTIFIER_POINTER (type_name);
    }
  const char *identifier = IDENTIFIER_POINTER (name_tree);
  gcc_assert (identifier);
  return identifier;
}

const char *
make_array_name (const_tree array)
{
  gcc_assert (TREE_CODE (array) == ARRAY_TYPE);
  unsigned int indirection_level;
  const_tree base_type
    = get_base_type_from_array_type (array, indirection_level);
  const char *array_name = make_array_name (base_type, indirection_level);
  return array_name;
}

const char *
get_array_name (const_tree array)
{
  gcc_assert (array);
  gcc_assert (TREE_CODE (array) == ARRAY_TYPE);
  const bool is_modified = TYPE_NAME (array);
  if (is_modified)
    return IDENTIFIER_POINTER (TYPE_NAME (array));

  return make_array_name (array);
}

const char *
get_pointer_name (const_tree pointer)
{
  gcc_assert (pointer);
  gcc_assert (TREE_CODE (pointer) == POINTER_TYPE);
  const bool is_modified = TYPE_NAME (pointer);
  if (is_modified)
    return IDENTIFIER_POINTER (TYPE_NAME (pointer));

  const char *new_pointer_name = make_pointer_name (pointer);
  return new_pointer_name;
}

const char *
make_reference_name (const_tree ref)
{
  gcc_assert (TREE_CODE (ref) == REFERENCE_TYPE);
  const_tree base_type = TREE_TYPE (ref);
  const char *old_name = get_type_name (base_type);
  char *ptr;
  static const char *prefix = "&";
  static const char *suffix = ".reorg";
  int new_size = strlen (prefix) + strlen (old_name) + strlen (suffix);
  int retval = asprintf (&ptr, "%s%s%s", prefix, old_name, suffix);
  gcc_assert (retval == new_size);
  return ptr;
}


const char *
get_reference_name (const_tree ref)
{
  gcc_assert (ref);
  gcc_assert (TREE_CODE (ref) == REFERENCE_TYPE);
  const bool is_modified = TYPE_NAME (ref);
  if (is_modified)
    return IDENTIFIER_POINTER (TYPE_NAME (ref));

  const char *new_pointer_name = make_reference_name (ref);
  return new_pointer_name;
}

const char *
get_type_name (const_tree type)
{
  enum tree_code code = TREE_CODE (type);
  switch (code)
    {
    case ARRAY_TYPE:
      return get_array_name (type);
      break;
    case POINTER_TYPE:
      return get_pointer_name (type);
      break;
    case RECORD_TYPE:
      return get_record_name (type);
      break;
    case REFERENCE_TYPE:
      return get_reference_name (type);
      break;
    default:
      // TODO: generalize even more?
      // wait for experimental results to dictate what
      // else we should specify.
      return get_tree_code_name (code);
      break;
    }
  return NULL;
}

const char *
get_field_name (const_tree field_decl)
{
  gcc_assert (field_decl);
  gcc_assert (TREE_CODE (field_decl) == FIELD_DECL);
  // TODO: deal with anonymous fields.
  tree id = DECL_NAME (field_decl);
  if (!id)
    return "anonymous";
  gcc_assert (id);
  const char *identifier = IDENTIFIER_POINTER (id);
  gcc_assert (identifier);
  return identifier;
}
