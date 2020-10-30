/* Interprocedural scalar replacement of aggregates
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

  Contributed by Erick Ochoa <erick.ochoa@theobroma-systems.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"
#include "options.h"
#include "cgraph.h"
#include "tree-pass.h"
#include "tree-cfg.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "stringpool.h"  //get_identifier
#include "basic-block.h" //needed for gimple.h
#include "function.h"    //needed for gimple.h
#include "gimple.h"
#include "cfg.h" // needed for gimple-iterator.h
#include "gimple-iterator.h"
#include "stor-layout.h"   // layout_type
#include "fold-const.h"    //build_fold_addr_expr
#include "gimple-ssa.h"    // update_stmt
#include "attribs.h"       // decl_attributes
#include "gimplify.h"      //unshare_expr
#include "value-range.h"   // make_ssa_name dependency
#include "tree-ssanames.h" // make_ssa_name
#include "ssa.h"
#include "tree-into-ssa.h"
#include <vector> // needed for ipa-structure-reorg
#include <map>
#include <set>
#include "ipa-structure-reorg.h"
#include "ipa-utils.h"
#include "ipa-str-reorg-utils.h"

#define test_write(M, ...)                                                     \
  if (dump_file)                                                               \
    {                                                                          \
      fprintf (dump_file, M, ##__VA_ARGS__);                                   \
    }

#define test_log(M, indent, ...)                                               \
  if (dump_file)                                                               \
    {                                                                          \
      fprintf (dump_file, "%*c" M "\n", indent, ' ', ##__VA_ARGS__);           \
    }


/*
static void
log_expr_prologue (const int indent, const char *debug_str, tree expr)
{
  const char *expr_str = print_generic_expr_to_str (expr);
  test_log ("<%s \"%s\">", indent, debug_str, expr_str);
  const_tree type = TREE_TYPE (expr);
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      const_tree domain = TYPE_DOMAIN (type);
      const_tree min = TYPE_MIN_VALUE (domain);
      const_tree max = TYPE_MAX_VALUE (domain);
      int _min = tree_to_uhwi (min);
      int _max = tree_to_uhwi (max);
      test_log ("< domain = (%d,%d)>", indent, _min, _max);
    }
  test_log ("< type = %s>", indent, get_type_name (type));
}

static tree
const_to_tree (const_tree ctree)
{
	return (tree) ctree;
}

static void
substitute_type (tree expr, const_tree new_type)
{
	gcc_assert(expr);
	gcc_assert(new_type);
	const_tree old_type = TREE_TYPE(expr);
	TREE_TYPE(expr) = const_to_tree(new_type);
}

static void
substitute_type (tree expr, const_tree* new_type_ptr)
{
	gcc_assert(new_type_ptr);
	substitute_type(expr, *new_type_ptr);
}

// This function should be hidden
static const_tree
get_base_type_from_ptr_or_arr_var (const_tree var,
				   unsigned int &indirection_level)
{
  tree ptr_or_array = TREE_TYPE (var);
  const bool is_array = TREE_CODE (ptr_or_array) == ARRAY_TYPE;
  const bool is_ptr = TREE_CODE (ptr_or_array) == POINTER_TYPE;
  const bool is_array_or_ptr = is_array || is_ptr;
  gcc_assert (is_array_or_ptr);
  const_tree retval
    = get_base_type_from_ptr_or_arr_type (ptr_or_array, indirection_level);
  return retval;
}

static const_tree __attribute__((unused))
get_base_type_from_array_var (const_tree var, unsigned int &indirection_level)
{
  gcc_assert (TREE_CODE (var) != ARRAY_TYPE);
  tree array_type = TREE_TYPE (var);
  gcc_assert (TREE_CODE (array_type) == ARRAY_TYPE);
  const_tree retval
    = get_base_type_from_ptr_or_arr_var (array_type, indirection_level);
  return retval;
}

static const_tree 
get_base_type_from_pointer_var (const_tree var, unsigned int &indirection_level)
{
  gcc_assert (TREE_CODE (var) != POINTER_TYPE);
  tree pointer_type = TREE_TYPE (var);
  gcc_assert (TREE_CODE (pointer_type) == POINTER_TYPE);
  const_tree retval
    = get_base_type_from_ptr_or_arr_type (pointer_type, indirection_level);
  return retval;
}

static const_tree __attribute__((unused))
get_base_type_from_pointer_var (const_tree var)
{
  gcc_assert (TREE_CODE (var) != POINTER_TYPE);
  unsigned int indirection_level;
  return get_base_type_from_pointer_var (var, indirection_level);
}

// INFO: cannot change
// tree expr to const_tree expr
static void
log_expr_epilogue (const int indent, const char *debug_str, tree expr)
{
  const char *expr_str = print_generic_expr_to_str (expr);
  const_tree type = TREE_TYPE (expr);
  if (TREE_CODE (type) == ARRAY_TYPE)
    {
      const_tree domain = TYPE_DOMAIN (type);
      const_tree min = TYPE_MIN_VALUE (domain);
      const_tree max = TYPE_MAX_VALUE (domain);
      int _min = tree_to_uhwi (min);
      int _max = tree_to_uhwi (max);
      test_log ("< domain = (%d,%d)>", indent, _min, _max);
    }
  test_log ("< type = %s>", indent, get_type_name (type));
  test_log ("</%s \"%s\">", indent, debug_str, expr_str);
}



static const char *
make_base_name_based_on_old (const char *old_name)
{
  gcc_assert (old_name);
  char *ptr;
  static const char *suffix = ".reorg";
  int new_size = strlen (old_name) + strlen (suffix);
  int retval = asprintf (&ptr, "%s%s", old_name, suffix);
  gcc_assert (retval == new_size);
  return ptr;
}

static const char *
make_base_name_based_on_old (const_tree base_type)
{
  enum tree_code base_code = TREE_CODE (base_type);
  const bool is_pointer = POINTER_TYPE == base_code;
  const bool is_array = ARRAY_TYPE == base_code;
  const bool prohibited = is_pointer || is_array;
  // This means that this is only for base types
  // and structs.
  // I.e. we don't want to generate names which have
  //[][][] at the end or ****
  gcc_assert (!prohibited);
  const char *base_type_name = get_type_name (base_type);
  return make_base_name_based_on_old (base_type_name);
}

static const char *
make_record_name_based_on_old (const char *old_record_name)
{
  return make_base_name_based_on_old (old_record_name);
}

static const char *
make_array_name_based_on_old (const_tree array)
{
  enum tree_code code = TREE_CODE (array);
  gcc_assert (ARRAY_TYPE == code);
  unsigned int indirection_level;
  const_tree base_type
    = get_base_type_from_array_type (array, indirection_level);
  const char *reorged_base_type_name = make_base_name_based_on_old (base_type);
  const char *suffix = make_array_postfix (indirection_level);
  return make_pointer_or_array_name (reorged_base_type_name, suffix);
}

static const char *
make_pointer_name_based_on_old (const_tree pointer)
{
  enum tree_code code = TREE_CODE (pointer);
  gcc_assert (POINTER_TYPE == code);
  unsigned int indirection_level;
  const_tree base_type
    = get_base_type_from_pointer_type (pointer, indirection_level);
  const char *reorged_base_type_name = make_base_name_based_on_old (base_type);
  const char *suffix = make_pointer_postfix (indirection_level);
  return make_pointer_or_array_name (reorged_base_type_name, suffix);
}

static const char *
make_record_name_based_on_old (const_tree record)
{
  enum tree_code code = TREE_CODE (record);
  gcc_assert (RECORD_TYPE == code);
  const char *old_name = get_type_name (record);
  return make_record_name_based_on_old (old_name);
}


typedef hash_set<const_tree> field_set;
typedef hash_set<const_tree> tree_set;
struct delete_info { const_tree old_record; const_tree new_record; field_set* deleted_fields;};
typedef struct delete_info delete_info_t;
typedef hash_map<const_tree, delete_info_t> t_map;

static bool
rewrite_addr_expr_def (tree expr,  t_map &type_map,
		       const int indent);
static bool
rewrite_addr_expr (tree expr,  t_map &type_map,
		   const int indent)
{
  log_expr_prologue (indent, "rewrite_addr_expr", expr);
  bool retval = rewrite_addr_expr_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_addr_expr", expr);
  return retval;
}

static bool
rewrite_array_ref_def (tree expr,  t_map &type_map,
		       const int indent);

static bool
rewrite_array_ref (tree expr,  t_map &type_map,
		   const int indent)
{
  log_expr_prologue (indent, "rewrite_array_ref", expr);
  bool retval = rewrite_array_ref_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_array_ref", expr);
  return retval;
}

static bool
rewrite_bin_expr_default_def (tree expr,
			       t_map &type_map,
			      const int indent);
static bool
rewrite_bin_expr_default (tree expr,  t_map &type_map,
			  const int indent)
{
  log_expr_prologue (indent, "rewrite_bin_expr", expr);
  bool retval = rewrite_bin_expr_default_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_bin_expr", expr);
  return retval;
}

static bool
rewrite_constructor_def (tree expr,  t_map &type_map,
			 const int indent);
static bool
rewrite_constructor (tree expr,  t_map &type_map,
		     const int indent)
{
  log_expr_prologue (indent, "rewrite_constructor", expr);
  bool retval = rewrite_constructor_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_constructor", expr);
  return retval;
}

static bool
rewrite_field_decl_def (tree expr,  t_map &type_map,
			const int indent);
static bool
rewrite_field_decl (tree expr,  t_map &type_map,
		    const int indent)
{
  log_expr_prologue (indent, "rewrite_field_decl", expr);
  bool retval = rewrite_field_decl_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_field_decl", expr);
  return retval;
}

static bool
rewrite_integer_cst_def (tree expr,  t_map &type_map,
			 const int indent);
static bool
rewrite_integer_cst (tree expr,  t_map &type_map,
		     const int indent)
{
  log_expr_prologue (indent, "rewrite_integer_cst", expr);
  bool retval = rewrite_integer_cst_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_integer_cst", expr);
  return retval;
}

static bool
rewrite_function_decl_def (tree expr,
			    t_map &type_map,
			   const int indent);
static bool
rewrite_function_decl (tree expr,  t_map &type_map,
		       const int indent)
{
  log_expr_prologue (indent, "rewrite_function_decl", expr);
  bool retval = rewrite_function_decl_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_function_decl", expr);
  return retval;
}

static bool
rewrite_component_ref (tree expr,  t_map &type_map,
		       const int indent);
static bool
rewrite_component_ref_def (tree expr,
			    t_map &type_map,
			   const int indent);
static bool
rewrite_component_ref (tree expr,  t_map &type_map,
		       const int indent)
{
  log_expr_prologue (indent, "rewrite_component_ref", expr);
  bool retval = rewrite_component_ref_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_component_ref", expr);
  return retval;
}

static bool
rewrite_expr (tree expr,  t_map &type_map,
	      const int indent);
static bool
rewrite_expr_def (tree expr,  t_map &type_map,
		  const int indent);
static bool
rewrite_expr (tree expr,  t_map &type_map,
	      const int indent)
{
  log_expr_prologue (indent, "rewrite_expr", expr);
  bool retval = rewrite_expr_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_expr", expr);
  return retval;
}

static bool
rewrite_mem_ref_def (tree expr,  t_map &type_map,
		     const int indent);

static bool
rewrite_mem_ref (tree expr,  t_map &type_map,
		 const int indent)
{
  log_expr_prologue (indent, "rewrite_mem_ref", expr);
  bool retval = rewrite_mem_ref_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_mem_ref", expr);
  return retval;
}


static bool
rewrite_ssa_name_def (tree expr,  t_map &type_map,
		      const int indent);
static bool
rewrite_ssa_name (tree expr,  t_map &type_map,
		  const int indent)
{
  log_expr_prologue (indent, "rewrite_ssa_name", expr);
  bool retval = rewrite_ssa_name_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_ssa_name", expr);
  return retval;
}

static bool
rewrite_var_decl_def (tree expr,  t_map &type_map,
		      const int indent);
static bool
rewrite_var_decl (tree expr,  t_map &type_map,
		  const int indent)
{
  log_expr_prologue (indent, "rewrite_var_decl", expr);
  bool retval = rewrite_var_decl_def (expr, type_map, indent + 4);
  log_expr_epilogue (indent, "rewrite_var_decl", expr);
  return retval;
}

static bool
rewrite_expr_def (tree expr,  t_map &type_map,
		  const int indent)
{
  bool retval = false;
  switch (TREE_CODE (expr))
    {
    case ADDR_EXPR:
      retval = rewrite_addr_expr (expr, type_map, indent);
      break;
    case ARRAY_REF:
      retval = rewrite_array_ref (expr, type_map, indent);
      break;
    case COMPONENT_REF:
      retval = rewrite_component_ref (expr, type_map, indent);
      break;
    case CONSTRUCTOR:
      retval = rewrite_constructor (expr, type_map, indent);
      break;
    case FIELD_DECL:
      retval = rewrite_field_decl (expr, type_map, indent);
      break;
    case FUNCTION_DECL:
      retval = rewrite_function_decl (expr, type_map, indent);
      break;
    case INTEGER_CST:
      retval = rewrite_integer_cst (expr, type_map, indent);
      break;
    case MEM_REF:
      retval = rewrite_mem_ref (expr, type_map, indent);
      break;
    case MINUS_EXPR:
      retval = rewrite_bin_expr_default (expr, type_map, indent);
      break;
    case MULT_EXPR:
      retval = rewrite_bin_expr_default (expr, type_map, indent);
      break;
    case POINTER_DIFF_EXPR:
      // not needed
      break;
    case POINTER_PLUS_EXPR:
      // not needed
      break;
    case PLUS_EXPR:
      // not needed
      break;
    case SSA_NAME:
      retval = rewrite_ssa_name (expr, type_map, indent);
      break;
    case VAR_DECL:
      retval = rewrite_var_decl (expr, type_map, indent);
      break;
    default:
      {
	//TODO: can we put an unreachable here?
	enum tree_code code = TREE_CODE (expr);
	test_log ("code = %s", indent, get_tree_code_name (code));
	delete_info_t *delete_info = type_map.get (TREE_TYPE(expr));
	const_tree *new_type_ptr = delete_info ? &(delete_info->new_record) : NULL;
	if (!new_type_ptr)
	  return false;
	retval = true;
	substitute_type(expr, new_type_ptr);
      }
      break;
    }

  return retval;
}

inline static void
test_print_generic_decl (tree decl)
{
  if (!dump_file)
    return;
  print_generic_decl (dump_file, decl, TDF_DETAILS);
}

inline static void
test_print_gimple_stmt (gimple *stmt)
{
  if (!dump_file)
    return;
  print_gimple_stmt (dump_file, stmt, 0, TDF_NONE);
}

inline static void
print_function (cgraph_node *cnode)
{
  if (!dump_file)
    return;
  gcc_assert (cnode);
  cnode->get_untransformed_body ();
  dump_function_to_file (cnode->decl, dump_file, TDF_NONE);
}

static unsigned
get_field_offset (const_tree field)
{
  gcc_assert (TREE_CODE (field) == FIELD_DECL);
  tree cst = byte_position (field);
  unsigned offset = tree_to_uhwi (cst);
  return offset;
}


static const_tree
get_field_with_offset_unsafe (const_tree record, const unsigned offset)
{
  gcc_assert (record);
  gcc_assert (TREE_CODE (record) == RECORD_TYPE);
  for (tree field = TYPE_FIELDS (record); field; field = DECL_CHAIN (field))
    {
      unsigned field_offset = get_field_offset (field);
      if (field_offset == offset)
	return field;
    }

  return NULL;
}

static const_tree __attribute__((unused))
get_field_with_offset (const_tree record, const unsigned offset)
{
  const_tree retval = get_field_with_offset_unsafe (record, offset);
  gcc_assert (retval != NULL);
  return retval;
}

static const_tree
get_field_with_name_unsafe (const_tree record, const char *identifier)
{
  gcc_assert (record);
  gcc_assert (TREE_CODE (record) == RECORD_TYPE);
  for (tree field = TYPE_FIELDS (record); field; field = DECL_CHAIN (field))
    {
      const char *field_identifier = get_field_name (field);
      const bool is_same_name = strcmp (field_identifier, identifier) == 0;
      if (!is_same_name)
	continue;

      gcc_assert (TREE_CODE (field) == FIELD_DECL);
      return field;
    }
  return NULL;
}

static const_tree
get_field_with_name (const_tree record, const char *identifier)
{
  const_tree retval = get_field_with_name_unsafe (record, identifier);
  gcc_assert (retval != NULL);
  return retval;
}

static bool
filter_out_boring_type (const_tree type, hash_set<const_tree> &map);

inline static void
is_record_p(const_tree record)
{
  gcc_assert(record);
  const enum tree_code code = TREE_CODE(record);
  const bool is_record = RECORD_TYPE == code;
  gcc_assert(is_record);
}

struct cstrless {
  bool operator()(const char* a, const char* b) { return strcmp(a, b) < 0; }
};
std::set<const char*, cstrless> interesting_records;
std::set<const char*, cstrless> interesting_fields;
std::set<const char*, cstrless> record_field_pair_name;
std::set<const char*, cstrless> updated_functions;

static bool
is_interesting_struct_escape_analysis(const_tree record)
{
  is_record_p(record);
  const_tree main_variant = TYPE_MAIN_VARIANT(record);
  is_record_p(main_variant);
  const char* const ob_type_name = get_type_name(main_variant);
  const char* blacklist [5] = { "__gcov_fn_info", "__gcov_ctr_info", "__gcov_fn_info", "__gcov_info", "indirect_call_tuple" };
  //const char* whitelist [4] = { "arc", "arc_t", "node", "node_t" };

  for (int i = 0; i < 5; i++)
  {
     const bool is_blacklisted = strcmp(blacklist[i], ob_type_name) == 0;
     if (is_blacklisted) return false;
  }

  */
  /*
  bool is_whitelisted = false;
  for (int i = 0; i < 4; i++)
  {
     is_whitelisted |= strcmp(whitelist[i], ob_type_name) == 0;
  }
  if (!is_whitelisted) return false;
  */

  /*

  bool in_set = 
#if __cplusplus > 201703L
          interesting_records.contains(get_type_name(main_variant))
#else
	  interesting_records.find(get_type_name(main_variant)) != interesting_records.end()
#endif
	  ;

  in_set |=  
#if __cplusplus > 201703L
          interesting_records.contains(get_type_name(record))
#else
	  interesting_records.find(get_type_name(record)) != interesting_records.end()
#endif
	  ;


  log("is interesting struct ? %s\n", in_set ? "true" : "false");

  return in_set;

}

static bool is_interesting_struct_manual(const_tree);

static bool
is_interesting_struct (const_tree record_1)
{
  const bool use_escape_analysis_info = !flag_ipa_typelist_struct && !flag_ipa_typelist_field;
  return use_escape_analysis_info ? is_interesting_struct_escape_analysis(record_1) : is_interesting_struct_manual(record_1);
}

static bool
is_interesting_pair (const char* pair_name)
{
  const bool use_escape_analysis_info = !flag_ipa_typelist_struct && !flag_ipa_typelist_field;
  //TODO: If we are not in escape analysis we shouldn't be calling this function.
  //We need to define a better interface for specifying record type and name to avoid
  //having this problem.
  if (!use_escape_analysis_info) return true;
  const bool in_set = 
#if __cplusplus > 201703L
          record_field_pair_name.contains(pair_name);
#else
	  record_field_pair_name.find(pair_name) != record_field_pair_name.end()
#endif
	  ;

  log("is interesting pair? %s\n", in_set ? "true" : "false");

  return in_set;
}

static bool
is_interesting_struct_manual(const_tree record_1)
{
  is_record_p(record_1);

  const_tree record = TYPE_MAIN_VARIANT(record_1);
  is_record_p(record);

  const char *record_name = get_type_name (record);
  const int buffer_size = 1024;
  char interesting_structs[buffer_size];
  static const int cli_length = strlen (flag_ipa_typelist_struct);
  gcc_assert (cli_length < buffer_size);
  strcpy (interesting_structs, flag_ipa_typelist_struct);
  bool retval = false;
  char *save_ptr;
  char *token = strtok_r (interesting_structs, ",", &save_ptr);
  while (token)
    {
      retval |= strcmp (record_name, token) == 0;
      token = strtok_r (NULL, ",", &save_ptr);
      if (retval) break;
    }


  return retval;
}


inline static void
is_field_decl_p(const_tree field)
{
  gcc_assert(field);
  const enum tree_code code = TREE_CODE(field);
  const bool is_field = FIELD_DECL == code;
  gcc_assert(is_field);
}

static bool
is_interesting_field_manual (const_tree field)
{
  log ("HERE HERE %s\n", get_type_name(field));
  is_field_decl_p(field);

  const char *field_name = get_field_name (field);
  const int buffer_size = 1024;
  char interesting_fields[buffer_size];
  static const int cli_length = strlen (flag_ipa_typelist_field);
  gcc_assert (cli_length < buffer_size);
  strcpy (interesting_fields, flag_ipa_typelist_field);
  bool retval = false;
  char *save_ptr;
  char *token = strtok_r (interesting_fields, ",", &save_ptr);
  while (token)
    {
      retval |= strcmp (field_name, token) == 0;
      token = strtok_r (NULL, ",", &save_ptr);
    }
  return retval;
}

static bool
is_interesting_field_escape_analysis(const_tree field)
{
  is_field_decl_p(field);

  const char* field_name = get_field_name(field);
  */
  /*
  const char* whitelist [2] = { "nextin", "nextout" };

  bool is_whitelisted = false;
  for (int i = 0; i < 2; i++)
  {
     is_whitelisted |= strcmp(whitelist[i], field_name) == 0;
  }
  if (!is_whitelisted) return false;
  */

  /*
  const bool in_set = 
#if __cplusplus > 201703L
          interesting_fields.contains(field_name);
#else
	  interesting_fields.find(field_name) != interesting_fields.end()
#endif
	  ;

  log("is interesting field? %s\n", in_set ? "true" : "false");

  return in_set;
}

static bool
is_interesting_field (const_tree field)
{
  const bool use_escape_analysis_info = !flag_ipa_typelist_struct && !flag_ipa_typelist_field;
  return use_escape_analysis_info ? is_interesting_field_escape_analysis(field) : is_interesting_field_manual(field);
}

static bool
filter_out_boring_record (const_tree record, hash_set<const_tree> &map)
{
  test_log ("filter_out_boring_record", 0);
  enum tree_code code = TREE_CODE (record);
  gcc_assert (code == RECORD_TYPE);

  for (tree field = TYPE_FIELDS (record); field; field = DECL_CHAIN (field))
    {
      tree field_type = TREE_TYPE (field);
      enum tree_code field_code = TREE_CODE (field);
      test_log ("filter_out_boring_field %s %s", 0, get_type_name (field_type),
		get_tree_code_name (field_code));
      bool is_boring = filter_out_boring_type (field_type, map);
      if (!is_boring)
	return false;
    }

  return !is_interesting_struct (record);
}

static bool
filter_out_boring_pointer (const_tree pointer, hash_set<const_tree> &map)
{
  test_log ("filter_out_boring_pointer", 0);
  enum tree_code code = TREE_CODE (pointer);
  gcc_assert (code == POINTER_TYPE);
  const_tree base_type = get_base_type_from_pointer_type (pointer);
  return filter_out_boring_type (base_type, map);
}

static bool
filter_out_boring_array (const_tree array, hash_set<const_tree> &map)
{
  enum tree_code code = TREE_CODE (array);
  gcc_assert (code == ARRAY_TYPE);
  const_tree base_type = get_base_type_from_array_type (array);
  return filter_out_boring_type (base_type, map);
}

static bool
filter_out_boring_reference (const_tree ref, hash_set<const_tree> &map)
{
  gcc_unreachable();
  enum tree_code code = TREE_CODE (ref);
  gcc_assert (code == REFERENCE_TYPE);
  test_log ("in filter out boring reference", 0);
  bool retval = filter_out_boring_type (TREE_TYPE (ref), map);
  if (retval)
    {
      test_log ("we are getting a reference", 0);
    }
  return retval;
}

static bool
filter_out_boring_type (const_tree type, hash_set<const_tree> &map)
{
  if (!type)
    {
      test_log ("filter_out_boring_type (NULL)", 0);
      return true;
    }

  // maybe something like, if the type is
  // equivalent to one of the already
  // interesting types, then it should
  // also be interesting..?
  // But then, imagine this happens out of order...
  // We get type A which is non-interesting
  // type A is equivalent to B
  // then we receive type B which is interesting
  // this approach would fail...

  enum tree_code code = TREE_CODE (type);
  bool retval = true;
  switch (code)
    {
    case ARRAY_TYPE:
      retval = filter_out_boring_array (type, map);
      break;
    case POINTER_TYPE:
      retval = filter_out_boring_pointer (type, map);
      break;
    case RECORD_TYPE:
      retval = filter_out_boring_record (type, map);
      break;
    case REFERENCE_TYPE:
      retval = filter_out_boring_reference (type, map);
      break;
    default:
      {
	test_log ("default in tree code %s", 0, get_tree_code_name (code));
	retval = true;
      }
      break;
    }

  if (!retval)
    {
      test_log ("putting %s", 0, get_type_name (type));
      map.add (type);
    }
  return retval;
}

static bool
filter_var_decls (tree var_decl, hash_set<const_tree> &type_map)
{
  gcc_assert (var_decl);
  gcc_assert (TREE_CODE (var_decl) == VAR_DECL);
  tree type = TREE_TYPE (var_decl);
  return filter_out_boring_type (type, type_map);
}

static bool
filter_parm_decls (tree parm_decl, hash_set<const_tree> &type_map)
{
  gcc_assert (parm_decl);
  gcc_assert (TREE_CODE (parm_decl) == PARM_DECL);
  tree type = TREE_TYPE (parm_decl);
  return filter_out_boring_type (type, type_map);
}

static void
collect_parm_declarations (cgraph_node *cnode,
			   bool (*filter) (tree, hash_set<const_tree> &),
			   hash_set<const_tree> &decl_map)
{
  gcc_assert (cnode);
  // TODO: Do we need to get_untransformed_body_here?
  for (tree parm = DECL_ARGUMENTS (cnode->decl); parm; parm = DECL_CHAIN (parm))
    {
      bool filter_out = (*filter) (parm, decl_map);
      if (filter_out)
	continue;

      tree type = TREE_TYPE (parm);
      bool is_inside = decl_map.contains (type);
      // We already have this type.
      if (is_inside)
	continue;

      //const char *type_identifier = get_type_name (type);
      //test_log ("collecting,%s", 0, type_identifier);
      decl_map.add (type);
    }
}

static void
collect_pointer_plus (tree lhs, tree rhs1, tree rhs2,
		      hash_set<const_tree> &decl_map)
{
  tree lhs_type = lhs ? TREE_TYPE (lhs) : NULL;
  bool is_lhs_boring
    = lhs_type ? filter_out_boring_type (lhs_type, decl_map) : true;
  tree rhs1_type = rhs1 ? TREE_TYPE (rhs1) : NULL;
  bool is_rhs1_boring
    = rhs1_type ? filter_out_boring_type (rhs1_type, decl_map) : true;
  tree rhs2_type = rhs2 ? TREE_TYPE (rhs2) : NULL;
  bool is_rhs2_boring
    = rhs2_type ? filter_out_boring_type (rhs2_type, decl_map) : true;

  if (!is_lhs_boring)
    {
      test_log ("putting lhs %s", 0, get_type_name (lhs_type));
      decl_map.add (lhs_type);
    }
  if (!is_rhs1_boring)
    {
      test_log ("putting rhs1 %s", 0, get_type_name (rhs1_type));
      decl_map.add (rhs1_type);
    }
  if (!is_rhs2_boring)
    {
      test_log ("putting rhs2 %s", 0, get_type_name (rhs2_type));
      decl_map.add (rhs2_type);
    }
}

static void
collect_assign_rhs (gimple *stmt, hash_set<const_tree> &decl_map)
{
  enum tree_code code = gimple_expr_code (stmt);
  switch (code)
    {
    case POINTER_PLUS_EXPR:
    case POINTER_DIFF_EXPR:
    case COMPONENT_REF:
      {
	tree rhs2 = gimple_assign_rhs2 (stmt);
	tree rhs1 = gimple_assign_rhs1 (stmt);
	tree lhs = gimple_assign_lhs (stmt);
	collect_pointer_plus (lhs, rhs1, rhs2, decl_map);
      }
      break;
    default:
      {
	//print_gimple_stmt (dump_file, stmt, 0);
	test_log ("DEFAULT HERE: %s", 0, get_tree_code_name (code));
      }
      break;
    }
}

static void
collect_assign (gimple *stmt, hash_set<const_tree> &decl_map)
{
  gcc_assert (stmt);
  collect_assign_rhs (stmt, decl_map);
}

static void
collect_stmt (gimple *stmt, hash_set<const_tree> &decl_map)
{
  gcc_assert (stmt);
  const enum gimple_code code = gimple_code (stmt);
  switch (code)
    {
    case GIMPLE_ASSIGN:
      collect_assign (stmt, decl_map);
      break;
    default:
      break;
    }
  return;
}

static void
collect_basic_block (basic_block bb, hash_set<const_tree> &decl_map)
{
  gcc_assert (bb);
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      collect_stmt (stmt, decl_map);
    }
}

static void
collect_function_body (cgraph_node *cnode, hash_set<const_tree> &decl_map)
{
  gcc_assert (cnode);
  cnode->get_untransformed_body ();
  basic_block bb = NULL;
  function *func = DECL_STRUCT_FUNCTION (cnode->decl);
  push_cfun (func);
  FOR_EACH_BB_FN (bb, func)
    {
      collect_basic_block (bb, decl_map);
    }
  pop_cfun ();
  print_function (cnode);
}

static void
collect_local_declarations (cgraph_node *cnode,
			    bool (*filter) (tree, hash_set<const_tree> &),
			    hash_set<const_tree> &decl_map)
{
  gcc_assert (cnode);
  int i = 0;
  function *func = DECL_STRUCT_FUNCTION (cnode->decl);
  cnode->get_untransformed_body ();
  tree var_decl = NULL;
  FOR_EACH_LOCAL_DECL (func, i, var_decl)
    {
      bool filter_out = (*filter) (var_decl, decl_map);
      if (filter_out)
	continue;

      tree type = TREE_TYPE (var_decl);
      //const char *type_identifier = get_type_name (type);
      //test_log ("collecting,%s", 0, type_identifier);
      decl_map.add (type);
    }
}

static void
collect_global_declaration (varpool_node *vnode,
			    bool (*filter) (tree, hash_set<const_tree> &),
			    hash_set<const_tree> &decl_map)
{
  test_log ("collect global declarations", 0);
  int i;
  struct ipa_ref *ref;


  for (i = 0; vnode->iterate_referring (i, ref); i++)
    {
      bool filter_out = (*filter) (vnode->decl, decl_map);
      if (filter_out)
	{
	  test_log ("filter out", 0);
	  continue;
	}

      tree type = TREE_TYPE (vnode->decl);
      //const char *type_identifier = get_type_name (type);
      //test_log ("collecting,%s", 0, type_identifier);
      decl_map.add (type);
    }
}

static void
collect_global_declarations (bool (*filter) (tree,
					     hash_set<const_tree> &),
			     hash_set<const_tree> &decl_map)
{
  varpool_node *vnode;
  FOR_EACH_VARIABLE (vnode)
    {
      collect_global_declaration (vnode, filter, decl_map);
    }
}

static void
collect_orig_structs (hash_set<const_tree> &type_map)
{
  collect_global_declarations (&filter_var_decls, type_map);

  cgraph_node *cnode = NULL;
  FOR_EACH_DEFINED_FUNCTION (cnode)
    {
      print_function (cnode);
      collect_parm_declarations (cnode, &filter_parm_decls, type_map);
      collect_function_body (cnode, type_map);
      collect_local_declarations (cnode, &filter_var_decls, type_map);
    }
}

static const_tree
make_new_type_based_on_old (const_tree old,
			     t_map *mod_type_map);

static const_tree
get_new_type_main_variant (const_tree old, t_map *mod_type_map)
{
  const_tree old_type_main_variant = TYPE_MAIN_VARIANT(old);
  gcc_assert(old != old_type_main_variant);
  delete_info_t *ptr = mod_type_map->get (TYPE_MAIN_VARIANT(old_type_main_variant));
  if (ptr) {
     const_tree retval = TYPE_MAIN_VARIANT(ptr->new_record);
     return ptr->new_record;
  }

  const_tree new_main_variant_record = make_new_type_based_on_old(old_type_main_variant, mod_type_map);
  return new_main_variant_record;
}

static const_tree
make_new_record_based_on_typedef (const_tree _typedef, t_map *mod_type_map)
{
  log("make_new_record_based_on_typedef\n");
  bool is_typedef = _typedef != TYPE_MAIN_VARIANT(_typedef);
  gcc_assert(is_typedef);
  tree new_main_variant = (tree) get_new_type_main_variant(_typedef, mod_type_map);
  tree new_type = build_type_variant(new_main_variant, TYPE_READONLY(_typedef), TYPE_VOLATILE(_typedef));
  const char *new_name = make_record_name_based_on_old (_typedef);
  tree new_record_name = get_identifier (new_name);
  TYPE_NAME (new_type) = new_record_name;
  layout_type(new_type);

  delete_info_t* info = mod_type_map->get(TYPE_MAIN_VARIANT(_typedef));
  gcc_assert(info);
  delete_info_t new_info;
  new_info.old_record = _typedef;
  new_info.new_record = new_type;
  new_info.deleted_fields = info->deleted_fields;
  mod_type_map->put (_typedef, new_info);
  return new_type;
}

static const_tree
make_new_record_based_on_old (const_tree old,
			       t_map *mod_type_map)
{
  log("make_new_record_based_on_old\n %s", get_type_name(old));
  delete_info_t* info = mod_type_map->get(old);
  const_tree *new_record_ptr = info ? &(info->new_record) : NULL;

  gcc_assert (!new_record_ptr);

  bool is_typedef = old != TYPE_MAIN_VARIANT(old);
  if (is_typedef) { return make_new_record_based_on_typedef(old, mod_type_map); }

  bool will_definitely_change = is_interesting_struct (old);

  tree new_record = make_node (RECORD_TYPE);
  const char *new_name = make_record_name_based_on_old (old);
  tree new_record_name = get_identifier (new_name);
  TYPE_NAME (new_record) = new_record_name;
  hash_set<const_tree> *deleted_fields = new hash_set<const_tree> ();

  tree prev_new_field = NULL;
  for (tree old_field = TYPE_FIELDS (old); old_field;
       old_field = DECL_CHAIN (old_field))
    {

     const bool are_field_names_equal = is_interesting_field (old_field);
     log("am i deleting field %s ? %s\n", get_field_name(old_field), will_definitely_change && are_field_names_equal ? "true" : "false");
      if (will_definitely_change && are_field_names_equal)
	{
	  char *buffer;
	  char *buffer2;
          asprintf(&buffer, "%s.%s", get_type_name(old), get_field_name(old_field));
          asprintf(&buffer2, "%s.%s", get_type_name(TYPE_MAIN_VARIANT(old)), get_field_name(old_field));
	  log("buffer %s\n", buffer);
	  log("buffer2 %s\n", buffer2);
          if (is_interesting_pair(buffer) || is_interesting_pair(buffer2))
	  {
	  deleted_fields->add(old_field);
	  continue;
	  }
	}

      tree new_field = make_node (FIELD_DECL);
      DECL_NAME (new_field) = DECL_NAME (old_field);
      tree old_field_type = TREE_TYPE (old_field);
      const_tree new_field_type
	= make_new_type_based_on_old (TREE_TYPE (old_field), mod_type_map);
      test_log ("inside making new field, %s %s", 0,
		get_type_name (old_field_type), get_type_name (new_field_type));
      const_tree maybe_new_field
	= new_field_type == old_field ? old_field : new_field_type;
      substitute_type(new_field, maybe_new_field);
      DECL_SOURCE_LOCATION (new_field) = DECL_SOURCE_LOCATION (old_field);
      SET_DECL_ALIGN (new_field, DECL_ALIGN (old_field));
      DECL_USER_ALIGN (new_field) = DECL_ALIGN (old_field);
      TREE_ADDRESSABLE (new_field) = TREE_ADDRESSABLE (old_field);
      DECL_NONADDRESSABLE_P (new_field) = !TREE_ADDRESSABLE (old_field);
      TREE_THIS_VOLATILE (new_field) = TREE_THIS_VOLATILE (old_field);
      // DECL_FIELD_OFFSET and DECL_FIELD_BIT_OFFSET
      // are actually set by layout_type.
      const bool first_new_field = !prev_new_field;
      if (first_new_field)
	{
	  TYPE_FIELDS (new_record) = new_field;
	}
      else
	{
	  DECL_CHAIN (prev_new_field) = new_field;
	}

      prev_new_field = new_field;
    }

  layout_type (new_record);
  tree new_type = build_type_variant(new_record, TYPE_READONLY(old), TYPE_VOLATILE(old));
  layout_type (new_type);


  for (tree new_field = TYPE_FIELDS (new_record); new_field;
       new_field = DECL_CHAIN (new_field))
    {
      const char *new_field_name = get_field_name (new_field);
      int new_offset = get_field_offset (new_field);
      test_log ("offset,%s,%s,%d", 0, new_name, new_field_name, new_offset);
    }

  delete_info_t new_info;
  new_info.old_record = old;
  new_info.new_record = new_record;
  new_info.deleted_fields = deleted_fields;
  mod_type_map->put (old, new_info);
  return new_record;
}

const_tree
make_new_array_based_on_old (const_tree old_type,
			      t_map *mod_type_map)
{
  gcc_assert (TREE_CODE (old_type) == ARRAY_TYPE);
  delete_info_t *delete_info_1 = mod_type_map->get(old_type);
  const_tree *new_type_ptr = delete_info_1 ? &(delete_info_1->new_record) : NULL;
  gcc_assert (!new_type_ptr);

  const char *ptr = make_array_name_based_on_old (old_type);
  tree new_array = make_node (ARRAY_TYPE);
  TYPE_NAME (new_array) = get_identifier (ptr);
  TYPE_DOMAIN (new_array) = TYPE_DOMAIN (old_type);
  tree old_array_type = TREE_TYPE (old_type);
  const_tree possibly_new
    = make_new_type_based_on_old (old_array_type, mod_type_map);
  if (possibly_new == old_array_type)
    return old_type;
  substitute_type(new_array, possibly_new);
  layout_type (new_array);

  delete_info *ptr_2 = mod_type_map->get (old_type);
  if (ptr_2)
    return ptr_2->new_record;

  //HERE HERE
  // struct delete_info { const_tree old_record; const_tree new_record; field_set* deleted_fields;};
  delete_info_t delete_info;
  delete_info.old_record = old_type;
  delete_info.new_record = new_array;
  delete_info.deleted_fields = NULL;
  mod_type_map->put (old_type, delete_info);

  return new_array;
}

static const_tree
make_new_pointer_based_on_old (const_tree old,
			        t_map *mod_type_map)
{
  test_log ("make_new_pointer_based_on_old %s", 0, get_type_name (old));
  delete_info_t *delete_info_1 = mod_type_map->get(old);
  const_tree *already_computed = delete_info_1 ? &(delete_info_1->new_record) : NULL;
  gcc_assert (!already_computed);

  gcc_assert (TREE_CODE (old) == POINTER_TYPE);

  tree new_pointer = make_node (POINTER_TYPE);
  const char *ptr = make_pointer_name_based_on_old (old);

  TYPE_NAME (new_pointer) = get_identifier (ptr);
  SET_TYPE_MODE (new_pointer, TYPE_MODE (old));
  tree old_type = TREE_TYPE (old);
  gcc_assert (old_type);
  const_tree new_type = make_new_type_based_on_old (old_type, mod_type_map);
  // We do not need it...
  if (new_type == old_type)
    return old;

  substitute_type(new_pointer, new_type);
  layout_type (new_pointer);

  delete_info_t *ptr_2 = mod_type_map->get (old);
  if (ptr_2)
    return ptr_2->new_record;

  //HERE HERE
  // struct delete_info { const_tree old_record; const_tree new_record; field_set* deleted_fields;};
  delete_info_t delete_info;
  delete_info.old_record = old;
  delete_info.new_record = new_pointer;
  delete_info.deleted_fields = NULL;
  mod_type_map->put (old, delete_info);
  test_log ("putting %s %s", 0, get_type_name (old),
	    get_type_name (new_pointer));
  return new_pointer;
}

static const_tree
make_new_type_based_on_old (const_tree old,
			     t_map *mod_type_map)
{
  if (!old)
    return old;

  gcc_assert (old);
  delete_info_t *delete_info = mod_type_map->get (old);
  const_tree *new_base_ptr = delete_info ? &(delete_info->new_record) : NULL;
  if (new_base_ptr)
    return *new_base_ptr;

  switch (TREE_CODE (old))
    {
    case ARRAY_TYPE:
      return make_new_array_based_on_old (old, mod_type_map);
      break;
    case POINTER_TYPE:
      return make_new_pointer_based_on_old (old, mod_type_map);
      break;
    case RECORD_TYPE:
      return make_new_record_based_on_old (old, mod_type_map);
      break;
    default:
      return old;
      break;
    }
  gcc_unreachable ();
  return NULL;
}

// Non-null values for type_map
// will be the modified tree.
bool
compute_modified_structs_internal (
  const_tree const &old_type,
   t_map *mod_type_map)
{
  delete_info_t *ptr = mod_type_map->get (old_type);
  // already in map
  if (ptr)
    return true;

  const char *identifier = get_type_name (old_type);
  test_log ("modifying,%s", 0, identifier);

  const_tree new_record = make_new_type_based_on_old (old_type, mod_type_map);
  // We might have modified the mod_type_map inside make_new_type_based_on_old
  // and therefore we no longer have the need to store this there again...
  delete_info_t *ptr_2 = mod_type_map->get (old_type);
  if (ptr_2)
    return true;

  test_log ("new_name,%s", 0, get_type_name (new_record));
  //HERE HERE
  // struct delete_info { const_tree old_record; const_tree new_record; field_set* deleted_fields;};
  delete_info_t delete_info;
  delete_info.new_record = new_record;
  delete_info.old_record = old_type;
  delete_info.deleted_fields = NULL;
  mod_type_map->put (old_type, delete_info);
  return true;
}

static void
compute_modified_structs (hash_set<const_tree> &type_map,
			   t_map &mod_type_map)
{
  type_map.traverse< t_map *,
		    compute_modified_structs_internal> (&mod_type_map);
}

static bool
rewrite_constructor_def (tree expr,  t_map &type_map,
			 const int indent)
{
  // These nodes represent the brace initializers
  // for a structure or an array. They contain
  // a sequence of component values made out of a vector
  // of constructor_elt, which is a (INDEX, VALUE) pair.
  tree old_rhs_type = TREE_TYPE (expr);
  delete_info_t *delete_info = type_map.get (old_rhs_type);
  const_tree *new_rhs_type_ptr = delete_info ? &(delete_info->new_record) : NULL;
  if (!new_rhs_type_ptr)
    return false;

  substitute_type(expr, new_rhs_type_ptr);

  tree new_type = TREE_TYPE (expr);
  // If the TREE_TYPE of the CONSTRUCTOR is a RECORD_TYPE
  // UNION_TYPE or QUAL_UNION_TYPE, then the INDEX of each node
  // in the sequence will be a FIELD_DECL and the VALUE will be the
  // expression used to initialize that field.
  // If TREE_TYPE of the CONSTRUCTOR is an ARRAY_TYPE, then
  // the INDEX of each node in the sequence will be an INTEGER_CST
  // or a RANGE_EXPR of two INTEGER_CSTs.
  if (TREE_CLOBBER_P (expr))
    return true;

  enum tree_code code = TREE_CODE (new_type);
  bool is_record = code == RECORD_TYPE;
  bool is_union = code == UNION_TYPE;
  bool is_qual = code == QUAL_UNION_TYPE;
  bool is_array = code == ARRAY_TYPE;
  bool is_interesting = is_record || is_union || is_qual || is_array;
  if (!is_interesting)
    return true;

  // We have not tested this with other types...
  gcc_assert (is_record);
  // FIXME, we would need some time to rewrite the
  // API to allow gimple_stmt_iterator be passed
  // around all rewrite expressions...
  // or something like that.
  // Since not a lot of code uses this syntax,
  // let's avoid it for now. But make a note
  // we still need to fix this.
  gcc_unreachable ();

  unsigned i;
  tree val;
  FOR_EACH_CONSTRUCTOR_VALUE (CONSTRUCTOR_ELTS (expr), i, val)
    {
      rewrite_expr (val, type_map, indent + 4);
    }

  return true;
}

static bool
rewrite_field_decl_def (tree expr,  t_map &type_map,
			__attribute__((unused)) const int indent)
{
  tree old_rhs_type = TREE_TYPE (expr);
  delete_info_t *delete_info = type_map.get(old_rhs_type);
  const_tree *new_rhs_type_ptr = delete_info ? &(delete_info->new_record) : NULL;
  if (!new_rhs_type_ptr)
    return false;

  substitute_type(expr, new_rhs_type_ptr);
  gcc_assert (TREE_OPERAND (expr, 0) == NULL);
  return true;
}

static bool
rewrite_integer_cst_def (tree expr,  t_map &type_map,
			 __attribute__((unused)) const int indent)
{
  tree old_rhs_type = TREE_TYPE (expr);
  delete_info_t *delete_info = type_map.get (old_rhs_type);
  const_tree *new_rhs_type_ptr = delete_info ? &(delete_info->new_record) : NULL;
  if (!new_rhs_type_ptr)
    return false;

  substitute_type(expr, new_rhs_type_ptr);
  return true;
}

static void
rewrite_function_parms (tree expr,  t_map &type_map,
			const int indent);
static void
rewrite_function_return_type (tree expr,
			       t_map &type_map,
			      const int indent);

static bool
rewrite_function_decl_def (tree expr,
			    t_map &type_map,
			   const int indent)
{
  rewrite_function_parms (expr, type_map, indent + 4);
  rewrite_function_return_type (expr, type_map, indent + 4);
  return false;
}

static bool
rewrite_component_ref_def_lhs (gimple_stmt_iterator &gsi, tree expr, t_map &type_map, const int indent)
{
  gcc_assert (expr);
  tree expr_type = TREE_TYPE (expr);
  gcc_assert (expr_type);
  delete_info_t *delete_info1 = type_map.get (expr_type);
  delete_info1 = delete_info1 ? delete_info1 : type_map.get (TYPE_MAIN_VARIANT(expr_type));
  const_tree *new_expr_type_ptr = delete_info1 ? &(delete_info1->new_record) : NULL;

  tree _struct = TREE_OPERAND (expr, 0);
  gcc_assert (_struct);
  tree _struct_type = TREE_TYPE (_struct);


  gcc_assert (_struct_type);
  const enum tree_code code = TREE_CODE(_struct_type);
  const bool is_record = RECORD_TYPE == code;
  //TODO: Handle unions better
  switch (code)
  {
	  case UNION_TYPE: return false; break;
	  case RECORD_TYPE: break;
	  default: gcc_unreachable(); break;
  }
  gcc_assert(is_record);

  delete_info_t *delete_info = type_map.get (_struct_type);
  delete_info = delete_info ? delete_info : type_map.get (TYPE_MAIN_VARIANT(_struct_type));
  const_tree *new_struct_type_ptr = delete_info ? &(delete_info->new_record) : NULL;

  if (new_expr_type_ptr)
    substitute_type(expr, new_expr_type_ptr);

  bool retval = rewrite_expr (_struct, type_map, indent);

  tree new_type
    = new_struct_type_ptr ? const_to_tree(*new_struct_type_ptr) : _struct_type;
  tree field_decl = TREE_OPERAND (expr, 1);
  bool has_been_deleted = delete_info ? delete_info->deleted_fields->contains(field_decl) : false;
  if (has_been_deleted)
  {
          const char *field_name = get_field_name (field_decl);
	  test_log("we are removing field %s from struct %s", 0, field_name, get_type_name(delete_info->old_record));
	  return true;
  }


  const char *field_name = get_field_name (field_decl);
  if (strcmp (field_name, "anonymous") == 0)
    return false;


  const_tree new_field = get_field_with_name (new_type, field_name);
  // INFO: We need to rewrite the whole operand
  // not just its type.
  // INFO: When rewriting an operand we **must**
  // be in -flto-partition=none
  // INFO: setting the type and calling relayout_decl
  // doesn't work.
  // TODO: FIXME: In order to remove this assertion
  // you need to use clones.
  gcc_assert (in_lto_p && !flag_ltrans && !flag_wpa);
  TREE_OPERAND (expr, 1) = const_to_tree(new_field);
  int offset = get_field_offset (new_field);
  test_log ("rewrite,field_offset,%s,%d", indent, field_name, offset);
  gcc_assert (TREE_OPERAND (expr, 2) == NULL);

  return false;
}

static bool
rewrite_component_ref_def (tree expr,
			    t_map &type_map,
			   const int indent)
{
  gcc_assert (expr);
  tree expr_type = TREE_TYPE (expr);
  gcc_assert (expr_type);

  delete_info_t *delete_info1 = type_map.get (expr_type);
  const_tree *new_expr_type_ptr = delete_info1 ? &(delete_info1->new_record) : NULL;
  if (new_expr_type_ptr)
    substitute_type(expr, new_expr_type_ptr);

  tree _struct = TREE_OPERAND (expr, 0);
  gcc_assert (_struct);
  tree _struct_type = TREE_TYPE (_struct);
  gcc_assert (_struct_type);
  const enum tree_code code = TREE_CODE(_struct_type);
  const bool is_record = RECORD_TYPE == code;
  //TODO: Handle unions better
  switch (code)
  {
	  case UNION_TYPE: return false; break;
	  case RECORD_TYPE: break;
	  default: gcc_unreachable(); break;
  }
  gcc_assert(is_record);
  delete_info_t *delete_info = type_map.get (_struct_type);
  const_tree *new_struct_type_ptr = delete_info ? &(delete_info->new_record) : NULL;

  bool retval = rewrite_expr (_struct, type_map, indent);

  tree new_type
    = new_struct_type_ptr ? const_to_tree(*new_struct_type_ptr) : _struct_type;
  tree field_decl = TREE_OPERAND (expr, 1);
  bool has_been_deleted = delete_info ? delete_info->deleted_fields->contains(field_decl) : false;
  if (has_been_deleted)
  {
	  gcc_unreachable();
	  return false;
  }

  const char *field_name = get_field_name (field_decl);
  if (strcmp (field_name, "anonymous") == 0)
    return retval;

  const_tree new_field = get_field_with_name (new_type, field_name);
  // INFO: We need to rewrite the whole operand
  // not just its type.
  // INFO: When rewriting an operand we **must**
  // be in -flto-partition=none
  // INFO: setting the type and calling relayout_decl
  // doesn't work.
  // TODO: FIXME: In order to remove this assertion
  // you need to use clones.
  gcc_assert (in_lto_p && !flag_ltrans && !flag_wpa);
  TREE_OPERAND (expr, 1) = const_to_tree(new_field);
  int offset = get_field_offset (new_field);
  test_log ("rewrite,field_offset,%s,%d", indent, field_name, offset);
  gcc_assert (TREE_OPERAND (expr, 2) == NULL);

  return true;
}

static bool
rewrite_addr_expr_def (tree expr,  t_map &type_map,
		       const int indent)
{
  tree type = TREE_TYPE (expr);
  gcc_assert (TREE_CODE (expr) == ADDR_EXPR);
  delete_info_t *delete_info = type_map.get (type);
  const_tree *new_type_ptr = delete_info ? &(delete_info->new_record) : NULL;
  if (new_type_ptr)
    substitute_type(expr, new_type_ptr);
  tree op0 = TREE_OPERAND (expr, 0);
  bool retval = rewrite_expr (op0, type_map, indent);
  // There isn't an operand here, but we also can't
  // TREE_OPERAND(expr, 1)
  // Accessing this won't fail.
  return retval;
}

static bool
rewrite_var_decl_def (tree expr, __attribute__((unused))  t_map &type_map,
		      __attribute__((unused)) const int indent)
{
  gcc_assert (TREE_CODE (expr) == VAR_DECL);
  return false;
}

static bool
rewrite_mem_ref_def (tree expr,  t_map &type_map,
		     const int indent)
{
  // Do we need to modify this value so that we op1 and calculate a constant
  // value?
  gcc_assert (expr);
  gcc_assert (TREE_CODE (expr) == MEM_REF);
  tree type = TREE_TYPE (expr);

  delete_info_t *delete_info = type_map.get (type);
  delete_info = delete_info ? delete_info : type_map.get (TYPE_MAIN_VARIANT(type));
  const_tree *new_type_ptr = delete_info ? &(delete_info->new_record) : NULL;
  if (new_type_ptr)
    substitute_type(expr, new_type_ptr);

  tree op0 = TREE_OPERAND (expr, 0);
  // The first operand is the pointer being dereferenced
  bool retval = rewrite_expr (op0, type_map, indent);
  // The second operand is a pointer constant. Its type specifying the type
  // to be used for type-based alias analysis.
  tree op1 = TREE_OPERAND (expr, 1);
  rewrite_expr (op1, type_map, indent);
  gcc_assert (TREE_CODE (op1) == INTEGER_CST);
  int old_offset = tree_to_uhwi (op1);
  if (old_offset == 0 || !new_type_ptr)
    return retval;

  tree old_struct_size = TYPE_SIZE_UNIT (type);
  int old_type_size_int = tree_to_shwi (old_struct_size);
  test_log ("old_type = %s", 0, get_type_name (type));
  test_log ("old_type_size = %ld", 0, tree_to_shwi (old_struct_size));
  tree new_struct_size = TYPE_SIZE_UNIT (const_to_tree(*new_type_ptr));
  int new_type_size_int = tree_to_shwi (new_struct_size);
  int multiple = old_offset / old_type_size_int;
  int offset = multiple * new_type_size_int;
  int remainder = old_offset % old_type_size_int;
  // FIXME: Do I need a way of mapping old_offsets to new_offsets...
  // This seems insufficient.
  // I need to know at which offset the discontinuity happens so that
  // I can account for that.
  // for now, we can only guarantee this transformation if
  // old_offset is multiple of old_type_size_int;
  int new_offset = offset + remainder;
  if (new_offset == old_offset)
    return retval;

  tree new_offset_tree = build_int_cst (TREE_TYPE (op1), new_offset);
  TREE_OPERAND (expr, 1) = new_offset_tree;
  gcc_assert (old_offset % old_type_size_int == 0);

  // TREE_OPERAND(expr, 2) cannot be accessed.
  return retval;
}

static bool
rewrite_ssa_name_def (tree expr,  t_map &type_map,
		      __attribute__((unused)) const int indent)
{
  tree type = TREE_TYPE (expr);
  tree ssa_name_var = SSA_NAME_VAR (expr);
  delete_info_t *delete_info = type_map.get (type);
  const_tree *new_type_ptr = delete_info ? &(delete_info->new_record) : NULL;
  tree new_type_2 = NULL;
  if (new_type_ptr)
    {
      new_type_2 = build_type_variant(const_to_tree(*new_type_ptr), TYPE_READONLY(type), TYPE_VOLATILE(type));
      substitute_type(expr, new_type_2);
      // TODO: I had some cases where ssa_name_var was not set.
      // Not sure why...
      // I need to investigate
      // Could this be the variables that I create?
      // If so, then the temporary variables should have SSA_NAMES
      // and there is something else that I need to change
      if (ssa_name_var)
	{
	substitute_type(ssa_name_var, new_type_2);
	relayout_decl (ssa_name_var);
	}
    }

  return new_type_2;
}

static bool
rewrite_bin_expr_default_def (tree expr,
			       t_map &type_map,
			      const int indent)
{
  gcc_assert (expr);
  tree op0 = TREE_OPERAND (expr, 0);
  tree op1 = TREE_OPERAND (expr, 1);
  gcc_assert (op0 && op1);

  bool retval = false;
  retval |= rewrite_expr (op0, type_map, indent);
  retval |= rewrite_expr (op1, type_map, indent);
  // Why is this unreachable?
  gcc_unreachable ();
  gcc_assert (TREE_OPERAND (expr, 2) == NULL);

  return retval;
}


static bool
rewrite_array_ref_def (tree expr,  t_map &type_map,
		       const int indent)
{
  gcc_assert (expr);
  gcc_assert (TREE_CODE (expr) == ARRAY_REF);
  tree type = TREE_TYPE (expr);
  //const_tree *up_ptr = type_map.get (type);
  delete_info_t *delete_info = type_map.get (type);
  const_tree *up_ptr = delete_info ? &(delete_info->new_record) : NULL;
  if (up_ptr)
    substitute_type(expr, up_ptr);

  rewrite_expr (TREE_OPERAND (expr, 0), type_map, indent + 4);
  rewrite_expr (TREE_OPERAND (expr, 1), type_map, indent + 4);
  gcc_assert (TREE_OPERAND (expr, 2) == NULL);

  return true;
}

static void
rewrite_pointer_diff_def_rhs_variable_replace_constants_implementation (
  __attribute__((unused)) gimple_stmt_iterator &gsi, tree lhs, tree pointer, __attribute__((unused)) tree variable,
  const_tree old_type)
{
  // We will need this later
  tree old_type_size = TYPE_SIZE_UNIT (TREE_TYPE (old_type));
  int old_type_size_int = tree_to_shwi (old_type_size);
  // lhs = op0 - op1 // <-- we are here
  // ... SNIP ...
  // var = lhs / [ex] old_struct_size // <-- we want to be here
  //
  // Let's explore the uses of lhs

  gimple *stmt;
  imm_use_iterator iterator;
  FOR_EACH_IMM_USE_STMT (stmt, iterator, lhs)
    {
      // stmt is a use of lhs
      // gimple_expr_code is only valid for non-debug statements
      bool is_debug = is_gimple_debug (stmt);
      if (is_debug)
	continue;

      test_log ("is not debug", 0);
      enum tree_code code = gimple_expr_code (stmt);
      bool is_exact_div = code == EXACT_DIV_EXPR;
      if (!is_exact_div)
	continue;

      test_log ("is exact div expr", 0);
      tree divisor = gimple_op (stmt, 2);
      enum tree_code divisor_code = TREE_CODE (divisor);
      bool is_constant = divisor_code == INTEGER_CST;
      if (!is_constant)
	continue;

      test_log ("is constant", 0);
      int divisor_int = tree_to_shwi (divisor);
      test_log ("size %d %d", 0, divisor_int, old_type_size_int);
      bool is_same_size = divisor_int == old_type_size_int;
      if (!is_same_size)
	continue;

      const_tree new_base_type = TREE_TYPE (TREE_TYPE (pointer));
      tree new_struct_size_tree_1 = TYPE_SIZE_UNIT (new_base_type);
      int new_struct_size_int = tree_to_shwi (new_struct_size_tree_1);
      tree new_struct_size_const
	= build_int_cst (TREE_TYPE (divisor), new_struct_size_int);
      gimple_set_op (stmt, 2, new_struct_size_const);
    }
}

static tree
rewrite_pointer_plus_def_rhs_variable_replace_constants_implementation (
  __attribute__((unused)) gimple_stmt_iterator &gsi, tree lhs, __attribute__((unused)) tree pointer, tree variable,
  const_tree old_type)
{
  // We need to find the definition of the 3rd operand
  //   _1 = _0 * 72
  //   ... SNIP ...
  //   _2 = _1 + CONSTANT;
  //   ... SNIP ...
  //   _3 = &array + _2;  < -- this is where we are
  gcc_assert (TREE_CODE (variable) == SSA_NAME);
  gimple *def_for_variable = SSA_NAME_DEF_STMT (variable);

  // It is also possible that we are in a negation statement.
  // Example:
  //   _2 = _1 * 72;
  //   ... SNIP ...
  //   _3 = -_2;  < -- def_for_variable **might** be this stmt.
  //   ... SNIP ...
  //   _4 = &array + _3;
  // Let's find out how many operands we have
  unsigned num_operands = gimple_num_ops (def_for_variable);
  test_log ("how many operands ? %d", 0, num_operands);
  bool need_to_skip = num_operands == 2;
  gimple *to_change = need_to_skip
			? SSA_NAME_DEF_STMT (gimple_op (def_for_variable, 1))
			: def_for_variable;

  //   _2 = _1 * 72; <-- to change
  //   _3 = &array + _2;
  enum tree_code code = gimple_expr_code (to_change);
  const_tree old_base_type = TREE_TYPE (old_type);
  tree old_struct_size_tree_1 = TYPE_SIZE_UNIT (old_base_type);
  int old_struct_size_int = tree_to_shwi (old_struct_size_tree_1);
  const_tree pointer_to_unit_type = TREE_TYPE (lhs);
  const_tree unit_type = TREE_TYPE (pointer_to_unit_type);
  tree new_struct_size_tree_1 = TYPE_SIZE_UNIT (unit_type);
  int new_struct_size_int = tree_to_shwi (new_struct_size_tree_1);

  if (code == PLUS_EXPR)
    {
      // If we are here it is because we are adding an offset.
      // It is usually whenever we do somehting like
      //   _2 = _1 + CONSTANT; <-- to change
      //   _3 = &array + _2;
      gcc_assert (code == PLUS_EXPR);

      tree constant_plus = gimple_op (to_change, 2);
      gcc_assert (TREE_CODE (constant_plus) == INTEGER_CST);

      int old_unit_size_one_plus = tree_to_uhwi (constant_plus);
      // This used to work for unprofiled code.
      // However, with profiling, I guess we can jump wherever we want...
      // So, we need to find out which field we were intending to access
      // and where that field maps to the new struct...
      // gcc_assert(is_modulo);

      bool is_modulo_old = old_unit_size_one_plus % old_struct_size_int == 0;
      bool is_modulo_new = old_unit_size_one_plus % new_struct_size_int == 0;
      // If we are not in flag_profile_use
      // we will always be a modulo
      if (!flag_profile_use)
	gcc_assert (is_modulo_old);

      test_log ("old plus constant = %d", 0, old_unit_size_one_plus);
      test_log ("old struct size int = %d", 0, old_struct_size_int);
      test_log ("new struct size int = %d", 0, new_struct_size_int);
      test_log ("old_unit_size_one_plus modulo new_struct_pointer_int = %d", 0,
		old_unit_size_one_plus % new_struct_size_int);

      // FIXME:
      // The problem is that if flag_profile_use is enabled,
      // then we can potentially **predict** the offset
      // to be the correct one. But not always?
      // Therefore, we really do not have an idea if we are using
      // a value obtained from profiling or one from "correctness".
      if (flag_profile_use)
	{
	  bool is_from_profiling = is_modulo_new;
	  bool is_from_correctness = is_modulo_old;
	  gcc_assert (is_from_correctness || is_from_profiling);
	}

      int field_accessed_offset = old_unit_size_one_plus % old_struct_size_int;

      if (flag_profile_use && is_modulo_new)
	gcc_assert (field_accessed_offset != 0);

      int new_plus_constant_int
	= old_unit_size_one_plus / old_struct_size_int * new_struct_size_int
	  + field_accessed_offset;

      if (flag_profile_use && is_modulo_new)
	gcc_assert(field_accessed_offset % new_struct_size_int == 0);

      tree new_plus_constant
	= build_int_cst (TREE_TYPE (constant_plus), new_plus_constant_int);
      gimple_set_op (to_change, 2, new_plus_constant);

      // And now we need to get the defintion of _1
      variable = gimple_op (to_change, 1);
      gcc_assert (TREE_CODE (variable) == SSA_NAME);
      def_for_variable = SSA_NAME_DEF_STMT (variable);
      num_operands = gimple_num_ops (def_for_variable);
      test_log ("how many operands ? %d", 0, num_operands);
      need_to_skip = num_operands == 2;
      to_change = need_to_skip
		    ? SSA_NAME_DEF_STMT (gimple_op (def_for_variable, 1))
		    : def_for_variable;
      code = gimple_expr_code (to_change);
    }

  if (code == MULT_EXPR)
    {
      // How do we know what type we need to change?
      // That is _3 (or lhs) is the result of the pointer arithmetic.
      // So we need one level of dereference in order to find
      // the underlying type.

      // We need to get the second operand
      test_print_gimple_stmt (to_change);
      tree constant = gimple_op (to_change, 2);
      gcc_assert (TREE_CODE (constant) == INTEGER_CST);

      // We should have the same size...
      // Is this always true?
      // TODO: can we enable this assertion?
      // int old_unit_size_one = tree_to_shwi (constant);
      // gcc_assert(old_unit_size_one == old_struct_size_int);

      // Now we need to modify the second operand...
      tree new_constant
	= build_int_cst (TREE_TYPE (constant), new_struct_size_int);
      gimple_set_op (to_change, 2, new_constant);
    }

  return NULL;
}

static tree
rewrite_pointer_plus_def_rhs_integer_constant (tree pointer,
					       tree integer_constant,
					       const_tree old_type)
{
  gcc_assert (TREE_CODE (integer_constant) == INTEGER_CST);
  tree pointer_type = TREE_TYPE (pointer);
  gcc_assert (TREE_CODE (pointer_type) == POINTER_TYPE);

  const_tree base_type = TREE_TYPE (TREE_TYPE (pointer));

  tree size = TYPE_SIZE_UNIT (base_type);
  int new_struct_size = tree_to_shwi (size);
  int old_offset = tree_to_uhwi (integer_constant);

  const_tree old_base_type = TREE_TYPE (old_type);

  tree old_type_size = TYPE_SIZE_UNIT (old_base_type);
  int old_struct_size = tree_to_shwi (old_type_size);
  int modulo = old_offset % old_struct_size;
  test_log ("old_struct_size = %d", 0, old_struct_size);

  tree integer_constant_type = TREE_TYPE (integer_constant);

  int new_offset = old_offset / old_struct_size * new_struct_size;
  tree new_constant = build_int_cst (integer_constant_type, new_offset);
  return new_constant;
}

static bool
rewrite_pointer_plus_def_rhs (gimple *stmt, gimple_stmt_iterator &gsi, tree lhs,
			      tree op0, tree op1,
			       t_map &type_map,
			       t_map &inverse)
{
  gcc_assert (op0 && op1);
  tree op0_type = TREE_TYPE (lhs);
  //const_tree *new_op0_type = type_map.get (op0_type);
  delete_info_t *delete_info = type_map.get (op0_type);
  const_tree *new_op0_type = delete_info ? &(delete_info->new_record) : NULL;
  test_log ("rewrite_pointer_plus has old_type %s", 0,
	    new_op0_type ? "true" : "false");
  // gcc_assert(!new_op0_type);
  test_log ("rewrite_pointer_plus op0_type %s", 0, get_type_name (op0_type));
  if (new_op0_type)
    test_log ("rewrite_pointer_plus new_op0_type %s", 0,
	      get_type_name (*new_op0_type));

  rewrite_expr (op0, type_map, 0);
  rewrite_expr (op1, type_map, 0);

  const char *new_type_name
    = new_op0_type ? get_type_name (*new_op0_type) : get_type_name (op0_type);

  test_log ("rewrite_pointer_plus new_type_name %s", 0, new_type_name);
  //const_tree *inverse_type_ptr = inverse.get (op0_type);
  delete_info_t *delete_info_inverse = inverse.get(op0_type);
  const_tree *inverse_type_ptr = delete_info_inverse ? &(delete_info_inverse->old_record) : NULL;
  bool not_in_map_nor_inverse = !new_op0_type && !inverse_type_ptr;
  test_log ("continuing? %s", 0, not_in_map_nor_inverse ? "false" : "true");
  if (not_in_map_nor_inverse)
    return false;

  const_tree old_type
    = new_op0_type ? (const_tree) op0_type : *inverse_type_ptr;
  test_log ("what is the old_type name ? %s", 0, get_type_name (old_type));

  bool is_op1_int_cst = TREE_CODE (op1) == INTEGER_CST;
  tree integer_constant = is_op1_int_cst ? op1 : op0;
  bool has_integer_constant = (TREE_CODE (integer_constant) == INTEGER_CST);

  if (has_integer_constant)
  {
    // I want to know if this is an invariant.
    // TODO: Outline
    gcc_assert (TREE_CODE (op1) == INTEGER_CST);
    tree pointer = is_op1_int_cst ? op0 : op1;
    tree new_constant
	  = rewrite_pointer_plus_def_rhs_integer_constant (pointer,
							   integer_constant,
							   old_type);
    unsigned int operand = is_op1_int_cst ? 2 : 1;
    gimple_set_op (stmt, operand, new_constant);
    return true;
  }

	tree variable = op1;
	tree pointer = op0;
	// I want to know if this is an invariant.
	tree pointer_type = TREE_TYPE (pointer);
	gcc_assert (TREE_CODE (pointer_type) == POINTER_TYPE);
	tree new_variable
	  = rewrite_pointer_plus_def_rhs_variable_replace_constants_implementation (
	    gsi, lhs, pointer, variable, old_type);
	if (!new_variable)
	  return false;
	unsigned int operand
	  = TREE_CODE (TREE_TYPE (op0)) == POINTER_TYPE ? 2 : 1;
	gimple_set_op (stmt, operand, new_variable);

  return true;
}

static bool
rewrite_pointer_diff_def_rhs (gimple_stmt_iterator &gsi, tree lhs,
			      tree op0, tree op1,
			       t_map &type_map,
			       t_map &inverse)
{
  gcc_assert (op0 && op1);
  tree op0_type = TREE_TYPE (lhs);
  //const_tree *new_op0_type = type_map.get (op0_type);
  delete_info_t *delete_info = type_map.get(op0_type);
  const_tree *new_op0_type = delete_info ? &(delete_info->new_record) : NULL;
  test_log ("rewrite_pointer_diff has old_type %s", 0,
	    new_op0_type ? "true" : "false");
  test_log ("rewrite_pointer_diff op0_type %s", 0, get_type_name (op0_type));
  if (new_op0_type)
    test_log ("rewrite_pointer_diff new_op0_type %s", 0,
	      get_type_name (*new_op0_type));

  rewrite_expr (op0, type_map, 0);
  rewrite_expr (op1, type_map, 0);

  const char *new_type_name
    = new_op0_type ? get_type_name (*new_op0_type) : get_type_name (op0_type);
  // TODO: I'd prefer to have a parameter
  // that tells me what to change
  // several stack frames above.
  test_log ("rewrite_pointer_diff new_type_name %s", 0, new_type_name);
  //const_tree *inverse_type_ptr = inverse.get (op0_type);
  delete_info_t *delete_info_inverse = inverse.get (op0_type);
  const_tree *inverse_type_ptr = delete_info_inverse ? &(delete_info_inverse->old_record) : NULL;
  bool not_in_map_nor_inverse = !new_op0_type && !inverse_type_ptr;
  test_log ("continuing? %s", 0, not_in_map_nor_inverse ? "false" : "true");
  if (not_in_map_nor_inverse)
    return false;

  const_tree old_type
    = new_op0_type ? (const_tree) op0_type : *inverse_type_ptr;

  bool is_op1_int_cst = TREE_CODE (op1) == INTEGER_CST;
  tree integer_constant = is_op1_int_cst ? op1 : op0;
  bool has_integer_constant = (TREE_CODE (integer_constant) == INTEGER_CST);

  // I want to know if this is an invariant.
  if (has_integer_constant)
    gcc_unreachable ();

  // I want to know if this is an invariant.
  gcc_assert (TREE_CODE (op0_type) == POINTER_TYPE);
  rewrite_pointer_diff_def_rhs_variable_replace_constants_implementation (
    gsi, lhs, op0, op1, old_type);

  return true;
}
static bool
rewrite_assign_rhs (gimple *stmt, gimple_stmt_iterator &gsi,
		     t_map &type_map,
		     t_map &inverse)
{
  // WONTFIX, It looks like there is no way to obtain
  // the expression for the rhs. So instead, we have to do
  // this hack.
  // Which expressions can be toplevel?
  bool is_stmt_rewritten = false;
  enum tree_code code = gimple_expr_code (stmt);
  tree lhs = gimple_assign_lhs (stmt);
  switch (TREE_CODE(lhs))
  {
	  case COMPONENT_REF:
		  test_log(" I AM HERE HERE HERE", 0);
	break;
	  default:
	break;
  }

  switch (code)
    {
    case POINTER_PLUS_EXPR:
      {
	tree rhs2 = gimple_assign_rhs2 (stmt);
	tree rhs1 = gimple_assign_rhs1 (stmt);
	tree lhs = gimple_assign_lhs (stmt);
	tree lhs_type = TREE_TYPE (lhs);
	gcc_assert (lhs_type);
	delete_info_t *delete_info = type_map.get (lhs_type);
	gcc_assert (lhs && rhs2 && rhs1);
	bool retval = rewrite_pointer_plus_def_rhs (stmt, gsi, lhs, rhs1, rhs2,
						    type_map, inverse);
	if (delete_info)
	  substitute_type(lhs, delete_info->new_record);
	return retval;
      }
    case POINTER_DIFF_EXPR:
      {
	tree rhs2 = gimple_assign_rhs2 (stmt);
	tree rhs1 = gimple_assign_rhs1 (stmt);
	tree lhs = gimple_assign_lhs (stmt);
	tree lhs_type = TREE_TYPE (lhs);
	gcc_assert (lhs_type);
	delete_info_t *delete_info = type_map.get (lhs_type);
	gcc_assert (lhs && rhs2 && rhs1);
	bool retval = rewrite_pointer_diff_def_rhs (gsi, lhs, rhs1, rhs2,
						    type_map, inverse);
	if (delete_info)
	  substitute_type(lhs, delete_info->new_record);
	return retval;
      }
      break;
    default:
      {
	//print_gimple_stmt (dump_file, stmt, 0);
	test_log ("DEFAULT HERE: %s", 0, get_tree_code_name (code));
      }
      break;
    }

  switch (gimple_assign_rhs_class (stmt))
    {
    case GIMPLE_TERNARY_RHS:
      {
	tree rhs3 = gimple_assign_rhs3 (stmt);
	is_stmt_rewritten |= rewrite_expr (rhs3, type_map, 0);
      }
    case GIMPLE_BINARY_RHS:
      {
	tree rhs2 = gimple_assign_rhs2 (stmt);
	is_stmt_rewritten |= rewrite_expr (rhs2, type_map, 0);
      }
    case GIMPLE_SINGLE_RHS:
    case GIMPLE_UNARY_RHS:
      {
	tree rhs1 = gimple_assign_rhs1 (stmt);
	is_stmt_rewritten |= rewrite_expr (rhs1, type_map, 0);
      }
      break;
    default:
      gcc_unreachable ();
      break;
    }

  return is_stmt_rewritten;
}

static void
rewrite_call_lhs (gimple *stmt, __attribute__((unused)) gimple_stmt_iterator &gsi,
		   t_map &type_map)
{
  gcc_assert (stmt);
  test_print_gimple_stmt (stmt);
  tree lhs = gimple_call_lhs (stmt);
  // LHS can be null. Example `foo()`
  if (!lhs)
    return;

  rewrite_expr (lhs, type_map, 0);
}

static void
rewrite_call_rhs (gimple *stmt, __attribute__((unused)) gimple_stmt_iterator &gsi,
		   t_map &type_map)
{
  gcc_assert (stmt);
  tree fn = gimple_call_fn (stmt);
  gcc_assert (fn);
  gcall *call = dyn_cast<gcall *> (stmt);
  tree return_type = gimple_call_return_type (call);
  test_log ("what is the return type %s", 0, get_type_name (return_type));
  gcc_assert (return_type);

  unsigned args = gimple_call_num_args (stmt);
  for (unsigned i = 0; i < args; i++)
    {
      tree arg = gimple_call_arg (stmt, i);
      rewrite_expr (arg, type_map, 0);
    }
}

static void
rewrite_call (gimple *stmt, gimple_stmt_iterator &gsi,
	       t_map &type_map)
{
  rewrite_call_lhs (stmt, gsi, type_map);
  rewrite_call_rhs (stmt, gsi, type_map);
}

static void
rewrite_cond (gimple *stmt, __attribute((unused)) gimple_stmt_iterator &gsi,
	       t_map &type_map)
{
  tree lhs = gimple_cond_lhs (stmt);
  tree rhs = gimple_cond_rhs (stmt);
  rewrite_expr (lhs, type_map, 0);
  rewrite_expr (rhs, type_map, 0);
}

static void
rewrite_phi (gimple *stmt, __attribute__((unused)) gimple_stmt_iterator &gsi,
	      t_map &type_map)
{
  test_log ("rewriting phi result", 0);
  tree ssa_result = gimple_phi_result (stmt);
  rewrite_expr (ssa_result, type_map, 0);
  unsigned args = gimple_phi_num_args (stmt);
  for (unsigned i = 0; i < args; i++)
    {
      test_log ("rewriting phi arg", 0);
      tree arg = gimple_phi_arg_def (stmt, i);
      rewrite_expr (arg, type_map, 0);
    }
}

static bool
rewrite_assign_lhs (gimple *stmt, gimple_stmt_iterator &gsi, t_map &type_map, t_map &inverse)
{
   tree lhs = gimple_assign_lhs(stmt);
   enum tree_code code = TREE_CODE(lhs);
   bool is_component_ref = code == COMPONENT_REF;
   if (is_component_ref)
   {
      return rewrite_component_ref_def_lhs(gsi, lhs, type_map, 0);
   }

   rewrite_expr(lhs, type_map, 0);
   return false;
}

static bool
rewrite_assign (gimple *stmt, gimple_stmt_iterator &gsi,
		 t_map &type_map,
		 t_map &inverse)
{
  gcc_assert (stmt);
  test_print_gimple_stmt (stmt);
  bool deleted = rewrite_assign_lhs (stmt, gsi, type_map, inverse);
  if (deleted) { 
	  unlink_stmt_vdef (stmt);
	  gsi_remove(&gsi, true); 
	  return deleted;
  }
  rewrite_assign_rhs (stmt, gsi, type_map, inverse);
  test_print_gimple_stmt (stmt);
  return false;
}

static bool
rewrite_return (gimple *stmt, gimple_stmt_iterator &gsi,
		 t_map &type_map)
{
  gcc_assert (stmt);
  test_print_gimple_stmt (stmt);
  greturn *retisn = dyn_cast<greturn *> (stmt);
  gcc_assert(retisn);
  tree retval = gimple_return_retval(retisn);
  if (!retval) return false;

  tree decl = DECL_RESULT (current_function_decl);
  rewrite_expr(decl, type_map, 0);

  rewrite_expr (retval, type_map, 0);
  tree retval_type = TREE_TYPE(retval);
  gcc_assert(retval_type);
  log("return type %s\n", get_type_name(retval_type));
  return false;
}

static bool
rewrite_stmt (gimple *stmt, gimple_stmt_iterator &gsi,
	       t_map &type_map,
	       t_map &inverse)
{
  gcc_assert (stmt);
  const enum gimple_code code = gimple_code (stmt);
  bool deleted = false;
  switch (code)
    {
    case GIMPLE_ASSIGN:
      deleted = rewrite_assign (stmt, gsi, type_map, inverse);
      break;
    case GIMPLE_CALL:
      rewrite_call (stmt, gsi, type_map);
      break;
    case GIMPLE_COND:
      rewrite_cond (stmt, gsi, type_map);
      break;
    case GIMPLE_PHI:
      rewrite_phi (stmt, gsi, type_map);
      break;
    case GIMPLE_RETURN:
      rewrite_return (stmt, gsi, type_map);
      break;
    default:
      {
	const char *const gimple_code_str = gimple_code_name[code];
	test_log ("gimple_code %s", 0, gimple_code_str);
	test_print_gimple_stmt (stmt);
      }
      break;
    }
  return deleted;
}

static void
rewrite_basic_block (basic_block bb,  t_map &type_map,
		      t_map &inverse)
{
  gcc_assert (bb);
  bool is_first = true;
  for (gimple_stmt_iterator gsi = gsi_start_bb (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      bool deleted = rewrite_stmt (stmt, gsi, type_map, inverse);

      if (deleted && !is_first)
      {
	      gsi_prev(&gsi);
      }

      while (deleted && is_first)
      {
	      stmt = gsi_stmt(gsi);
	      if (!stmt) break; // we might have deleted and was end of basic block?
	      deleted = rewrite_stmt (stmt, gsi, type_map, inverse);
	      if (gsi_end_p(gsi)) break;
      }

      if (deleted && gsi_end_p (gsi)) break;

      is_first = false;
    }
  for (gimple_stmt_iterator gsi = gsi_start_phis (bb); !gsi_end_p (gsi);
       gsi_next (&gsi))
    {
      gimple *stmt = gsi_stmt (gsi);
      rewrite_stmt (stmt, gsi, type_map, inverse);
    }
}

static void
rewrite_function_body (cgraph_node *cnode,
		        t_map &type_map,
		        t_map &inverse)
{
  gcc_assert (cnode);
  const char* name_s = cnode->name();
  const char* dot = strstr(name_s, "."); // Updated functions have a dot on them.
  // If there's a dot, we need to do a memcpy 
  // of the string name[0:dot]
  size_t n = dot ? dot - name_s : strlen(name_s);
  char precanon[100];
  const char* canonical = dot ? strncpy(precanon, name_s, n) : name_s;
  bool already_processed = updated_functions.find(name_s) != updated_functions.end();
  log("original name %s\n", name_s);
  log("canonical name %s\n", canonical);
  log("already processed ? %s\n", already_processed ? "true" : "false");
  if (already_processed) return;

  updated_functions.insert(name_s);
  cnode->get_untransformed_body ();
  basic_block bb = NULL;
  function *func = DECL_STRUCT_FUNCTION (cnode->decl);
  push_cfun (func);
  FOR_EACH_BB_FN (bb, func)
    {
      rewrite_basic_block (bb, type_map, inverse);
    }
  // TODO:
  // can we move this logic into the above?
  // Thanks!
  size_t i;
  tree name;
  FOR_EACH_SSA_NAME (i, name, cfun)
  {
    if (SSA_NAME_VAR (name) != NULL_TREE
	&& TREE_TYPE (name) != TREE_TYPE (SSA_NAME_VAR (name)))
      {
	TREE_TYPE (name) = TREE_TYPE (SSA_NAME_VAR (name));
	test_log ("changing to %s", 0,
		  get_type_name (TREE_TYPE (SSA_NAME_VAR (name))));
      }
  }
  pop_cfun ();
  print_function (cnode);
}

static void
rewrite_local_decl (tree var_decl,  t_map &type_map)
{
  gcc_assert (var_decl);
  gcc_assert (TREE_CODE (var_decl) == VAR_DECL);
  const_tree type = TREE_TYPE (var_decl);
  gcc_assert (type);
  delete_info_t *delete_info = type_map.get (type);

  if (!delete_info)
    return;


  const_tree new_type = delete_info->new_record;
  gcc_assert (new_type);
  test_write ("rewriting,local_decl");
  test_print_generic_decl (var_decl);
  test_write (",");
  //TODO: Doing an experiment, we might need to keep qualifications...
  substitute_type(var_decl, new_type);
  
  // Keep this relayout_decl
  relayout_decl (var_decl);
  test_print_generic_decl (var_decl);
  test_write ("\n");
}

static void
rewrite_local_decls (cgraph_node *cnode,
		      t_map &type_map)
{
  gcc_assert (cnode);
  tree decl = cnode->decl;
  gcc_assert (decl);
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL);
  function *func = DECL_STRUCT_FUNCTION (decl);
  gcc_assert (func);
  tree var_decl = NULL;
  int i = 0;
  FOR_EACH_LOCAL_DECL (func, i, var_decl)
    {
      rewrite_local_decl (var_decl, type_map);
    }
  print_function (cnode);
}

static void
rewrite_function_parms (tree expr,  t_map &type_map,
			const int indent)
{
  enum tree_code code = TREE_CODE (expr);
  bool is_function_decl = code == FUNCTION_DECL;
  gcc_assert (is_function_decl);

  for (tree parm = DECL_ARGUMENTS (expr); parm; parm = DECL_CHAIN (parm))
    {
      tree parm_type = TREE_TYPE (parm);
      delete_info_t *delete_info = type_map.get (parm_type);
      if (!delete_info)
	continue;

      const_tree new_type = delete_info->new_record;
      test_log ("before rewrite_function_arg %s", indent, get_type_name (parm_type));
      substitute_type(parm, new_type);
      relayout_decl (parm);
      tree parm_type_2 = TREE_TYPE (parm);
      test_log ("after rewrite_function_arg %s", indent,
		get_type_name (parm_type_2));
    }
}

static void
rewrite_function_parms (cgraph_node *cnode,
			 t_map &type_map)
{
  rewrite_function_parms (cnode->decl, type_map, 0);
}

static void
rewrite_function_return_type (tree expr,
			       t_map &type_map,
			      const int indent)
{
  enum tree_code code = TREE_CODE (expr);
  bool is_function_decl = code == FUNCTION_DECL;
  gcc_assert (is_function_decl);

  tree function_type = TREE_TYPE (expr);

  // TODO: We do not support method's yet.
  gcc_assert (TREE_CODE (function_type) == FUNCTION_TYPE);
  tree function_return_type = TREE_TYPE (function_type);
  gcc_assert (function_return_type);

  test_log ("before rewrite_function_return_type %s", indent,
	    get_type_name (function_return_type));
  delete_info_t *delete_info = type_map.get (function_return_type);
  if (!delete_info)
    return;

  const_tree new_type = delete_info->new_record;
  tree new_type_2 = build_type_variant(const_to_tree(new_type), TYPE_READONLY(function_return_type), TYPE_VOLATILE(function_return_type));
  substitute_type(function_type, new_type_2);
  // Do not use relayout_decl(cnode->decl);
  tree function_type_2 = TREE_TYPE (expr);
  tree function_return_type_2 = TREE_TYPE (function_type_2);
  test_log ("after rewrite_function_return_type %s", indent,
	    get_type_name (function_return_type_2));
}

static void
rewrite_function_return_type (cgraph_node *cnode,
			       t_map &type_map)
{
  gcc_assert (TREE_CODE (cnode->decl) == FUNCTION_DECL);
  tree function_type = TREE_TYPE (cnode->decl);
  gcc_assert (function_type);
  rewrite_function_return_type (cnode->decl, type_map, 0);
}

static void
rewrite_global_decl (varpool_node *vnode,
		      t_map &type_map)
{
  int i;
  struct ipa_ref *ref;

  for (i = 0; vnode->iterate_referring (i, ref); i++)
    {
      test_log ("rewriting global declaration", 0);
      rewrite_local_decl (vnode->decl, type_map);
      test_print_generic_decl (vnode->decl);
    }
}

static void
rewrite_global_decls ( t_map &type_map)
{
  varpool_node *vnode;
  FOR_EACH_VARIABLE (vnode)
    {
      rewrite_global_decl (vnode, type_map);
    }
}

static void
rewrite_function (cgraph_node *cnode,
		   t_map &type_map,
		   t_map &inverse)
{
  // rewrite_function_body should be before
  // rewrite_local_decls to make work easier.
  // Why? Because if we rewrite local decl types
  // first, then we might need to look for
  // old AND new types in rewrite_function_body.
  // If we rewrite local decl types later,
  // then in rewrite_function_body we only
  // need to look for old types.
  rewrite_function_parms (cnode, type_map);
  rewrite_function_return_type (cnode, type_map);
  rewrite_function_body (cnode, type_map, inverse);
  rewrite_local_decls (cnode, type_map);
}

static void
rewrite_functions ( t_map &type_map,
		    t_map &inverse)
{
  cgraph_node *cnode = NULL;
  FOR_EACH_DEFINED_FUNCTION (cnode)
    {
      rewrite_function (cnode, type_map, inverse);
    }
}

static void
rewrite_references_to_modified_structs (
   t_map &type_map,
   t_map &inverse)
{
  rewrite_functions (type_map, inverse);
  rewrite_global_decls (type_map);
}

bool
compute_inverse_type_map_internal (
  const_tree const &old_type, delete_info_t *delete_info,
   t_map *inverse_map)
{
  gcc_assert (inverse_map && delete_info);
  const_tree new_type = delete_info->new_record;
  inverse_map->put (delete_info->new_record, *delete_info);
  const char *old_identifier = get_type_name (old_type);
  const char *new_identifier = get_type_name (new_type);
  test_log ("inverse,%s,%s", 0, new_identifier, old_identifier);
  return true;
}

static void
compute_inverse_type_map ( t_map &mod_type_map,
			   t_map &inverse_map)
{
  mod_type_map.traverse< t_map *,
			compute_inverse_type_map_internal> (&inverse_map);
}


static void
separate_set_of_pairs_into_pair_of_sets(const record_field_set &set_of_pairs, std::set<const char*, cstrless> &one, std::set<const char*, cstrless> &two)
{
  
  for (auto it = set_of_pairs.cbegin(); it != set_of_pairs.cend() ; ++it)
  {
    fields record_field_pair = *it;
    const_tree record = record_field_pair.first;
    if (strcmp(get_record_name(record), "<name_tree_is_null>") == 0) continue;
    const_tree field = record_field_pair.second;
    one.insert(get_type_name(record));
    one.insert(get_type_name(TYPE_MAIN_VARIANT(record)));
    two.insert(get_field_name(field));
    char *buffer;
    char *buffer2;
    asprintf(&buffer, "%s.%s", get_type_name(record), get_field_name(field));
    asprintf(&buffer2, "%s.%s", get_type_name(TYPE_MAIN_VARIANT(record)), get_field_name(field));
    log("putting in %s\n", buffer);
    log("putting in %s\n", buffer2);
    record_field_pair_name.insert(buffer);
    record_field_pair_name.insert(buffer2);
  }
}

static void
iphw_execute_escape_analysis()
{
  gcc_assert(!flag_ipa_typelist_struct && !flag_ipa_typelist_field);
  const record_field_set to_reorg = get_fields_to_reorg();
  separate_set_of_pairs_into_pair_of_sets(to_reorg, interesting_records, interesting_fields);

  hash_set<const_tree> orig_type_map;
  collect_orig_structs (orig_type_map);
  t_map mod_type_map;
  compute_modified_structs (orig_type_map, mod_type_map);
  t_map inverse_type_map;
  compute_inverse_type_map (mod_type_map, inverse_type_map);
  rewrite_references_to_modified_structs (mod_type_map, inverse_type_map);
}

static void
iphw_execute_cli_args()
{
  gcc_assert(flag_ipa_typelist_struct && flag_ipa_typelist_field);

  hash_set<const_tree> orig_type_map;
  collect_orig_structs (orig_type_map);

  // So what would a client will have to implement?
  // * Pass a set of types
  // * A function that translates old_types to new_types
  // class TypeRewriter {
  //   // A set might not be 
  //   // sufficient to say **how** the types should
  //   // be modified
  //   TypeRewriter(set, function type -> new_type);
  //   ~TypeRewriter(); // allows me to free memory used during
  //                    // the pass easily
  //   rewrite(stmt); // maybe no since it can corrupt the function?
  //   rewrite(bb);   // maybe no since it can corrupt the function?
  //   rewrite(func);
  //   rewrite(set(functions));
  // }
  t_map mod_type_map;
  compute_modified_structs (orig_type_map, mod_type_map);
  t_map inverse_type_map;
  compute_inverse_type_map (mod_type_map, inverse_type_map);
  rewrite_references_to_modified_structs (mod_type_map, inverse_type_map);
}

static unsigned int
iphw_execute ()
{
  test_log ("Executing structreorg", 0);
  const bool perform_escape_analysis = !flag_ipa_typelist_struct && !flag_ipa_typelist_field ;
  perform_escape_analysis ? iphw_execute_escape_analysis() : iphw_execute_cli_args();
  return 0;
}

int
str_reorg_dead_field_eliminate (__attribute__((unused)) Info *info)
{
  gcc_assert (in_lto_p);
  return iphw_execute ();
}

*/

#if USE_NEW_INTERFACE
int
str_reorg_dead_field_eliminate_qual (Info *info)
{
  //GimpleCaster caster(info->sets);
  //caster.walk();
  // sets here now holds the types that
  // are casted...
  // So, maybe we want to print them?
  //caster.print_reasons();
  //ptrset_t sets = caster.get_sets();
  return 0;
}
int
str_reorg_dead_field_eliminate_trans (Info *info)
{
  return 0;
}
#endif


