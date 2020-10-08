#ifndef GCC_IPA_STR_REORG_UTILS_H
#define GCC_IPA_STR_REORG_UTILS_H
#pragma once

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tree.h"

const char * get_type_name (const_tree type);
const char * get_reference_name (const_tree ref);
const char * make_reference_name (const_tree ref);
const char * get_pointer_name (const_tree pointer);
const char * get_array_name (const_tree array);
const char * get_record_name (const_tree record);
const char * make_array_name (const_tree base_type, const unsigned int indirection_level);
const char * make_array_name (const char *base_type_name,  const unsigned int indirection_level);
const char * make_pointer_name (const_tree base_type, const unsigned int indirection_level);
const char * make_pointer_name (const char *base_type_name, const unsigned int indirection_level);
const char * make_pointer_postfix (unsigned int indirection_level);
const char * make_array_postfix (unsigned int indirection_level);
const char * make_array_name (const_tree array);
const char * make_pointer_or_array_name (const char *base_type, const char *postfix);
const char * make_pointer_name (const_tree pointer);
const_tree get_base_type_from_ptr_or_arr_type (const_tree old_pointer_type, const_tree pointer_type, unsigned int &indirection_level);
const_tree get_base_type_from_ptr_or_arr_type (const_tree ptr_or_array, unsigned int &indirection_level);
const_tree get_base_type_from_array_type (const_tree array_type, unsigned int &indirection_level);
const_tree get_base_type_from_array_type (const_tree array_type);
const_tree get_base_type_from_pointer_type (const_tree pointer_type, unsigned int &indirection_level);
const_tree get_base_type_from_pointer_type (const_tree pointer_type);
const_tree get_base_type (const_tree type);
const char* get_field_name (const_tree type);

#include <stdio.h>

#include "types-inlines.h"


#endif 
