/* Interprocedural structure reorganization
   Copyright (C) 2019-2020 Free Software Foundation, Inc.

  Contributed by Gary Oblock <gary@amperecomputing.com>

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

// This is depercated
#define USE_NEW_INTERFACE 1
// This is when our stuff is integrated
#define INTEGRATION_FUNCTIONAL 0
// Erick this is when you do something that really steps on some toes
#define KLUDGE 1
#define USE_REORG_TYPES 1
// If PRINT_FORMAT is true use pass specific print format.
#define PRINT_FORMAT false
// Trun off actual transformations for testing
#define BYPASS_TRANSFORM false
// Use Erick's escape analysis
#define USE_ESCAPE_ANALYSIS 1
// Enables old code sequence (this breaks things)
#define USE_DO_INSTANCE_INTERLEAVE 0
// Testing reversing the sense of modification comparison.
// This is because the decl types are modified before
// compoment reference expressions are modified so to
// detect something needing modification we need to the
// determine the decl was modified.
#define ALLOW_REVERSE 1

typedef struct RT_Elim       RT_Elim;
typedef struct RT_Reorder    RT_Reorder;
typedef struct RT_Interleave RT_Interleave;

#include <set>

struct RT_Elim {
  int dummy;
};

struct RT_Reorder {
  int dummy;
};

struct RT_Interleave {
  int        numbOfGlobalArrays; // Statically allocated only
  int        numbOfLocalArrays;  // Statically allocated only
  int        numbOfDynmAllocs;
  tree       base;
  double     reorg_perf;         
  double     regular_perf;
  bool       multi_pool;         // single pool if not set
  int        dummy;
};

typedef struct ReorgType ReorgType_t;
// TODO: Gary, is there supposed to be 
// 1 ReorgType_t for every tree?
// Don't we need something that also is global?
// I have a set of trees that do not escape
// and I would like to use that for the moment.
// Not sure how it will evolve in the future...
// REPLY: Erick, I'm not sure I follow what you
// are asking. In my mind there should
// be one reorg type for every unique type and
// if some N types are equivalent then there should
// be only one reorg type for all of them.
struct ReorgType {
  unsigned      id;
  bool          delete_me;
  tree          gcc_type;           // This info is for this type.
  // TBD will field reordering and/or dead field elimination use
  // use the reorg_ver_type? I think it's likely that they can.
  tree          reorg_ver_type;     // the base type
  tree          pointer_rep;        // new pointer format (multi-pool)
  bool          do_dead_field_elim;
  bool          do_field_reorder;
  bool          do_instance_interleave;
  RT_Elim       dead_field_elim;
  RT_Reorder    field_reorder;
  RT_Interleave instance_interleave;
};

typedef struct ProgDecl ProgDecl_t;
struct ProgDecl {
  tree       gcc_decl;
};

enum ReorgOpTrans {
  // TODO: What is the purpose of this enum?
  // Let's try to use scalar insead of this... nope!
  ReorgOpT_Temp,         // SSA temp
  ReorgOpT_Address,     // "&x[i]"
  ReorgOpT_Pointer,     // "a"
  ReorgOpT_Struct,      // "s"
  ReorgOpT_Deref,       // "*a"
  ReorgOpT_Array,       // "x[i]"
  ReorgOpT_Scalar,      // "z"
  ReorgOpT_Indirect,    // "a->f"
  ReorgOpT_AryDir,      // "x[i].f"
  ReorgOpT_Cst,
  ReorgOpT_Cst0,
  ReorgOpT_Clobber
};

enum CompressionControl {
  Initial,
  Subsequent
};

enum ReorgTransformation {
  // TODO: Gary, if these are transformation, shouldn't they have
  // a pre-state and a post-state?
  // Here I am only seeing statements. Not sure why 
  // a reorg transformation has statements / expressions.
  //
  // Also, why aren't we using the expressions already
  // defined by GCC? For example EQ_EXPR?
  // Ultimately what is the purpose of these?
  ReorgT_StrAssign,    // "*a = x[i]", "x[i] = y[j]", "s = *a", etc.
  ReorgT_ElemAssign,   // "a->f = z", "z = x[i].f", etc.
  ReorgT_If_Null,      // "if(a == 0)..."
  ReorgT_If_NotNull,   // "if(a != 0)..."
  ReorgT_IfPtrEQ,      // "if(a == b)..."
  ReorgT_IfPtrNE,      // "if(a != b)..."
  ReorgT_IfPtrLT,      // "if(a < b)..."
  ReorgT_IfPtrGT,      // "if(a > b)..."
  ReorgT_IfPtrLE,      // "if(a <= b)..."
  ReorgT_IfPtrGE,      // "if(a >= b)..."
  ReorgT_PtrPlusInt,   // "a = b + i"
  ReorgT_Ptr2Zero,     // "a = 0"
  ReorgT_PtrDiff,      // "i = a - b"
  ReorgT_Adr2Ptr,      // "a = &x[i]"
  ReorgT_PtrNull,      // "x = a == 0"
  ReorgT_PtrNotNull,   // "x = a != 0"
  ReorgT_PtrEQ,        // "x = a == b"
  ReorgT_PtrNE,        // "x = a != b"
  ReorgT_PtrLT,        // "x = a < b"
  ReorgT_PtrLE,        // "x = a <= b"
  ReorgT_PtrGT,        // "x = a > b"
  ReorgT_PtrGE,        // "x = a >= b"
  ReorgT_Malloc,       //
  ReorgT_Calloc,       //
  ReorgT_Realloc,      //
  ReorgT_Free,         //
  ReorgT_UserFunc,     //
  ReorgT_Convert,      // type casts
  ReorgT_Return,       // return t
  ReorgT_Ignore,       // "a = b"
  Not_Supported
};

// Added as design bug fix
typedef struct BoolPair BoolPair_t;
struct BoolPair {
  bool processed;
  bool layout_changed;
};

typedef struct two_trees two_trees_t;
struct two_trees {
  tree first;
  tree second;
};

typedef struct type_holder TypeHolder;

struct type_holder
{
  std::vector <tree> refed_types;
  std::vector <tree> rec_types;
};

typedef struct Info Info_t;
struct Info {
  // TODO: What is the meaning of reorg type?
  // Hasn't this meaning changed now that we have three
  // transformations roughly running at the same time?
  // Eric: Interesting point
  std::vector <ReorgType_t>      *reorg_type;
  // Added to by remove_deleted_types
  std::vector <ReorgType_t>      *saved_reorg_type;
  std::vector <ProgDecl_t>       *prog_decl;
  // Gcc doesn't have global decls readily available
  // so this holds them
  std::map <tree,BoolPair_t>     *struct_types; // desing bug fix
  // For pointer modification
  std::vector <two_trees_t>      *modified_types;
  std::set <tree>                *dont_modify;
  int                             num_deleted;
  double                          total_cache_accesses;
  FILE                           *reorg_dump_file;
  std::map <tree,TypeHolder>     *type_mod_info;
  // Debug flags
  bool                            show_all_reorg_cands;
  bool                            show_all_reorg_cands_in_detail;
  bool                            show_perf_qualify;
  bool                            show_prog_decls;
  bool                            show_delete;
  bool                            show_new_BBs;
  bool                            show_transforms;
  bool                            show_bounds;
  bool                            is_non_escaping_set_empty();

  Info (std::vector <ReorgType_t>     *v1,
        std::vector <ReorgType_t>     *v2,
	std::vector <ProgDecl_t>      *v3,
	std::map    <tree,BoolPair_t> *v4,
	std::vector <two_trees_t>     *v5,
	std::set    <tree>            *v6,
	std::map <tree,TypeHolder>    *v7)
  : reorg_type(v1)
  , saved_reorg_type(v2)
  , prog_decl(v3)
  , struct_types(v4)
  , modified_types(v5)
  , dont_modify(v6)
  , type_mod_info(v7) 
  , num_deleted(0)
  , total_cache_accesses(0)
  , reorg_dump_file(NULL)
  , show_all_reorg_cands(false)
  , show_all_reorg_cands_in_detail(false)
  , show_perf_qualify(false)
  , show_prog_decls(false)
  , show_new_BBs(false)
  , show_transforms(false)
  , show_bounds(false)
  {};
};

// This will perform a function on the supplied
// reorg type. It's primarily to support debugging.
typedef void (*ReorgFn)( Info *, ReorgType_t *);

#define USE_NEW_INTERFACE 1
#if USE_NEW_INTERFACE
extern int str_reorg_dead_field_eliminate_qual ( Info *);
extern int str_reorg_dead_field_eliminate_trans ( Info *);
extern int str_reorg_field_reorder_qual ( Info *);
extern int str_reorg_field_reorder_trans ( Info *);
extern int str_reorg_instance_interleave_qual ( Info *);
extern int str_reorg_instance_interleave_trans ( Info *);
#else
extern int str_reorg_dead_field_eliminate ( Info *);
extern int str_reorg_field_reorder ( Info *);
extern int str_reorg_instance_interleave ( Info *);
#endif
extern void find_and_create_all_modified_types ( Info_t *);
extern std::vector<two_trees_t>::iterator find_in_vec_of_two_types ( std::vector<two_trees_t> *, tree);
extern std::vector<two_trees_t>::iterator find_in_vec_of_two_types_2nd ( std::vector<two_trees_t> *, tree);
extern tree find_modified ( tree,
			    #if ALLOW_REVERSE
			    bool,
			    #endif
			    Info_t *);
extern tree find_deepest_comp_ref_type ( tree);
extern bool new_contains_a_modified ( gimple *, tree *, tree *, Info_t *);
extern tree contains_a_modified ( gimple *,
				  #if ALLOW_REVERSE
				  bool,
				  #endif
				  Info_t *);
extern tree find_deepest_comp_ref ( tree);
#if 0
extern void modify_local_declarations ( Info *);
#endif
extern void modify_global_declarations ( Info *);
extern void modify_parameter_declarations ( Info *);
extern int number_of_levels ( tree);
extern tree make_multilevel( tree, int);
extern bool modify_decl_core ( tree *, Info *);
extern void delete_reorgtype ( ReorgType_t *, Info_t *);
extern void undelete_reorgtype ( ReorgType_t *, Info_t *);
extern void clear_deleted_types( Info *);
extern void restore_deleted_types ( Info *);
extern void remove_deleted_types ( Info *, char *, ReorgFn);
extern enum ReorgOpTrans recognize_op ( tree,  bool, Info_t *);
extern ReorgTransformation reorg_recognize ( gimple *,
					     cgraph_node *,
					     Info_t *);
extern void apply_to_all_gimple ( bool (*)(gimple *, void *), bool, void *);
extern bool same_type_p( tree, tree);
extern ReorgType_t *get_reorgtype_info ( tree, Info_t *);
extern void print_reorg_with_msg ( FILE *, ReorgType_t *, int, const char *);
extern ReorgType_t *contains_a_reorgtype ( gimple *, Info *);
extern bool tree_contains_a_reorgtype_p ( tree, Info *);
extern ReorgType_t *tree_contains_a_reorgtype ( tree, Info *);
extern tree multilevel_component_ref ( tree);
extern bool is_reorg_type ( tree, Info_t *);
extern tree base_type_of ( tree);
extern tree base_type_with_levels ( tree, int *);
extern void print_reorg ( FILE *, int, ReorgType_t *);
extern void print_program ( FILE *, bool, bool, int, Info_t *);
extern void print_type ( FILE *, tree);
extern void modify_ssa_name_type ( tree, tree);
extern bool print_internals (gimple *, void *);
extern const char *optrans_to_str ( enum ReorgOpTrans);
extern char *reorgtrans_to_str ( enum ReorgTransformation);
extern bool is_assign_from_ssa ( gimple *);



// I have no intention of leaving these debugging marcos or uses of
// them in the code. However, some of the uses should obviously be
// converted to dump file information.0
#define DEBUGGING 0
#if DEBUGGING
enum Display {
  Show_nothing,
  Show_failures,
  Show_everything
};

enum Failure {
  Do_not_fail,
  Fail_1st_bad,
  Fail_at_End
};

extern int debug_indenting;
extern void handle_debug_indenting( int);
extern const char *code_str( enum tree_code);
extern void wolf_fence( Info *);
extern bool ssa_check ( FILE *, Display, Failure, bool, bool);

// File and line numbered followed by DEBUG_A on next line
#define DEBUG_FLA(...) { fprintf( stderr, "L# %s:%4d: \n%*s", __FILE__, __LINE__, debug_indenting + 7, ""); fprintf( stderr, __VA_ARGS__); }
// Line numbered followed by DEBUG_A on next line
#define DEBUG_LA(...) { fprintf( stderr, "L# %4d: \n%*s", __LINE__, debug_indenting + 7, ""); fprintf( stderr, __VA_ARGS__); }
// Line numbered
#define DEBUG_L(...) { fprintf( stderr, "L# %4d: %*s", __LINE__, debug_indenting, ""); fprintf( stderr, __VA_ARGS__); }
// Alinged with line numbered
#define DEBUG_A(...) { fprintf( stderr, "%*s", debug_indenting + 7, ""); fprintf( stderr, __VA_ARGS__); }
//With no indenting
#define DEBUG(...) { fprintf( stderr, __VA_ARGS__); }
#define DEBUG_F(f,...) f( __VA_ARGS__)
#define INDENT(a) handle_debug_indenting(a)
#else
#define DEBUG_FLA(...)
#define DEBUG_LA(...)
#define DEBUG_L(...)
#define DEBUG_A(...)
#define DEBUG(...)
#define DEBUG_F(...)
#define INDENT(a)

#endif
extern void flexible_print( FILE *, tree, int, dump_flags_t);

