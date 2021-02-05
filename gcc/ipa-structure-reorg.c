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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "tree-ssa.h"
#include "gimple.h"
#include "gimple-iterator.h"
#include "gimple-walk.h"
#include "tree-pass.h"
#include "tree-cfg.h"
#include "cfgloop.h"
#include "cgraph.h"
#include "dumpfile.h"
#include "pretty-print.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "langhooks.h"
#include "stor-layout.h"
#include "tree-dfa.h"
#include <vector>
#include <map>
#include <set>
#include "ipa-structure-reorg.h"
#include "gimple-ssa.h"
#include "tree-phinodes.h"
#include "ssa-iterators.h"
#include <stack>
#include "ipa-type-escape-analysis.h"
#include "value-range.h"
#include "stringpool.h"
#include "tree-ssanames.h"

static void setup_debug_flags ( Info *);
static void initial_debug_info ( Info *);
static void final_debug_info ( Info *);
static unsigned int reorg_analysis ( Info *);
static unsigned number_of_executions ( gimple *, struct cgraph_node *);
static void reorg_analysis_debug ( Info *, ReorgType *);
static bool find_decls_and_types ( Info *);
static void find_all_record_types ( tree type, Info_t *);
static std::vector<tree>::iterator find_in_type_vec ( std::vector<tree> *, tree);
static void dump_modified_types ( FILE *, bool, Info_t *);
//static tree find_deepest_comp_ref_type ( tree);
#if USE_REORG_TYPES
static void add_reorg_type( tree, bool, Info *);
#endif
static void disqualify_struct_in_struct_or_union ( Info *);
static void initial_reorg_debug ( Info *, ReorgType *reorg ); 
static void disqualify_struct_in_struct_or_union_debug ( Info *,
							 ReorgType *);
static void disq_str_in_str_or_union_helper ( tree,
					      std::set<tree> *,
					      Info *);
static unsigned int reorg_qualification ( Info *);
static bool transformation_legality ( Info *);
static void transformation_legality_debug ( Info *, ReorgType *);
static bool reorg_legality ( Info *);
static void reorg_common_middle_code ( Info *);
static void modify_declarations ( Info *);
//static void modify_global_declarations ( Info *);
static bool modify_func_decl_core ( struct function *, Info *);
static void disqualify_all_reorgtypes_of ( gimple *, int, Info *);
static void adjust_result_decl ( struct function *);
static tree modify_func_type ( struct function *, Info *);
static tree reverse_args( tree);
static bool needs_modification_p ( struct function *, Info *);
//static int number_of_levels ( tree);
//static void modify_decl_core ( tree *, Info *);
static void reorg_forbidden ( gimple *, Info *);
// Name changed and moved to its own file
//static void reorg_transformation ( Info *);
// made extern
//static void delete_reorgtype ( ReorgType_t *, Info *);
//static void undelete_reorgtype ( ReorgType_t *, Info *);
//static void remove_deleted_types ( Info *, ReorgFn);
//static enum ReorgOpTrans recognize_op ( tree,  Info *);
//static ReorgTransformation reorg_recognize ( gimple *, Info *);
//static bool is_reorg_type ( tree, Info *);
//static tree base_type_of ( tree);
static bool is_user_function ( gimple *, cgraph_node *, Info *);
static bool is_reorg_alloc_trigger ( gimple *);
static ReorgType_t *find_struct_type_ptr_to_struct ( tree, Info *);
static ReorgType_t *the_reorg_walker ( gimple *, Info *);
//static ReorgType_t *get_reorgtype_info ( tree, Info *);
//static void print_reorg_with_msg ( FILE *, ReorgType_t *, int, const char *);
static void dump_reorg ( ReorgType_t *reorg);
static void print_reorgs ( FILE *, int, Info *);
static void print_detailed_reorgs ( FILE *, int, Info *);
//static void print_reorg ( FILE *, int, ReorgType_t *);
static void print_progdecls ( FILE *, int, Info *);
static void print_progdecl ( FILE *, int, ProgDecl_t *);
//static void print_program ( FILE *, bool, int);
static void print_function ( FILE *, bool, int, function *, Info *);
static ReorgType_t *get_reorgtype( gimple *stmt, Info *, int);
static int num_reorgtypes( gimple *, Info *);
static bool uses_field_of_reorgtypes( gimple *, Info *);

//-- debugging only --
#if DEBUGGING
//static const char *code_str( enum tree_code);
static const char *type_name_to_str( tree);
//static void handle_debug_indenting( int);
int debug_indenting = 0;
#endif
//---------------- Code Follows ----------------
  
static unsigned int
ipa_structure_reorg ( void)
{
  std::vector <ReorgType_t>     Reorg_Type;
  std::vector <ReorgType_t>     Saved_Reorg_Type;
  std::vector <ProgDecl_t>      Prog_Decl;
  std::map    <tree,BoolPair_t> StructTypes; // TBD horrible type name
  std::vector <two_trees_t>     Modified_Types;
  std::set    <tree>            Dont_Modify;
  std::map <tree,TypeHolder>    Type_Mod_Info;
  //DEBUG("SAMPLE DEGUG\n");
  //DEBUG_L( "Running ipa_structure_reorg\n");
  //INDENT(2);

  // TBD we must have run the IPA points-to analysis and
  // be running in a single LTRANS partition. Sanity check
  // these here.

  // TODO:
  // Why not make this a class and avoid having all these parameters
  // to initialize?
  // Also, all functions should be references and not pointers...
  Info info ( &Reorg_Type,
	      &Saved_Reorg_Type,
	      &Prog_Decl,
	      &StructTypes,
	      &Modified_Types,
	      &Dont_Modify,
	      &Type_Mod_Info);
  //DEBUG_L("At init dum_deleted %d\n",info.num_deleted);

  cgraph_node* node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node) node->get_untransformed_body ();
  // It really doesn't like get_body.
  //FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node) node->get_body ();
  
  //DEBUG_F( ssa_check, stderr, Show_everything, Do_not_fail, false, true);

  setup_debug_flags ( &info);
  initial_debug_info ( &info);

  //DEBUG_L("");
  //DEBUG_F( wolf_fence, &info);

  if ( !reorg_analysis ( &info) )
  {
    return true;
  }

  //DEBUG_L("after reorg_analysis\n");

  bool qualified = reorg_qualification(&info);
  DEBUG_L("after reorg_qualification\n");
  //DEBUG_L("");
  //DEBUG_F(wolf_fence, &info);

  
  if ( qualified )
  {
    if ( flag_ipa_structure_reorg || flag_ipa_dead_field_eliminate )
      {
        str_reorg_dead_field_eliminate_qual ( &info);
	// Because I just want to do this now...
	#if KLUDGE
	return true;
	#endif
      }
    if ( flag_ipa_structure_reorg || flag_ipa_field_reorder )
      {
	str_reorg_field_reorder_qual ( &info);
      }
    if ( flag_ipa_structure_reorg || flag_ipa_instance_interleave )
      {

	//DEBUG_L("");
	//DEBUG_F( wolf_fence, &info);
	
	//DEBUG_L("before str_reorg_instance_interleave_qual\n");
	str_reorg_instance_interleave_qual ( &info);
	DEBUG_L("after str_reorg_instance_interleave_qual\n");
	//DEBUG_L("");
	//DEBUG_F(wolf_fence, &info);

      }

    fprintf (stderr, "info.show_all_reorg_cands_in_detail %s\n",
	     info.show_all_reorg_cands_in_detail ? "true" : "false");
    if ( info.show_all_reorg_cands_in_detail )
      {
	fprintf ( info.reorg_dump_file, "Qualified the following types:\n"); 
	print_detailed_reorgs ( info.reorg_dump_file, 2, &info);
      }

    reorg_common_middle_code( &info); // ??? might not amount to anything
    DEBUG_L("after reorg_common_middle_code\n");
    //DEBUG_L("");
    //DEBUG_F(wolf_fence, &info);
      
    if ( flag_ipa_structure_reorg || flag_ipa_dead_field_eliminate )
      {
        str_reorg_dead_field_eliminate_trans ( &info);
      }
    if ( flag_ipa_structure_reorg || flag_ipa_field_reorder )
      {
        str_reorg_field_reorder_trans ( &info);
      }
    if ( flag_ipa_structure_reorg || flag_ipa_instance_interleave )
      {
        str_reorg_instance_interleave_trans ( &info);
      }
    DEBUG_L("after optimizations\n");
    //DEBUG_L("");
    //DEBUG_F(wolf_fence, &info);
  }

  final_debug_info ( &info);

  return true;
}

static void
setup_debug_flags ( Info *info)
{
  // The normal way of doing this would be to
  // set the flags with dump_file && (dump_flags & TDF_XXX
  // where XXX is some level of dump control but
  // I think the code here would work better
  // (where each flag is set off the dump_flags.)
  
  if ( dump_file )
  {
    info->show_all_reorg_cands = true;
    info->show_all_reorg_cands_in_detail = dump_flags & TDF_DETAILS;
    info->show_prog_decls = true;
    info->show_perf_qualify = true;
    info->show_delete = dump_flags & TDF_DETAILS;
    info->show_new_BBs = dump_flags & TDF_DETAILS;
    info->show_transforms = dump_flags & TDF_DETAILS;
    info->show_bounds = dump_flags & TDF_DETAILS;
    info->reorg_dump_file = dump_file;
    #if DEBUGGING
    info->reorg_dump_file = stderr;
    #endif
  }
}

static void
initial_debug_info ( Info *info)
{
  if ( info->reorg_dump_file ) 
  {
    print_program ( info->reorg_dump_file, "Initial State", PRINT_FORMAT, false, 0, info);
   }
}

static void
final_debug_info ( Info *info)
{
  if ( info->reorg_dump_file ) 
  {
    print_program ( info->reorg_dump_file, "Final State", PRINT_FORMAT, false, 0, info);
  }
  DEBUG_F( print_program, stderr, "Final Internal State", true, true, 0, info);
}

static unsigned int
reorg_analysis ( Info *info)
{
  struct cgraph_node *node;

  find_decls_and_types ( info);

  //DEBUG_L("\n");
  //DEBUG_F( wolf_fence, info);

  // Skip computing numbOfGlobalArrays initially.

  // Skip computing numbOfLocalArrays initially.

  // Compute numbOfDynmAllocs per type in regtype
  FOR_EACH_FUNCTION ( node) {
    basic_block bb;

    struct function *func = DECL_STRUCT_FUNCTION ( node->decl);

    // This is a work around
    if ( func == NULL )
    {
      //DEBUG_L("  tripped work around\n");
      continue;
    }
    //DEBUG_L("  bypassed work around\n");

    // Note,there is a major design issue with the design of this code.
    // This can stand as is for now but it must be fixed relatively soon.
    FOR_EACH_BB_FN ( bb, func)
    {
      gimple_stmt_iterator gsi;
      for ( gsi = gsi_start_bb ( bb);
	    !gsi_end_p ( gsi);
	    gsi_next ( &gsi) )
      {
	gimple *stmt = gsi_stmt ( gsi);
	//DEBUG_L ( "");
	//DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
	//INDENT(2);
	if ( is_gimple_call ( stmt) )
	{
	  // next line has issues but the mechanism is sound
	  tree t = *gimple_call_lhs_ptr ( stmt);
	  //DEBUG_A( "t %p\n", t);
	  // Calls to a function returning void are skipped.
	  if ( t != NULL )
	  {
	    //DEBUG_A( "t: ");
	    //DEBUG_F( flexible_print, stderr, t, 1, (dump_flags_t)0);
	    tree type = TREE_TYPE( t);
	    //DEBUG_A( "type: ");
	    //DEBUG_F( flexible_print, stderr, type, 1, (dump_flags_t)0);
	    //tree bt = base_type_of ( t);
	    tree bt = base_type_of ( type);
	    if ( TREE_CODE( bt) != RECORD_TYPE &&  TREE_CODE( bt) != VOID_TYPE)
	    {
	      //DEBUG_A( "TREE_CODE( bt) == %s\n", code_str( TREE_CODE ( bt)));
	      //DEBUG_A("");
	      //DEBUG_F(flexible_print, stderr, bt, 1, (dump_flags_t)0);
	      //INDENT(-2);
	      continue;
	    }
	    if ( TREE_CODE( bt) == VOID_TYPE )
	      {
		// Find the use of lhs.  If is an assign
		// get use the base type of it's lhs.
		// Otherwise never mind. Note, there
		// can be multiple uses but find the "one"
		// that's a simple assignment to a typed
		// variable.

		tree ssa_name = gimple_call_lhs( stmt);
		gimple *use_stmt;
		imm_use_iterator iter;
		int num_bt = 0;
		FOR_EACH_IMM_USE_STMT ( use_stmt, iter, ssa_name)
		  {
		    //DEBUG_A("use_stmt: ");
		    //DEBUG_F ( print_gimple_stmt, stderr, use_stmt, 0);
		    if ( is_assign_from_ssa ( use_stmt ) )
		      {
			//DEBUG_A("is assign from ssa\n");
			tree lhs_assign = gimple_assign_lhs( use_stmt);
			tree lhs_type = TREE_TYPE ( lhs_assign);
			tree lhs_base_type = base_type_of ( lhs_type);
			if ( TREE_CODE( lhs_base_type) != VOID_TYPE )
			  {
			    //DEBUG_A("not void\n");
			    num_bt++;
			    bt = lhs_base_type;
			  }
		      }
		  }
		if ( num_bt != 1 )
		  {
		    //DEBUG_L("bailed on base type of complicated case\n");
		    //INDENT(-2);
		    continue;
		  }
	      }
		
	    
	    bool alloc_trigger = is_reorg_alloc_trigger ( stmt);
	      
	    // find if in reorgtypes and get the info (in one call)
	    ReorgType_t *ri = get_reorgtype_info ( bt, info);
	    if ( ri != NULL && alloc_trigger )
	    {
	      //DEBUG_L( "Found allocaion: \n");
	      //DEBUG_A( "  Reorg: ");
	      //DEBUG_F( print_reorg, stderr, 0, ri);
	      //DEBUG("\n");
	      // TBD this needs to increment with the execution count
	      // instead of one. I hope the build-in block count estimation
	      // will work or a DIY solution might be called for.
	      
	      //ri->instance_interleave.numbOfDynmAllocs++;
	      
	      ri->instance_interleave.numbOfDynmAllocs +=
		number_of_executions ( stmt, node);
	      
	    }
	  }
	}
	//INDENT(-2);
      }
    }
  }

  //DEBUG_L( "possible deletes:\n");
  //INDENT(2);
  // It's LOT more clear to use an iterator here TBD
  for ( int i = 0; i < info->reorg_type->size (); i++ )
  {
    int n = (*(info->reorg_type))[i].instance_interleave.numbOfGlobalArrays
      + (*(info->reorg_type))[i].instance_interleave.numbOfLocalArrays
      + (*(info->reorg_type))[i].instance_interleave.numbOfDynmAllocs;
    if ( n > 1 )
    {
      (*(info->reorg_type))[i].instance_interleave.multi_pool = true;
    }
    // Note when multi-pools are enabled the test should be
    // "n == 0" but until then...
    //DEBUG_A("%d pools\n",n)
    if ( n != 1 )
    {
      if ( info->show_all_reorg_cands_in_detail )
	{
	  fprintf ( info->reorg_dump_file, "Delete ReorgType: ");
	  flexible_print ( info->reorg_dump_file,
			   (*(info->reorg_type))[i].gcc_type,
			   0, (dump_flags_t)0);
	  fprintf ( info->reorg_dump_file, ", it has %d pools\n", n);
	}
      delete_reorgtype ( &(*(info->reorg_type))[i], info);
    }
  }
  //INDENT(-2);

  remove_deleted_types ( info, "after reorg_analysis", &reorg_analysis_debug);

  if ( info->show_all_reorg_cands )
  {
    fprintf ( info->reorg_dump_file, "All Reorg Analysis ReorgTypes:\n");
    print_reorgs ( info->reorg_dump_file, 2, info);
  }

  return !info->reorg_type->empty ();
}

// This will be imprecise in that it return 1 or many
// and any number greaater than one is totally imprecise.
static unsigned
number_of_executions ( gimple *stmt,
		       struct cgraph_node *node)
{
  basic_block bb = stmt->bb;
  //DEBUG_A("number_of_executions:> <bb %d> stmt: ", bb->index);
  //DEBUG_F(print_gimple_stmt, stderr, stmt, TDF_DETAILS);

  // Check for in loop
  if ( bb->loop_father != NULL && bb->loop_father->latch->index != 1 )
    {
      //DEBUG_A("In a loop (loop latch <bb %d>, returns 2\n", bb->loop_father->latch->index);
      return 2;
    }
  struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
  
  // Check for recursive
  // TBD Note, recusion detection might be moot since
  // it implies more than on caller (see below where
  // that's detected.)
  //if ( funs->find ( function) != funs->end () ) return 2;
  //funs->insert ( function);
  
  // Check for main (which shouldn't be called)
  struct cgraph_edge *edge = node->get_edge ( stmt);
  if ( node->callers == NULL )
    {
      //DEBUG_A("In main, returns 1\n");
      return 1;
    }
  else
    {
      // Check for multiple callers
      if ( node->callers->next_caller != NULL )
	{
	  //DEBUG_A("Has multiple callers, returns 2\n");
	  return 2;
	}
      // Check single caller for multiple call sites
      //if ( edge->prev_caller != NULL || edge->next_caller != NULL )
      if ( node->callers->prev_caller != NULL ||
	   node->callers->next_caller != NULL    )
	{
	  //DEBUG_A("Single caller has multiple call sites, returns 2\n");
	  return 2;
	}
      // walk up the call chain
      gimple *call_stmt = static_cast <gimple *> (node->callers->call_stmt);
      //DEBUG_A("Walk up the call chain, e->call_stmt: ");
      //DEBUG_F(print_gimple_stmt, stderr, call_stmt, TDF_DETAILS);
      //return number_of_executions ( call_stmt, edge->caller);
      return number_of_executions ( call_stmt, node->callers->caller);
    }
}

static void
reorg_analysis_debug ( Info *info, ReorgType *reorg )
{
  if ( info->show_delete )
  {
    print_reorg_with_msg ( info->reorg_dump_file, reorg, 2,
			   "Was not allocated");
  }
}

#if USE_ESCAPE_ANALYSIS
static bool
find_decls_and_types ( Info *info)
{
  DEBUG_L("find_decls_and_types:> entered\n");

  detected_incompatible_syntax = false;
  std::map<tree, bool> whitelisted = get_whitelisted_nodes();
  tpartitions_t escaping_nonescaping_sets
    = partition_types_into_escaping_nonescaping (whitelisted);

  if ( detected_incompatible_syntax )
    {
      if ( info->show_all_reorg_cands_in_detail )
	{
	  fprintf ( info->reorg_dump_file, "find_decls_and_types: Not C code!\n");
	}
      return false;
    }
  std::set<tree> typeset; // ???
  std::map <tree,TypeHolder> type_mod_info;

  for ( auto typei = escaping_nonescaping_sets.points_to_record.begin ();
	typei != escaping_nonescaping_sets.points_to_record.end (); typei++ )
    {
      tree type = *typei;
      tree canonical = TYPE_MAIN_VARIANT ( base_type_of ( type));

      // This is here as a convenience.
      find_all_record_types( canonical, info);
      
      if ( escaping_nonescaping_sets.non_escaping.find ( type)
	   !=
	   escaping_nonescaping_sets.non_escaping.end ())
	{
	  // Ignore non records
	  //if ( TREE_CODE ( type) != RECORD_TYPE ) continue;
	  if ( TREE_CODE ( canonical) != RECORD_TYPE ) continue;
	  // Check for incomplete types and ignore them.
	  if ( TYPE_SIZE ( canonical) == NULL ) continue;
	  //DEBUG_A( "canonical = ");
	  //DEBUG_F(flexible_print, stderr, canonical, 1, (dump_flags_t)0);
	  if ( TYPE_FIELDS ( canonical) == NULL ) continue;
	  
	  // The types here are highly redundant so ignore
	  // the duplicates.
	  if ( get_reorgtype_info ( canonical, info) ) continue;
	  
	  add_reorg_type ( canonical, false, info);
	  typeset.insert ( canonical);
	}
    }

  // We are have to look at the globals because we created some new types
  // for them
  varpool_node *var;
  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    tree canonical  = TYPE_MAIN_VARIANT ( base_type_of ( decl));

    find_all_record_types( canonical, info);

    if ( escaping_nonescaping_sets.non_escaping.find ( decl)
	   !=
	   escaping_nonescaping_sets.non_escaping.end ())
	{
	  // Check for incomplete types and ignore them.
	  if ( TYPE_SIZE ( canonical) == NULL ) continue;
	  if ( TYPE_FIELDS ( canonical) == NULL ) continue;
	  
	  // The types here are highly redundant so ignore
	  // the duplicates.
	  if ( get_reorgtype_info ( canonical, info) ) continue;
	  
	  add_reorg_type ( canonical, false, info);
	  typeset.insert ( canonical);
	}
  }



  // Don't keep any structure types if they aren't
  // used in an array or have a pointer type (which
  // hopefully will have an associated allocation.)
  // Note, initially ignore the explicit statically
  // allocated arrays.
  //
  // Note, from the original comment this needs to
  // be the state. I'm not sure how Erick's scheme
  // deals with statically allocated arrays. They
  // are forbidden for instance interleaving Mark I
  // (single pool) and need to be detected just to
  // disqualify the assocaied type.

  
  // ???
  
  // We need this later for creating new types
  for ( std::set<tree>::iterator ti = typeset.begin ();
	ti != typeset.end ();
	ti++                                            )
  {
    (*(info->struct_types))[*ti] = { false, false };
  }

  if ( info->show_all_reorg_cands_in_detail )
  {
    fprintf ( info->reorg_dump_file, "All possible candidates:\n");
    print_reorgs ( info->reorg_dump_file, 2, info);
  }
  
  if ( info->show_all_reorg_cands )
  {
    fprintf ( info->reorg_dump_file, "All preliminary ReorgTypes:\n");
    print_reorgs ( info->reorg_dump_file, 2, info);
  }
      
  // Scan all types in ReorgTypes for structure fields and if they are
  // pointers to a type Q in ReorgTypes then clear the deletion mark
  // of Q. Note, at this point in the execution ReorgTypes is all the
  // structure types.
  //
  // It would be a bit nuts to allocate memory and hang it off of
  // pointer in a structure, but it's still possible.  More, likely is
  // just that they are simply pointers into the array of reorg types
  // but the other bit is what counts here.
  //
  // Note, if there are no pointers to a structure of a type then it
  // is impossible to dynamically allocate memory of that type. This
  // of course assumes sane programming practices and if they violate
  // those structure reorg has every right to punt.
  
  //DEBUG_L( "Examine imbedded pointers\n");
  //INDENT(2);
  for ( std::vector<ReorgType_t>::iterator ri = info->reorg_type->begin ();
	ri != info->reorg_type->end ();
	ri++                                                              )
  {
    //DEBUG_A("");
    //DEBUG_F( dump_reorg, &(*ri));
    //DEBUG("\n");
    //INDENT(2);
    for ( tree fld = TYPE_FIELDS ( ri->gcc_type); 
	  fld; 
	  fld = DECL_CHAIN ( fld) )
    {
      tree field_type = TREE_TYPE( fld);
      ReorgType_t *rtype =
	find_struct_type_ptr_to_struct ( field_type, info);
      if ( rtype != NULL )
      {
	undelete_reorgtype ( rtype, info);
      }
    }
    //INDENT(-2);
  }
  //INDENT(-2);
  remove_deleted_types ( info,
			 "after Scan all types in ReorgTypes for structure fields",
			 &initial_reorg_debug);

  // Disqualifying structures in interior to structures is optional
  // (see comment at end of type escape section) but if it's not 
  // done it commits the optimization to do something a little too
  // involved for the initial version.
  disqualify_struct_in_struct_or_union ( info);
  
  if ( info->reorg_type->empty () )
  {
    if ( info->show_all_reorg_cands_in_detail )
    {
      fprintf ( info->reorg_dump_file, "find_decls_and_types: Found no types\n");
    }
    return false;
  }

  // initialize ids of ReorgTypes
  int id = 0;
  for ( std::vector<ReorgType_t>::iterator ri = info->reorg_type->begin ();
	ri != info->reorg_type->end ();
	ri++                                                              )
  {
    ri->id = id;
    id++;
  }

  // Scan all declarations. If their type is in ReorgTypes
  // add them to ProgDecl.
  // Note, there is no mechanism for looking at global declarations
  // so use FOR_EACH_VARIABLE instead. I'm not 100% this is the thing
  // actuall do here... but...
  //DEBUG_L( "ProgDecl global declarations:\n");
  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    tree type = base_type_of ( decl);
    if ( TREE_CODE ( type ) == RECORD_TYPE        &&
	 get_reorgtype_info ( type, info) != NULL    )
    {
      ProgDecl_t decl_info;
      decl_info.gcc_decl = decl;
      info->prog_decl->push_back ( decl_info);
      //DEBUG_A("");
      //DEBUG_F( print_progdecl, stderr, 2, &decl_info);
    }
  }

  
  if ( info->show_all_reorg_cands_in_detail )
  {
    fprintf ( info->reorg_dump_file, "find_decls_and_types: Found the following types:\n"); 
    print_reorgs ( info->reorg_dump_file, 2, info);
  }
  
  if ( info->show_prog_decls )
  {
    fprintf ( info->reorg_dump_file, "ProgDecls:\n");
    print_progdecls ( info->reorg_dump_file, 2, info);
  }

  return true;
}

#else
static bool
find_decls_and_types ( Info *info)
{
  DEBUG_L("find_decls_and_types:> entered\n");
  
  // Don't keep any structure types if they aren't
  // used in an array or have a pointer type (which
  // hopefully will have an associated allocation.)
  // Note, initially ignore the explicit statically
  // allocated arrays.
  //
  // This initializes them too of course.
  
  // Find all struct types for initial ReorgTypes
  // marking them all to initially be deleted.
  // This is done by walking all variables.
  
  std::set<tree> typeset;
  varpool_node *var;
  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    //DEBUG_L( "Consider var->decl\n");
    //DEBUG_L( "");
    //DEBUG_F( print_generic_decl, stderr, decl, (dump_flags_t)-1);
    tree base = base_type_of ( decl);
    //DEBUG( "\n");
    //DEBUG_A("Base\n");
    //DEBUG_A( "TREE_CODE = %s, ", code_str( TREE_CODE ( base)));
    //DEBUG_F( print_generic_expr, stderr, base, (dump_flags_t)-1);
    //DEBUG( "\n");

    if ( TREE_CODE ( base) == RECORD_TYPE )
    {
      // skip if found before
      tree tmv_type = TYPE_MAIN_VARIANT ( base);
      tree tmv_base = tmv_type ? tmv_type : base;
      if ( typeset.find ( tmv_base) != typeset.end () )
      {
	//DEBUG_L( "  not found\n");
	continue;
      } else {
	//DEBUG_L( "  found\n")
	;
      }

      #if USE_REORG_TYPES
      add_reorg_type ( tmv_base, true, info);
      #endif
      typeset.insert ( tmv_base); // ???
    }
  }
  
  // NOTE, the scheme above leaves out local variables so
  // I'll repeat the for the local variable of functions.

  cgraph_node* node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
  {
    tree decl;
    unsigned i;
    
    node->get_untransformed_body ();

    struct function *fn = DECL_STRUCT_FUNCTION ( node->decl);
    // enable this to see error in test_1_8. Note, not a bug... 
    //DEBUG_L( "  function name = %s\n", lang_hooks.decl_printable_name ( node->decl, 2));
    if ( fn == NULL )
    {
      //DEBUG_L( "    EMPTY\n");
      continue;
    }

    //INDENT(2);
    FOR_EACH_LOCAL_DECL ( fn, i, decl)
    {
      tree base = base_type_of ( decl);
      // enable this to see error in test_1_8. Note, not a bug...
      //DEBUG_L( "Consider local var decl\n");
      //DEBUG_F( print_generic_decl, stderr, decl, (dump_flags_t)-1);
      //DEBUG( "\n");

      if ( TREE_CODE ( base) == RECORD_TYPE)
      {
	tree tmv_type = TYPE_MAIN_VARIANT ( base);
	tree tmv_base = tmv_type ? tmv_type : base;
	if ( typeset.find ( tmv_base) != typeset.end () )
        {
	  //INDENT(-2)
	  continue;
	}

	#if USE_REORG_TYPES
	add_reorg_type ( tmv_base, true, info);
	#endif
	typeset.insert ( tmv_base); // ???
      }
    }
    //INDENT(-2);
  }
  
  // We need this later for creating new types
  for ( std::set<tree>::iterator ti = typeset.begin ();
	ti != typeset.end ();
	ti++                                            )
  {
    (*(info->struct_types))[*ti] = { false, false };
  }

  if ( info->show_all_reorg_cands_in_detail )
  {
    fprintf ( info->reorg_dump_file, "All possible candidates:\n");
    print_reorgs ( info->reorg_dump_file, 2, info);
  }
  
  // Scan all declarations for pointers to ReorgTypes
  // and in a later version array of them. When found
  // clear the deletion mark.
  // Note, there is no mechanism for looking at global declarations
  // so use FOR_EACH_VARIABLE instead. I'm not 100% this is the thing
  // actuall do here... but...
  
  //DEBUG_L( "Scan all global decls for pointers to ReorgTypes (possible deletes)\n");
  //INDENT(2);

  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    
    //DEBUG_A( "look at each global var for undelete: ");
    //DEBUG_F( print_generic_decl, stderr, decl, (dump_flags_t)-1);
    //DEBUG( "\n");
    
    tree type = TREE_TYPE ( decl);
    ReorgType_t *rtype = find_struct_type_ptr_to_struct ( type, info);
    if ( rtype != NULL )
    {
      undelete_reorgtype ( rtype, info);
    }
  }
  //INDENT(-2);

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
  {
    tree decl;
    unsigned i;

    // Only need to do this once in the program and it was done
    // above!
    //Node->get_untransformed_body ();
    
    struct function *fn = DECL_STRUCT_FUNCTION ( node->decl);

    //DEBUG_L( "fn %p\n", fn);
    //DEBUG_A("");
    //DEBUG_F( print_generic_decl, stderr, node->decl, (dump_flags_t)-1);
    //DEBUG( "\n");
    // I don't know why this is coming up null.... but I'll
    // skip it for now.
    if ( fn == NULL )
    {
      continue;
    }

    //DEBUG_L( "possible deletes:\n");
    //INDENT(2);
    FOR_EACH_LOCAL_DECL ( fn, i, decl)
    {
      // Does this work... see tree.c:6132
      tree type = TREE_TYPE ( decl);
      ReorgType_t *rtype = find_struct_type_ptr_to_struct ( type, info);
      if ( rtype != NULL )
      {
	undelete_reorgtype ( rtype, info);
      }
    }
    //INDENT(-2);
  }

  if ( info->show_all_reorg_cands )
  {
    fprintf ( info->reorg_dump_file, "All preliminary ReorgTypes:\n");
    print_reorgs ( info->reorg_dump_file, 2, info);
  }
      
  // Scan all types in ReorgTypes for structure fields
  // and if they are pointers to a type Q in ReorgTypes
  // then clear the deletion mark of Q. Note, at this
  // point in the execution ReorgTypes is all the structure
  // types.
  //
  // It would be a bit nuts to allocate memory and hang it
  // off of pointer in a structure, but it's still possible.
  // Note, if there are no pointers to a structure of a type
  // then it is impossible to dynamically allocate memory of
  // that type. This of course assumes sane programming
  // practices and if they violate those structure reorg has
  // every right to punt.
  //DEBUG_L( "Examine imbedded pointers\n");
  //INDENT(2);
  for ( std::vector<ReorgType_t>::iterator ri = info->reorg_type->begin ();
	ri != info->reorg_type->end ();
	ri++                                                              )
  {
    //DEBUG_A("");
    //DEBUG_F( dump_reorg, &(*ri));
    //DEBUG("\n");
    //INDENT(2);
    for ( tree fld = TYPE_FIELDS ( ri->gcc_type); 
	  fld; 
	  fld = DECL_CHAIN ( fld) )
    {
      tree field_type = TREE_TYPE( fld);
      ReorgType_t *rtype =
	find_struct_type_ptr_to_struct ( field_type, info);
      if ( rtype != NULL )
      {
	undelete_reorgtype ( rtype, info);
      }
    }
    //INDENT(-2);
  }
  //INDENT(-2);
  remove_deleted_types ( info,
			 "after Scan all types in ReorgTypes for structure fields",
			 &initial_reorg_debug);

  // Disqualifying structures in interior to structures is optional
  // (see comment at end of type escape section) but if it's not 
  // done it commits the optimization to do something a little too
  // involved for the initial version.
  disqualify_struct_in_struct_or_union ( info);
  
  if ( info->reorg_type->empty () )
  {
    if ( info->show_all_reorg_cands_in_detail )
    {
      fprintf ( info->reorg_dump_file, "find_decls_and_types: Found no types\n");
    }
    return false;
  }

  // initialize ids of ReorgTypes
  int id = 0;
  for ( std::vector<ReorgType_t>::iterator ri = info->reorg_type->begin ();
	ri != info->reorg_type->end ();
	ri++                                                              )
  {
    ri->id = id;
    id++;
  }

  // Scan all declarations. If their type is in ReorgTypes
  // add them to ProgDecl.
  // Note, there is no mechanism for looking at global declarations
  // so use FOR_EACH_VARIABLE instead. I'm not 100% this is the thing
  // actuall do here... but...
  //DEBUG_L( "ProgDecl global declarations:\n");
  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    tree type = base_type_of ( decl);
    if ( TREE_CODE ( type ) == RECORD_TYPE        &&
	 get_reorgtype_info ( type, info) != NULL    )
    {
      ProgDecl_t decl_info;
      decl_info.gcc_decl = decl;
      info->prog_decl->push_back ( decl_info);
      //DEBUG_A("");
      //DEBUG_F( print_progdecl, stderr, 2, &decl_info);
    }
  }
  
  if ( info->show_all_reorg_cands_in_detail )
  {
    fprintf ( info->reorg_dump_file, "find_decls_and_types: Found the following types:\n"); 
    print_reorgs ( info->reorg_dump_file, 2, info);
  }
  
  if ( info->show_prog_decls )
  {
    fprintf ( info->reorg_dump_file, "ProgDecls:\n");
    print_progdecls ( info->reorg_dump_file, 2, info);
  }

  return true;
}
#endif

// Note, these replace possibly_modify_pointer_types.
static void
find_all_record_types ( tree type, Info_t *info)
{
  DEBUG_A("find_all_record_types:>\n");
  std::map <tree,TypeHolder> *types = info->type_mod_info;
  // Get cannonical form of record type
  tree base = base_type_of ( type);

  if( TREE_CODE ( base) != RECORD_TYPE ) return;
  
  tree canonocal = TYPE_MAIN_VARIANT ( base);

  // Add it to the set if need be
  if( types->find ( canonocal) == types->end () )
    {
      TypeHolder holder;
      (*types)[ canonocal] = holder;
    }
}

void
find_and_create_all_modified_types ( Info_t *info)
{
  std::map <tree,TypeHolder> *types = info->type_mod_info;
  std::deque <tree> rec_types_work_list;
  DEBUG_L("find_and_create_all_modified_types:>\n");

  //DEBUG_A("At start:\n");
  //DEBUG_F( dump_modified_types, stderr, false, info);
  //INDENT(4);
  
  // Fill out the references to each type from each type
  for ( auto typei = types->begin (); typei != types->end (); typei++ )
    {
      tree type = typei->first;
      TypeHolder *holder = &(typei->second);
      tree canonical_type = TYPE_MAIN_VARIANT ( base_type_of ( type));
      //DEBUG_A( "canonical_type = ");
      //DEBUG_F(flexible_print, stderr, canonical_type, 1, (dump_flags_t)0);
      //INDENT(4);
      
      // For each record type create an entry
      for ( tree field = TYPE_FIELDS ( canonical_type); 
	    field; 
	    field = DECL_CHAIN ( field) )
	{
	  tree field_type = TREE_TYPE ( field);
	  tree canonical_field_type = TYPE_MAIN_VARIANT ( base_type_of (field_type));
	  //DEBUG_A( "canonical_field_type = ");
	  //DEBUG_F(flexible_print, stderr, canonical_field_type, 1, (dump_flags_t)0);
	  
	  rec_types_work_list.push_back ( canonical_type);
	  
	  if ( TREE_CODE ( canonical_field_type) == RECORD_TYPE)
	    {
	      //DEBUG_A("TREE_CODE ( canonical_field_type) == RECORD_TYPE\n");
	      auto referenced = types->find ( canonical_field_type);
	      gcc_assert ( referenced != types->end ());

	      auto foundi =
		find_in_type_vec ( &referenced->second.refed_types, canonical_type);
	      if ( foundi == referenced->second.refed_types.end () )
		{
		  //DEBUG_A("referenced->second.refed_types.push_back ( canonical_type)\n");
		  referenced->second.refed_types.push_back ( canonical_type);
		}
	    }
	  if ( TREE_CODE ( field_type) == RECORD_TYPE )
	    {
	      //DEBUG_A("TREE_CODE ( field_type) == RECORD_TYPE\n");
	      auto foundi = find_in_type_vec ( &holder->rec_types, canonical_type);
	      if ( foundi == holder->rec_types.end () )
		{
		  //DEBUG_A("holder->rec_types.push_back ( field_type)\n");
		  holder->rec_types.push_back ( field_type);
		}
	    }
	}
      //INDENT(-4);
      // Having an interior record means it can't be processed initally.
      // That is because this is done bottom up starting with types
      // that have no modified interior records.
      if ( holder->rec_types.empty () )
	{
	  //DEBUG_A( "rec_types_work_list <- ");
	  //DEBUG_F(flexible_print, stderr, canonical_type, 1, (dump_flags_t)0);
	  rec_types_work_list.push_back ( canonical_type);
	}
    }
  //INDENT(-4);

  //DEBUG_A("Before modification setup:\n");
  //DEBUG_F( dump_modified_types, stderr, false, info);

  //
  // Propagate The Modifications
  //
  std::vector <tree> needs_modification;
  std::deque <tree> work_list;
  
  // setup up initial modifications
  for ( auto typei = types->begin (); typei != types->end (); typei++ )
    {
      tree type = typei->first;
      for ( tree field = TYPE_FIELDS ( type); 
	    field; 
	    field = DECL_CHAIN ( field) )
	{
	  tree field_type = TREE_TYPE ( field);
	  tree canonical_field_type = TYPE_MAIN_VARIANT ( base_type_of (field_type));
	  if (    TREE_CODE ( canonical_field_type) == RECORD_TYPE
		  && is_reorg_type ( canonical_field_type, info) )
	    {
	      // Need to replace the type if found and the one found is incomplete.
	      // But then we probably don't also need to mess with the work list.
	      auto foundtype = find_in_type_vec ( &needs_modification, canonical_field_type);
	      if( foundtype == needs_modification.end () )
		{
		  //DEBUG_L("needs_modification, insert canonical_field_type %p = ", canonical_field_type);
		  //DEBUG_F(flexible_print, stderr, canonical_field_type, 1, (dump_flags_t)0);
		  //DEBUG_A("  TYPE_FIELDS = %p\n", TYPE_FIELDS(canonical_field_type));
		  needs_modification.push_back ( canonical_field_type);
		  work_list.push_back ( canonical_field_type);
		}
	      else
		{
		  // If the found type is incomplete repalce it.
		  if ( TYPE_FIELDS ( *foundtype) == NULL )
		    {
		      *foundtype = canonical_field_type;
		    }
		}
	    }
	}
    }

  //DEBUG_A("Before propogation:\n");
  //DEBUG_F( dump_modified_types, stderr, false, info);

  // propigate
  while ( !work_list.empty () )
    {
      tree item = work_list.front ();
      work_list.pop_front ();
      
      TypeHolder *holder = &(types->find (item)->second);
      for ( auto ref_typei = holder->refed_types.begin ();
	    ref_typei != holder->refed_types.end ();
	    ref_typei++ )
	{
	  tree ref_type = *ref_typei;
	  tree canonical_ref_type = TYPE_MAIN_VARIANT ( base_type_of (ref_type));
	  //DEBUG_A("ref_type %p = ", canonical_ref_type);
	  //DEBUG_F(flexible_print, stderr, canonical_ref_type, 1, (dump_flags_t)0);
	  //DEBUG_A("  TYPE_FIELDS = %p\n", TYPE_FIELDS(canonical_ref_type));
	  auto foundtype = find_in_type_vec ( &needs_modification, canonical_ref_type);
	  if ( foundtype == needs_modification.end () )
	    {
	      //DEBUG_A("  Inserted\n");
	      needs_modification.push_back ( canonical_ref_type);
	      work_list.push_back ( canonical_ref_type);
	    }
	  else
	    {
	      // If the found type is incomplete repalce it.
	      if ( TYPE_FIELDS ( *foundtype) == NULL )
		{
		  *foundtype = canonical_ref_type;
		}
	    }
	}
    }

  // Create The Modifications

  //DEBUG_A("Before doing the modifications:\n");
  //DEBUG_F( dump_modified_types, stderr, false, info);

  // Just create the type, the fields are created afterwards
  for ( auto type2modi = needs_modification.begin ();
	type2modi != needs_modification.end (); type2modi++ )
    {
      tree type_to_modify = *type2modi;
      //DEBUG_A("type_to_modify (from needs_modification) = ");
      //DEBUG_F(flexible_print, stderr, type_to_modify, 1, (dump_flags_t)0);
      // Create new record type
      tree modified_type = lang_hooks.types.make_type (RECORD_TYPE);
      //(*(info->modified_types))[ type_to_modify] = modified_type;
      two_trees_t entry = { type_to_modify, modified_type};
      info->modified_types->push_back ( entry);
      
      const char *old_type_name =
	identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( type_to_modify)));
      size_t len = strlen ( "_modif_") + strlen ( old_type_name);
      char *rec_name = ( char*)alloca ( len + 1);
      strcpy ( rec_name, "_modif_");
      strcat ( rec_name, old_type_name);
    
      // Build the new pointer type fields
      TYPE_NAME ( modified_type) = get_identifier ( rec_name);

      // Make sure this is does (this might be unnecessary)
      TYPE_FIELDS (modified_type) = NULL;
    }
  //DEBUG_A("Before field creation:\n");
  //DEBUG_F( dump_modified_types, stderr, false, info);

  // Create the fields but the types must be created in a bottom up
  // manner where the types include types as elements that are below
  // them.
  while ( !rec_types_work_list.empty () )
    {
      tree type = rec_types_work_list.front ();
      rec_types_work_list.pop_front ();
      DEBUG_A("type (rec_types_work_list.front) = ");
      DEBUG_F(flexible_print, stderr, type, 1, (dump_flags_t)0);

      // Already processed, skip.
      if ( TYPE_FIELDS ( type) != NULL ) continue;
      
      //auto pairi = info->modified_types->find (type);
      auto peari = find_in_vec_of_two_types ( info->modified_types, type);
      //gcc_assert( peari != info->modified_types->end ());
      // The is a possibility the type on the work list does need to
      // be modified. If found skip this type.
      // This situtaion can likely be avoided but it might not be worth
      // the effort.
      if ( peari == info->modified_types->end ()) continue;
      
      // The sane type should be complete! (I hope)
      tree sane_type = peari->first;
      gcc_assert( TYPE_FIELDS ( sane_type) != NULL );
      //DEBUG_A("sane_type (TYPE_FIELDS %p) = ", TYPE_FIELDS ( sane_type));
      //DEBUG_F(flexible_print, stderr, sane_type, 1, (dump_flags_t)0);
      tree modified_type = peari->second;

      tree field;
      tree new_fields = NULL;
      for ( field = TYPE_FIELDS ( sane_type); field; field = DECL_CHAIN ( field))
	{
	  tree field_type = TREE_TYPE ( field);
	  tree new_fld_type;
	  //DEBUG_A("transforming field = ");
	  //DEBUG_F(flexible_print, stderr, field, 1, (dump_flags_t)0);
	  tree canoncl_fld_type = TYPE_MAIN_VARIANT ( base_type_of (field_type));

	  // TBD Do I need a canonical type here instead?
	  //auto is_modified = info->modified_types->find ( type);
	  //auto is_modified = find_in_vec_of_two_types ( info->modified_types, sane_type); // ???
	  auto is_modified = find_in_vec_of_two_types ( info->modified_types, canoncl_fld_type);
	  //DEBUG_A("is_modified = %s\n", is_modified != info->modified_types->end () ? "T" : "F");
	  if ( is_modified != info->modified_types->end () )
	    {
	      if ( POINTER_TYPE_P ( field_type))
		{
		  ReorgType_t *ri = get_reorgtype_info ( canoncl_fld_type, info);
		  int levels = number_of_levels ( field_type);
		  //DEBUG_A("ri = %p, levels = %d\n", ri, levels);
		  if ( ri == NULL )
		    {
		      new_fld_type = make_multilevel ( is_modified->second, levels);
		    }
		  else
		    {
		      new_fld_type = make_multilevel ( ri->pointer_rep, levels - 1);
		    }
		}
	      else
		{
		  new_fld_type = is_modified->second;
		}
	    }
	  else
	    {
	      // This field doesn't need to be modified.
	      new_fld_type = field_type;
	    }
	  tree new_decl =
	    build_decl ( DECL_SOURCE_LOCATION (field),
			 FIELD_DECL, DECL_NAME (field), new_fld_type);
	  DECL_CONTEXT ( new_decl) = modified_type;
	  
	  //DEBUG_A("new_fld_type = ");
	  //DEBUG_F(flexible_print, stderr, new_fld_type, 1, (dump_flags_t)0);
	  //DEBUG_A("new_decl = ");
	  //DEBUG_F(flexible_print, stderr, new_decl, 1, (dump_flags_t)0);

	  DECL_CHAIN ( new_decl) = new_fields; // <- bug: need decl, not type
	  new_fields = new_decl;
	}

      // Reverse fields. Note, a some point try nreverse here instead.
      TYPE_FIELDS ( modified_type) = NULL;
      tree next_fld;
      for ( field = new_fields;
	    field; 
	    field = next_fld    )
	{
	  next_fld = DECL_CHAIN ( field);
	  DECL_CHAIN ( field) = TYPE_FIELDS ( modified_type);
	  TYPE_FIELDS ( modified_type) = field;
	}

      //DEBUG_A("Before lay_type:\n");
      //DEBUG_F( dump_modified_types, stderr, false, info);

      // Lay it out

      // From ubsan.c:ubsan_get_type_descriptor_type
      // This doesn't break anything but it doesn't help either!
      
      // create a decl for the type
      tree type_decl = build_decl ( UNKNOWN_LOCATION, TYPE_DECL,
				    //get_identifier ("__ubsan_type_descriptor")???,
				    TYPE_NAME ( modified_type),
				    modified_type);
      
      TYPE_STUB_DECL ( modified_type) = type_decl;
      
      layout_type ( modified_type);

      //DEBUG_LA("modified_type (right after layout) = \n");
      //DEBUG_F( dump_record, stderr, modified_type, true);

      TypeHolder *holder = &(types->find ( sane_type)->second);
      // Add new types to the work list if possible
      // Note, this code is adding types to the work list too
      // agressively and multiple entries are the result.
      for ( auto refingi = holder->refed_types.begin ();
	    refingi != holder->refed_types.end ();
	    refingi++  )
	{
	  tree refing_type = *refingi;
	  TypeHolder *refing_holder = &(types->find ( sane_type)->second);
	  for ( auto record_typei = refing_holder->rec_types.begin ();
		record_typei != refing_holder->rec_types.end ();
		record_typei++ )
	    {
	      tree record_type = *record_typei;
	      if ( TYPE_FIELDS (record_type) == NULL ) goto dont_add;
	    }
	  // Only add to the work list if all the necessary
	  // structure ypes have already been processed.
	  // Note, this is only added once when the last
	  // structure type necessary has its fields created
	  // above.
	  rec_types_work_list.push_back (refing_type);
	  
	dont_add:
	  ;
	}
    }
  DEBUG_A("End results:\n");
  DEBUG_F( dump_modified_types, stderr, true, info);
}

static std::vector<tree>::iterator
find_in_type_vec ( std::vector<tree> *types, tree type)
{
  //INDENT(2);
  for ( auto looki = types->begin (); looki != types->end (); looki++)
    {
      tree look = *looki;
      if ( same_type_p ( look, type) )
	{
	  //INDENT(-2);
	  return looki;
	}
    }
  //INDENT(-2);
  return types->end ();
}

bool use_dump_record = false;

void
dump_record (FILE *file, tree record_type, bool extra_details) 
{
  fprintf ( stderr, "dump_record:>\n");
  tree field;
  fprintf ( file, "  %p, ", record_type);
  flexible_print ( file, record_type, 1, (dump_flags_t)0);
  for ( field = TYPE_FIELDS ( record_type);
	field; field = DECL_CHAIN ( field))
    {
      fprintf ( file, "  ");
      flexible_print ( file, field, 1, (dump_flags_t)0);
      if ( extra_details )
	{
	  tree off = DECL_FIELD_OFFSET ( field);
	  if ( off )
	    {
	      fprintf ( file, "    ");
	      flexible_print ( file, off, 0, (dump_flags_t)0);
	    }
	  else
	    {
	      fprintf ( file, "    (nil)");
	    }
	  fprintf ( file, ", %p", &DECL_FIELD_OFFSET ( field));
	  fprintf ( file, "\n");
	}
    }
}

static void
dump_modified_types (FILE *file, bool extra_details, Info_t *info)
{
  //DEBUG_A("");
  fprintf ( file, "dump_modified_types:> %s\n",
	    info->modified_types->begin () == info->modified_types->end ()
	    ? " (empty)" : "");
  for ( auto modifi = info->modified_types->begin ();
	modifi != info->modified_types->end (); modifi++ )
    {
      tree field;
      tree orig_type = modifi->first;
      tree new_type = modifi->second;
      //DEBUG_A("");

      dump_record (file, orig_type, extra_details);

      //DEBUG_A("");

      dump_record (file, new_type, extra_details);
    }
}

tree
find_modified ( tree type,
		bool reverse,
		Info_t *info )
{
  DEBUG_A("find_modified:> ");
  DEBUG_F(flexible_print, stderr, type, 1, (dump_flags_t)0);
  // Use canonical type
  tree canonical = TYPE_MAIN_VARIANT ( base_type_of ( type));
  tree ret_val = NULL;
  if ( reverse )
    {
      auto resulti = find_in_vec_of_two_types_2nd ( info->modified_types, canonical);
      if ( resulti != info->modified_types->end () )
	{
	  ret_val = resulti->first; // ???
	}
    }
  else
    {
      auto resulti = find_in_vec_of_two_types ( info->modified_types, canonical);
      if ( resulti != info->modified_types->end () )
	{
	  ret_val = resulti->second;
	}
    }
  
  DEBUG_A("  returns... ");
  DEBUG_F(flexible_print, stderr, ret_val, 1, (dump_flags_t)0);
  
  return ret_val;
}

bool
new_contains_a_modified ( gimple *stmt, tree *lhs_mod, tree *rhs_mod, Info_t *info )
{
  bool modified = false;
  *lhs_mod = NULL;
  *rhs_mod = NULL;
  DEBUG_FLA("new_contains_a_modified:> ");
  DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
  INDENT(2);
  // For an assign check both sides component refs.
  if ( gimple_code ( stmt) == GIMPLE_ASSIGN )
    {
      tree lhs = gimple_assign_lhs( stmt);
      tree rhs = NULL;
      gassign *gass = static_cast <gassign *> (stmt);
      // A simple 
      if ( get_gimple_rhs_class ((enum tree_code)gass->subcode)
	   ==
	   GIMPLE_SINGLE_RHS)
	{
	  rhs = gimple_assign_rhs1 ( stmt);
	}
      // Use find_deepest_comp_ref on both sides. It's safe in
      // meaningless circumstances.
      tree l_deepest_type = find_deepest_comp_ref_type ( lhs);
      tree r_deepest_type;
      //If a side has a deepest component ref check to see if
      // modified.
      if ( l_deepest_type )
	{
	  if ( find_modified ( l_deepest_type,
			       false,
			       info) )
	    {
	      *lhs_mod = l_deepest_type;
	      modified = true;
	    }
	}
      if (    rhs
	   && (r_deepest_type = find_deepest_comp_ref_type ( rhs))
	   && find_modified ( r_deepest_type,
			      false,
			      info) )
	{
	  *rhs_mod = r_deepest_type;
	  modified = true;
	}
    }
  INDENT(-2);
  return modified;
}

tree
contains_a_modified ( gimple *stmt,
		      bool reverse, // reverse sense of modification
		      Info_t *info )
{
  DEBUG_A("contains_a_modified:> ");
  DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
  INDENT(2);
  // For an assign check both sides component refs.
  if ( gimple_code ( stmt) == GIMPLE_ASSIGN )
    {
      DEBUG_A("Is GIMPLE_ASSIGN\n");
      tree lhs = gimple_assign_lhs( stmt);
      tree rhs = NULL;
      gassign *gass = static_cast <gassign *> (stmt);
      // A simple 
      if ( get_gimple_rhs_class ((enum tree_code)gass->subcode)
	   ==
	   GIMPLE_SINGLE_RHS)
	{
	  rhs = gimple_assign_rhs1 ( stmt);
	}
      // Use find_deepest_comp_ref on both sides. It's safe in
      // meaningless circumstances.
      tree l_deepest_type = find_deepest_comp_ref_type ( lhs);
      tree r_deepest_type;
      //If a side has a deepest component ref check to see if
      // modified.
      if ( l_deepest_type )
	{
	  if ( find_modified ( l_deepest_type,
			       reverse,
			       info) )
	    {
	      INDENT(-2);
	      return l_deepest_type;
	    }
	  // Just having a componet ref means we don't
	  // have to look at the other side
	  INDENT(-2);
	  return NULL;
	}
      if (    rhs
	   && (r_deepest_type = find_deepest_comp_ref_type ( rhs))
	   && find_modified ( r_deepest_type,
		              reverse,
		              info) )
	{
	  INDENT(-2);
	  return r_deepest_type;
	}
    }
  // Otherwise,
  INDENT(-2);
  return NULL;
}

tree
find_deepest_comp_ref_type ( tree op )
{
  DEBUG_A("find_deepest_comp_ref_type:> ");
  DEBUG_F(flexible_print, stderr, op, 1, (dump_flags_t)0);
  INDENT(2);
  if ( op == NULL )
    {
      INDENT(-2);
      return NULL;
    }
  tree comp_ref = find_deepest_comp_ref ( op);
  if ( comp_ref == NULL )
    {
      INDENT(-2);
      return NULL;
    }
  tree type = TREE_TYPE ( TREE_OPERAND( comp_ref, 0));
  tree canonical_type = TYPE_MAIN_VARIANT ( base_type_of ( type));
  INDENT(-2);
  return canonical_type;
}

#if USE_REORG_TYPES
static void
add_reorg_type (
		tree base,
		bool del, Info *info)
{
  tree tmv_type = TYPE_MAIN_VARIANT ( base);
  //tree type2add = tmv_type ? tmv_type : base;
  //ReorgType_t rt =
  //  { 0, del, base, NULL, NULL, false, false, false,
  //    { 0}, { 0}, { 0, 0, 0, NULL, 0.0, 0.0, false}};
  ReorgType_t rt =
    { 0, del, tmv_type, NULL, NULL, false, false, false,
      { 0}, { 0}, { 0, 0, 0, NULL, 0.0, 0.0, false}};

  //DEBUG_L("add_reorg_type:> ");
  //DEBUG_F(flexible_print, stderr, base, 1, (dump_flags_t)0);
  info->reorg_type->push_back ( rt);
  #if !USE_ESCAPE_ANALYSIS
  // Remember the intial assumption is the type added will be deleted
  // and is marked to be deleted.
  info->num_deleted++;
  #endif
}
#endif

void
disqualify_struct_in_struct_or_union ( Info *info)
{
  varpool_node *var;
  std::set<tree> typeset;
  
  DEBUG_L( "disqualify_struct_in_struct_or_union:>\n");
  //INDENT(2);
  
  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    
    //DEBUG( "type %s\n", TYPE_NAME( TREE_TYPE( decl)));
    
    tree base = base_type_of ( decl);
    if (    TREE_CODE ( base) == RECORD_TYPE
 	 || TREE_CODE ( base) == UNION_TYPE  )
    {
      disq_str_in_str_or_union_helper ( base, &typeset, info);
      typeset.insert ( base);
    }
  }
  //INDENT(-2);

  // Repeating the above for local variables
  cgraph_node* node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
  {
    tree decl;
    unsigned i;
    
    node->get_untransformed_body ();

    struct function *fn = DECL_STRUCT_FUNCTION ( node->decl);
    //DEBUG_L( "function name = %s\n",
    //	   lang_hooks.decl_printable_name ( node->decl, 2));
    if ( fn == NULL )
    {
      continue;
    }

    //INDENT(2);
    FOR_EACH_LOCAL_DECL ( fn, i, decl)
    {
      tree base = base_type_of ( decl);
      
      //DEBUG_L( "local var decl: ");
      //DEBUG_F( print_generic_decl, stderr, decl, (dump_flags_t)-1);
      //DEBUG( "TREE_CODE( base) = %s\n", code_str( TREE_CODE ( base)));
      
      if (    TREE_CODE ( base) == RECORD_TYPE
	      || TREE_CODE ( base) == UNION_TYPE  )
      {
	disq_str_in_str_or_union_helper ( base, &typeset, info);
	typeset.insert ( base);
      }
    }
    //INDENT(-2);
  }
  remove_deleted_types ( info,
			 "after disqualify_struct_in_struct_or_union",
			 &disqualify_struct_in_struct_or_union_debug);
}

static void
initial_reorg_debug ( Info *info, ReorgType *reorg )
{
  if ( info->show_delete )
  {
    print_reorg_with_msg ( info->reorg_dump_file, reorg, 2,
			   "No Pointer to Structure");
  }
}
 
static void
disqualify_struct_in_struct_or_union_debug ( Info *info,
					     ReorgType *reorg )
{
  if ( info->show_delete )
  {
    print_reorg_with_msg ( info->reorg_dump_file, reorg, 2,
			   "Interior Struct or Union");
  }
}


static void
disq_str_in_str_or_union_helper ( tree type,
				  std::set<tree> *typeset,
				  Info *info )
{
  DEBUG_L( "disq_str_in_str_or_union_helper:> (possibele deletes)\n");
  //INDENT(2);
  
  if ( typeset->find ( type) != typeset->end ()) return;
  tree fld;
  for ( tree fld = TYPE_FIELDS( type); fld; fld = DECL_CHAIN ( fld) )
  {
    //DEBUG_A( ": ", DECL_NAME( fld));
    //DEBUG_F( print_generic_decl, stderr, fld, (dump_flags_t)-1);
    //DEBUG_F( print_generic_decl, stderr, fld, (dump_flags_t)-1);
    //DEBUG( " -- ");
    //INDENT(2);

    // If we go to the base we end up disqualifying pointer to reorganizable
    // structure. That's not what we want!
    tree field_type = TREE_TYPE( fld);
    //tree base = base_type_of ( field_type);
    if ( TREE_CODE ( field_type) == RECORD_TYPE )  // base to field type
    {
      //DEBUG( "RECORD\n");
      
      ReorgType_t *rinfo = get_reorgtype_info ( field_type, info);  // base to field type
      if ( rinfo != NULL )
      {
	if ( info->show_all_reorg_cands_in_detail )
	  {
	    fprintf ( info->reorg_dump_file, "Delete ReorgType: ");
	    flexible_print ( info->reorg_dump_file,
			     rinfo->gcc_type,
			     0, (dump_flags_t)0);
	    fprintf ( info->reorg_dump_file, ", Structure in structure.\n");
	  }
	delete_reorgtype ( rinfo, info);
      } else {
	disq_str_in_str_or_union_helper ( field_type, typeset, info ); // base to field type
	typeset->insert ( field_type); // might be bug here -- base to field type
      }
    } else {
      if ( TREE_CODE ( field_type) == UNION_TYPE ) { // base to field type
	//DEBUG( "UNION\n");
	
	disq_str_in_str_or_union_helper ( field_type, typeset, info ); // base to field type
	typeset->insert ( field_type); // might be bug here -- base to field type
      } else {
	//DEBUG( "%s\n", code_str( TREE_CODE ( field_type)) ); // base to field type
      }
    }
    //INDENT(-2);
  }
  //INDENT(-2);
  
  return;
}
  
static unsigned int
reorg_qualification ( Info *info)
{
  // TBD
  // This only does a generic legality qualification and each
  // subpass does its own performance qualification.
  unsigned int retval = reorg_legality( info);
  return retval;
  
}

// Return false if nothing qualified
bool
reorg_legality( Info *info)  {
  bool retval = transformation_legality( info);
  return retval;
}

bool
Info::is_non_escaping_set_empty()
{
  return true;
}

// Return false if nothing qualified
// TODO:
// What exactly is the difference between legality and
// non_escaping?
bool 
transformation_legality ( Info *info)
{
  DEBUG_A("transformation_legality:>\n");
  //TODO: Gary, for my purposes, I need to start running the
  // code related to dead field eliminate. So, I'll add this bit
  //
  // TODO:
  // * Legality must include type-casting
  // * Legality must include address ??
  // * What about memmove?
  //
  // struct astruct_s { _Bool a; _Bool b; _Bool c; };
  // struct astruct_s astruct_1;
  // struct astruct_s astruct_2;
  // GIMPLE_IL sizeof tree expr
  // memmove(&astruct_1, &astruct_2, sizeof(struct astruct_s));
  // memmove(&astruct_1.b, &astruct_2.b, 2*sizeof(_Bool));
  const bool run_escape_analysis =
    flag_ipa_dead_field_eliminate &&
    !flag_ipa_instance_interleave &&
    !flag_ipa_field_reorder;
  
  if (run_escape_analysis)
  {
    bool retval = !info->is_non_escaping_set_empty();
    return retval;
  }

  cgraph_node* node;

  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
  {
    struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
    basic_block bb;

    FOR_EACH_BB_FN ( bb, func)
    {
      gimple_stmt_iterator gsi;
      for ( gsi = gsi_start_bb ( bb);
	    !gsi_end_p ( gsi);
	    gsi_next ( &gsi) )
      {
	gimple *stmt = gsi_stmt ( gsi);
	int num = num_reorgtypes ( stmt, info);
	if ( num != 0 )
	{
	  ReorgTransformation trans = reorg_recognize( stmt, node, info);
	  switch ( trans )
	    {
	    case Not_Supported:
	      //DEBUG_L("deleting %d reorgs for unsuported stmt: ", num);
	      //DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
	      // TBD This is not working
	      //disqualify_all_reorgtypes_of ( stmt, num, info);
	    case ReorgT_UserFunc:
	      // TBD ReorgT_Ptr2Zero does not catch all cases of
	      // setting a reorg pointer to zero. One that I
	      // discoivered in a dump is phis can hold constanst.
	      break;
	    case ReorgT_Return:
	      // TBD ReorgT_Ptr2Zero does not catch all cases of
	      // setting a reorg pointer to zero. One that I
	      // discoivered in a dump is phis can hold constanst.
	      break;
	    case ReorgT_Convert:
	      // TBD Note, any conversion of an integer type to a
	      // reorg pointer type can obsure the zero transformation
	      // and needs to disqualify the type.
	      break;
	    default:
	      // No problem.
	      ;
	    }
	}
	if ( uses_field_of_reorgtypes( stmt, info) )
	{
	  // This will mark types to be deleted if need be.
	  reorg_forbidden ( stmt, info );
	}
      }
    }

    // TBD Walk the PHIs looking for reorg type PHIs with a
    // nonzero constant. Disqualify any typeas this happens with.
  }

  remove_deleted_types ( info,
			 "after transformation_legality",
			 &transformation_legality_debug);
    
  return !info->reorg_type->empty ();
}

void
transformation_legality_debug ( Info *info, ReorgType *reorg )
{
  if ( info->show_delete )
  {
    print_reorg_with_msg ( info->reorg_dump_file, reorg, 2,
			   "Unallowed transformation");
  }
}

static void
reorg_common_middle_code ( Info *info)
{
  if ( BYPASS_TRANSFORM ) return;

  // Useed to modify_declarations here
}

static void
modify_declarations ( Info *info)
{
  modify_global_declarations (info);
}

void
modify_global_declarations ( Info *info)
{
  DEBUG_A("modify_global_declarations:>\n");
  INDENT(2);
  // For the moment we ignore initializations assuming
  // all potential reorg types that had initialized
  // arrays were disqualified. Note, it's the way
  // to go until statically allocated arrays are optimized
  // Once we attempt statically allocated arrays problems
  // crop up because some initializations aren't preserved
  // until LTRANS time and even those that are don't necessarily
  // lend themselves to any necessary reorg transformation.
  // Note, it's possible to preserve them, if that makes sense,
  // in remove_unreferenced_decls.

  // The prog_decls are obsolete
  varpool_node *var;
  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    tree type = base_type_of ( decl);
    
    bool do_reorg = 
      TREE_CODE ( type ) == RECORD_TYPE && get_reorgtype_info ( type, info) != NULL;
    bool do_modify = find_modified ( type, false, info) != NULL;
      if ( do_reorg || do_modify )
    {
      modify_decl_core ( &decl, info);
    }
  }

  // NOTE, Call modufy_decl_core breaks hello world!
  
  // Modify the declaration of the function type itself.
  // Note, create a new declaration if necessary. If this
  // loop already created and the function is seen a second
  // time reuse the previous one created.
  
  // Note, most of the function type stuff can use memoization but
  // it's not worth doing unless it proves to be a significant
  // performance issue. This is what fncache is all about. But it's
  // used not to save time but to guarantee that if two functions have
  // the same type before this exercise, their new types will be
  // equal afterwards.
  std::map <tree,tree> fncache;

  struct cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)

    {
      struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
      push_cfun ( func); // Is this necessary?
      tree curr_func_type = TREE_TYPE ( func->decl);
      
      // TBD Check cached function type. If the decl's type
      // has been modified use the cached new type.
      auto location = fncache.find ( curr_func_type);
      tree old_ret_type = TREE_TYPE ( TREE_TYPE ( func->decl));
      if ( location !=  fncache.end () )
	{
	  TREE_TYPE ( func->decl) = location->second;

	  // The function type can be cached but this must be created
	  // anew for any function who's return type has changed
	  if ( old_ret_type != TREE_TYPE ( TREE_TYPE ( func->decl)) )
	    {
	      adjust_result_decl ( func);
	    }
	  
	  pop_cfun ();
	  continue;
	}

      // check if funtion type needs modification.
      if ( needs_modification_p ( func, info) )
	{
	  // Create new type and set the decl's type to it.
	  tree new_func_type = modify_func_type ( func, info);

	  if ( old_ret_type != TREE_TYPE ( new_func_type)) 
	    {
	      adjust_result_decl ( func);
	    }

	  // Add the type to the cache.
	  fncache [ curr_func_type] = new_func_type;
	}

      pop_cfun ();
    }

  // Note, do not relayout functions decls....
  INDENT(-2);
}

static void
disqualify_all_reorgtypes_of ( gimple *stmt, int num, Info *info)
{
  DEBUG_L("disqualify %d reorgtypes of:> ", num);
  //DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
  if ( info->show_all_reorg_cands_in_detail )
    {
      print_gimple_stmt( info->reorg_dump_file, stmt, 0);
    }
  int i;
  for ( i = 0; i < num; i++ )
    {
      ReorgType_t *reorg_type =
	get_reorgtype ( stmt, info, i);
      //DEBUG_A( "reorg_type = %p\n", reorg_type);
      if ( info->show_all_reorg_cands_in_detail )
	{
	  fprintf ( info->reorg_dump_file, "  Delete ReorgType: ");
	  flexible_print ( info->reorg_dump_file,
			   (*(info->reorg_type))[i].gcc_type,
			   0, (dump_flags_t)0);
	  fprintf ( info->reorg_dump_file, ", because of gimple above.\n");
	}
      delete_reorgtype ( reorg_type, info);
    }
}

static void
adjust_result_decl ( struct function *func)
{
  tree func_decl = func->decl;
  tree ret_type = TREE_TYPE ( TREE_TYPE ( func_decl));
  tree decl_result;

  decl_result =
    build_decl ( DECL_SOURCE_LOCATION ( func_decl),
		      RESULT_DECL, NULL_TREE, ret_type);
  DECL_RESULT ( func_decl) = decl_result;
  DECL_CONTEXT ( decl_result) = func_decl;  
}


static tree
modify_func_type ( struct function *func, Info *info )
{
  DEBUG_A("modify_func_type:>\n");
  tree func_type = TREE_TYPE ( func->decl);
  //DEBUG_L("old func_type = ");
  //DEBUG_F( flexible_print, stderr, func_type, 1, (dump_flags_t)0);
  //INDENT(4);
  tree new_type;
  tree func_ret_type = TREE_TYPE ( func_type);
  tree base = base_type_of ( func_ret_type);

  ReorgType_t *ri = get_reorgtype_info ( base, info);
  if ( ri != NULL )
    {
      int levels = number_of_levels ( func_ret_type );

      if ( levels == 1 )
	{
	  func_ret_type = TYPE_MAIN_VARIANT ( ri->pointer_rep);
	}
      else
	{
	  func_ret_type = make_multilevel ( ri->pointer_rep, levels);
	}
    }

  tree interim_args = void_list_node;
  tree new_args = void_list_node;
  tree existing_args = TYPE_ARG_TYPES ( func_type);
  //DEBUG_A("TYPE_ARG_TYPES ( func_type) = ");
  //DEBUG_F( flexible_print, stderr, existing_args, 1, (dump_flags_t)0);
  if ( existing_args != void_list_node )
    {
      tree arg;
      //DEBUG_A("old arg = ");
      //DEBUG_F( flexible_print, stderr, TYPE_ARG_TYPES ( func_type), 1, (dump_flags_t)0);
      for ( arg = TYPE_ARG_TYPES ( func_type);
	    arg != NULL && arg != void_list_node;
	    arg = TREE_CHAIN ( arg))
	{
	  //DEBUG_L("arg: ");
	  //DEBUG_F( flexible_print, stderr, arg, 1, (dump_flags_t)0);
	  
	  tree type_of_arg = TREE_VALUE (arg);
	  //DEBUG_L("type_of_arg: ");
	  //DEBUG_F( flexible_print, stderr, type_of_arg, 1, (dump_flags_t)0);
	  base = base_type_of ( type_of_arg);
	  //DEBUG_L("base: ");
	  //DEBUG_F( flexible_print, stderr, base, 1, (dump_flags_t)0);
	  
	  tree new_arg_type;
	  ri = get_reorgtype_info ( base, info);
	  if ( ri != NULL )
	    {
	      int levels = number_of_levels ( type_of_arg );

	      if ( number_of_levels ( type_of_arg ) == 1 )
		{
		  new_arg_type = TYPE_MAIN_VARIANT ( ri->pointer_rep);
		}
	      else
		{
		  new_arg_type = make_multilevel ( ri->pointer_rep, levels);
		}
	    }
	  else
	    {
	      new_arg_type = type_of_arg;
	    }
	  interim_args = tree_cons ( NULL_TREE, new_arg_type, interim_args);
	  //DEBUG_A("interim new_args = ");
	  //DEBUG_F( flexible_print, stderr, interim_args, 1, (dump_flags_t)0);
	}
      //DEBUG_A("before reverse interim_args = %p, ", new_args);
      //DEBUG_F( flexible_print, stderr, interim_args, 1, (dump_flags_t)0);
      tree last = new_args;
      // swithed form nreverse to reverse_args 
      new_args = reverse_args ( interim_args);
    }
  // Here
  
  //DEBUG_A("new args = %p, ", new_args);
  //DEBUG_F( flexible_print, stderr, new_args, 1, (dump_flags_t)0);
  //INDENT(-4);

  new_type = build_function_type ( func_ret_type, new_args);
  //DEBUG_L("new_type (func) = ");
  //DEBUG_F( flexible_print, stderr, new_type, 1, (dump_flags_t)0);
  TREE_TYPE ( func->decl) = new_type;
  return new_type;
}

static bool
needs_modification_p ( struct function *func, Info *info )
{
  DEBUG_A("needs_modification_p:>\n");
  tree func_type = TREE_TYPE ( func->decl);
  tree ret_type = TREE_TYPE ( func_type);
  tree base = base_type_of ( ret_type);
  if ( get_reorgtype_info ( base, info) != NULL ) return true;

  tree arg;
  for ( arg = TYPE_ARG_TYPES ( func_type); arg != NULL; arg = TREE_CHAIN ( arg))
    {
      base = base_type_of ( ret_type);
      if ( get_reorgtype_info ( base, info) != NULL ) return true;
    }
 
  return false;
}

static tree
reverse_args( tree args )
{
  tree next;
  tree arg = args;
  tree reveresed = void_list_node;
  if ( args!= NULL )
    next = TREE_CHAIN ( args);
  else
    {
      return args;
    }
  for ( arg = args; arg != NULL && arg != void_list_node;
	arg = next )
    {
      next = TREE_CHAIN ( arg);
      TREE_CHAIN ( arg) = reveresed;
      reveresed = arg;
      //DEBUG_A("partial reverse = ");
      //DEBUG_F( flexible_print, stderr, reveresed, 1, (dump_flags_t)0);
    }
  return reveresed;
}
	
std::vector<two_trees_t>::iterator
find_in_vec_of_two_types ( std::vector<two_trees_t> *types, tree type)
{
  DEBUG_A("find_in_vec_of_two_types_2nd:> ");
  DEBUG_F( flexible_print, stderr, type, 1, (dump_flags_t)0);
  INDENT(2);
  for ( auto looki = types->begin (); looki != types->end (); looki++)
    {
      tree look = looki->first;
      DEBUG_A("look = ");
      DEBUG_F( flexible_print, stderr, look, 1, (dump_flags_t)0);

      if ( same_type_p ( look, type) )
	{
	  INDENT(-2);
	  return looki;
	}
    }
  INDENT(-2);
  return types->end ();
}

std::vector<two_trees_t>::iterator
find_in_vec_of_two_types_2nd ( std::vector<two_trees_t> *types, tree type)
{
  DEBUG_A("find_in_vec_of_two_types_2nd:> ");
  DEBUG_F( flexible_print, stderr, type, 1, (dump_flags_t)0);
  INDENT(2);
  for ( auto looki = types->begin (); looki != types->end (); looki++)
    {
      tree look = looki->second;
      DEBUG_A("look = ");
      DEBUG_F( flexible_print, stderr, look, 1, (dump_flags_t)0);

      if ( same_type_p ( look, type) )
	{
	  INDENT(-2);
	  return looki;
	}
    }
  INDENT(-2);
  return types->end ();
}

int
number_of_levels ( tree type)
{
tree prev_type;
  int levels = 0;
  for ( ; TREE_CODE ( type) == POINTER_TYPE; levels++ )
    {
      prev_type = type;
      type = TREE_TYPE ( prev_type);
      //DEBUG_A( "prev_type: %p, type: %p\n", prev_type, type);
    }
  //DEBUG_A("number_of_levels = %d\n", levels);
  return levels;
}

tree
make_multilevel ( tree base_type, int levels_indirection)
{
  //DEBUG_A("make_multilevel;> %d levels of ", levels_indirection);
  //DEBUG_F( flexible_print, stderr, base_type, 1, (dump_flags_t)0);
  //INDENT(4);
  if ( levels_indirection == 0 )
    {
      //INDENT(-4);
      //DEBUG_A("returns: ");
      //DEBUG_F( flexible_print, stderr, base_type, 1, (dump_flags_t)0);
      return base_type;
    }
  else
    {
      tree lower = make_multilevel ( base_type, levels_indirection - 1);
      tree returns = build_pointer_type ( lower);
      //INDENT(-4);
      //DEBUG_A("returns: ");
      //DEBUG_F( flexible_print, stderr, returns, 1, (dump_flags_t)0);
      return returns;
    }
}

static bool
modify_func_decl_core ( struct function *func, Info *info)
{
  DEBUG_L("modify_func_decl_core:>\n");
  //INDENT(4);
  //DEBUG_A("func->decl = %p, ", func->decl);
  //DEBUG_F( flexible_print, stderr, func->decl, 1, (dump_flags_t)0);
  //DEBUG_A("TREE_TYPE (func->decl) = %p, ", TREE_TYPE (func->decl));
  //DEBUG_F( flexible_print, stderr, TREE_TYPE (func->decl), 1, (dump_flags_t)0);
  //DEBUG_A("TREE_TYPE(TREE_TYPE (func->decl)) = %p, ", TREE_TYPE(TREE_TYPE (func->decl)));
  //DEBUG_F( flexible_print, stderr, TREE_TYPE(TREE_TYPE (func->decl)), 1, (dump_flags_t)0);
  // TBD Implement
  tree *func_type_loc = &(TREE_TYPE(TREE_TYPE (func->decl)));
  tree func_type = *func_type_loc;
  tree base = base_type_of ( func_type);

  ReorgType_t *ri = get_reorgtype_info ( base, info);
  if ( ri == NULL )
    {
      //DEBUG_A("Return as not a reorg type.\n");
      //INDENT(-4);
      return false;
    }
  //DEBUG_A("pointer_rep = ");
  //DEBUG_F( flexible_print, stderr, ri->pointer_rep, 1, (dump_flags_t)0);
  //DEBUG_A("TYPE_MAIN_VARIANT( pointer_rep) = ");
  //DEBUG_F( flexible_print, stderr, TYPE_MAIN_VARIANT( ri->pointer_rep), 1, (dump_flags_t)0);
  
  int levels = number_of_levels ( func_type);

  if ( levels == 1 )
    {
      //DEBUG_A( "levels == 1\n");
      // Why type main variant pointer_rep ???
      // We created it!
      gcc_assert ( TYPE_MAIN_VARIANT ( ri->pointer_rep));
      //TREE_TYPE( *func_type_loc) = TYPE_MAIN_VARIANT ( ri->pointer_rep);
      //*func_type_loc = TYPE_MAIN_VARIANT ( ri->pointer_rep);
      TREE_TYPE ( TREE_TYPE ( func->decl)) = TYPE_MAIN_VARIANT ( ri->pointer_rep);
    }
  else
    {
      //DEBUG_A( "levels != 1\n");
      TREE_TYPE ( TREE_TYPE ( func->decl)) =
	make_multilevel ( ri->pointer_rep, levels);
    }

  //DEBUG_L("AFTER modify_func_decl_core:\n");
  //DEBUG_A("func->decl = %p, ", func->decl);
  //DEBUG_F( flexible_print, stderr, func->decl, 1, (dump_flags_t)0);
  //DEBUG_A("TREE_TYPE (func->decl) = %p, ", TREE_TYPE (func->decl));
  //DEBUG_F( flexible_print, stderr, TREE_TYPE (func->decl), 1, (dump_flags_t)0);
  //DEBUG_A("TREE_TYPE(TREE_TYPE (func->decl)) = %p, ", TREE_TYPE(TREE_TYPE (func->decl)));
  //DEBUG_F( flexible_print, stderr, TREE_TYPE(TREE_TYPE (func->decl)), 1, (dump_flags_t)0);
  
  //INDENT(-4);
  return true;
}

void
xxx( tree x )
{
  DEBUG_A("XXX  decl = ");
  DEBUG_F( flexible_print, stderr, x, 1, (dump_flags_t)0);
  if ( TREE_CODE ( x) == PARM_DECL )
    {
      DEBUG_A("XXX  decl_arg_type = ");
      DEBUG_F( flexible_print, stderr, DECL_ARG_TYPE(x), 1, (dump_flags_t)0);
    }
}

// Returns true if a modification occurred
bool
modify_decl_core ( tree *location, Info *info)
{
  DEBUG_A("modify_decl_core:> ");
  DEBUG_F( flexible_print, stderr, *location, 1, (dump_flags_t)0);
  INDENT(4);

  tree decl = *location;
  tree type = TREE_TYPE ( *location);
  
  DEBUG_A("OLD:\n");
  DEBUG_A("  decl = ");
  DEBUG_F( flexible_print, stderr, decl, 1, (dump_flags_t)0);
  if ( TREE_CODE ( decl) == PARM_DECL )
    {
      DEBUG_A("  decl_arg_type = ");
      DEBUG_F( flexible_print, stderr, DECL_ARG_TYPE(decl), 1, (dump_flags_t)0);
    }
  
  //DEBUG_A("type = ");
  //DEBUG_F( flexible_print, stderr, type, 0, (dump_flags_t)0);
  
  tree base = base_type_of ( type);
  
  //DEBUG(", base = ");
  //DEBUG_F( flexible_print, stderr, base, 1, (dump_flags_t)0);

  // Look for a modified type
  tree modified_to = find_modified ( base,
				     false,
				     info);
  tree mtv_modified_to =
    modified_to == NULL ? NULL : TYPE_MAIN_VARIANT ( modified_to);
  //DEBUG_A("Before Modify_Decl_Core\n");
  //DEBUG_A("  modified_to = ");
  //DEBUG_F( flexible_print, stderr, modified_to, 1, (dump_flags_t)0);
  
  bool use_mod = false;
  
  ReorgType_t *ri = get_reorgtype_info ( base, info);
  if ( ri == NULL )
    {
      use_mod = modified_to != NULL;
      if( !use_mod )
	{
	  DEBUG_A( "!use_mod && ri == NULL... return\n");
	  INDENT(-4);
	  return false;
	}
    }
  
  tree prev_type;
  int levels = number_of_levels ( type);
  //DEBUG_A( "levels = %d\n", levels);

  DEBUG_A("Modify TREE_TYPE\n");
  if ( use_mod )
    {
      //DEBUG_A( "  use_mod \n");
      if ( levels == 0 )
	{
	  TREE_TYPE(*location) = mtv_modified_to;
	}
      else
	{
	  TREE_TYPE(*location) = make_multilevel ( modified_to, levels);
	}
    }
  else
    {
      //DEBUG_A( "  !use_mod \n");
      // TBD, I have some doubt that this actually works right for more
      // that one level... It's not that make_multilevel is wrong, it's
      // that it seems like it will build one too many levels because
      // the arguement is off by one. Remember that transforming to
      // reorg pointer strips off one level of indirection!

      // Fakes this for levels == 1
      if ( levels == 0)
	{
	  DEBUG_A("  Not a pointer, don't modify it!\n");
	  return false;
	}
      // This also happens with other similar uses of make_multilevel.
      if ( levels == 1)
	{
	  //DEBUG_A( "  LEVEL  ONE\n");
	  gcc_assert ( TYPE_MAIN_VARIANT ( ri->pointer_rep));
	  TREE_TYPE(*location) = TYPE_MAIN_VARIANT ( ri->pointer_rep);
	}
      else
	{
	  //DEBUG_L( "  LEVEL > ONE\n");
	  TREE_TYPE(*location) = make_multilevel ( ri->pointer_rep, levels -1); // changed here
	}
    }

  //DEBUG_A("Modify DECL_INITIAL\n");
  /// TBD !!! Add levels to this!
  // I'm not sure what this does!
  if ( use_mod )
    {
      //DEBUG_A( "  use_mod \n");
      if ( DECL_INITIAL ( *location) != NULL )
	{
	  if ( levels == 0 )
	    {
	      DECL_INITIAL ( *location) = mtv_modified_to;
	    }
	  else
	    {
	      DECL_INITIAL ( *location) =
		make_multilevel ( mtv_modified_to, levels);
	    }
	}
    }
  else
    if ( DECL_INITIAL ( *location) != NULL )
      {
	//DEBUG_A( "  !use_mod \n");
	if ( levels == 1 )
	  {
	    DECL_INITIAL ( *location) = TYPE_MAIN_VARIANT ( ri->pointer_rep);
	  }
	else
	  {
	    DECL_INITIAL ( *location) =
	      make_multilevel ( ri->pointer_rep, levels -1); // changed here
	  }
      }

  relayout_decl ( *location);

  DEBUG_A("NEW:\n");
  DEBUG_A("  decl = ");
  DEBUG_F( flexible_print, stderr, *location, 1, (dump_flags_t)0);
  if ( TREE_CODE ( *location) == PARM_DECL )
    {
      DEBUG_A("  decl_arg_type = ");
      DEBUG_F( flexible_print, stderr, DECL_ARG_TYPE(*location), 1, (dump_flags_t)0);
    }
  INDENT(-4);
  return true;
}

void
delete_reorgtype ( ReorgType_t *rt, Info *info )
{
  //DEBUG_L( "delete_reorgtype( %s ):>", type_name_to_str( TYPE_NAME( rt->gcc_type)));
  if ( !rt->delete_me )
  {
    //DEBUG( "TO DELETE\n");
    info->num_deleted++;
    rt->delete_me = true;
  } else {
    //DEBUG( "SKIP\n");
  }
}

void
undelete_reorgtype ( ReorgType_t *rt, Info *info )
{
  //DEBUG_L( "undelete_reorgtype( %s ):> ", type_name_to_str( TYPE_NAME( rt->gcc_type)));
  if ( rt->delete_me )
  {
    //DEBUG( "UNDELETE\n");
    info->num_deleted--;
    rt->delete_me = false;
  } else {
    //DEBUG( "SKIP\n");
  }
}

#define REORG_RECOG_RET_ACT(rt) reorg_recognize_ret_action(rt,__LINE__)

static ReorgTransformation
reorg_recognize_ret_action( ReorgTransformation val, unsigned ln)
{
  #if DEBUGGING && 1
  fprintf ( stderr, "L# %4d: %*s  returns %s\n",
	    ln, debug_indenting, "", reorgtrans_to_str (val));
  #endif
  return val;
}

int trigger = 0;

ReorgTransformation
reorg_recognize ( gimple *stmt, cgraph_node* node, Info_t *info )
{
  DEBUG("trigger = %d\n",trigger);
  trigger++;
  DEBUG_FLA ( "ReorgTransformation reorg_recognize for:> ");
  DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
  INDENT(2);
  switch (  gimple_code( stmt) )
  {
  case  GIMPLE_ASSIGN:
    {
      DEBUG_LA ( "GIMPLE_ASSIGN:\n");
      tree lhs = gimple_assign_lhs ( stmt);
      enum tree_code rhs_code = gimple_assign_rhs_code ( stmt);
      
      if ( gimple_assign_single_p ( stmt) )
      {
	DEBUG_LA ("gimple_assign_single_p() = true\n");
	INDENT(2);
	tree rhs = gimple_assign_rhs1 ( stmt);
	enum ReorgOpTrans lhs_op = recognize_op ( lhs, true, info);
	switch ( lhs_op )
	{
	case ReorgOpT_Pointer:     // "a"
	  DEBUG_LA ( "case ReorgOpT_Pointer\n");
	  INDENT(-4);
	  switch ( recognize_op ( rhs, true, info) )
	  {
	  case ReorgOpT_Scalar:
	    {
	      if ( integer_zerop ( rhs) )
		{
		  return REORG_RECOG_RET_ACT ( ReorgT_Ptr2Zero);
		}
	      // If we get here this is clearly really odd code
	      // so we need to bail out... Nope!

	      // If seems that this code occurrs in "nature"... sigh...
	      return REORG_RECOG_RET_ACT ( ReorgT_Ignore);
	    }
	  case ReorgOpT_Temp:      // t
	    return REORG_RECOG_RET_ACT ( ReorgT_ElemAssign);
	  case ReorgOpT_Address:   // "&x[i]"
	    return REORG_RECOG_RET_ACT ( ReorgT_Adr2Ptr);
	  case ReorgOpT_Pointer:
	    return REORG_RECOG_RET_ACT ( ReorgT_PtrAssign);
	  case ReorgOpT_Cst0:
	    return REORG_RECOG_RET_ACT ( ReorgT_Ptr2Zero);
	  case ReorgOpT_Indirect:
	    return REORG_RECOG_RET_ACT ( ReorgT_ElemAssign);
	  default:
	    return REORG_RECOG_RET_ACT ( Not_Supported);
	  }
	case ReorgOpT_Struct:      // "s"
	  DEBUG_LA ( "case ReorgOpT_Struct\n");
	  INDENT(-4);
	  switch ( recognize_op ( rhs, true, info) )
	  {
	  case ReorgOpT_Deref:     // "*a"
	  case ReorgOpT_Array:     // "x[i]"
	    // Technically with a struct on both sides
	    // this could be ignored but it's
	    // better to do this at transform time.
	    // With the case commented out test_09_23
	    // exposes a bug.
	  case ReorgOpT_Struct:    // "s"
	    return REORG_RECOG_RET_ACT ( ReorgT_StrAssign);
	  default:
	    return REORG_RECOG_RET_ACT ( Not_Supported);
	  }
	case ReorgOpT_Deref:       // "*a"
	  DEBUG_LA ( "case ReorgOpT_Deref\n");
	  INDENT(-4);
	  switch ( recognize_op ( rhs, true, info) )
	  {
	  case ReorgOpT_Deref:     // "*a"
	  case ReorgOpT_Struct:    // "s"
	  case ReorgOpT_Array:     // "x[i]"
	    return REORG_RECOG_RET_ACT ( ReorgT_StrAssign);
	  default: 
	    return REORG_RECOG_RET_ACT ( Not_Supported);
	  }
	case ReorgOpT_Array:       // "x[i]"
	  DEBUG_LA ("case ReorgOpT_Array\n");
	  INDENT(-4);
	  switch ( recognize_op ( rhs, true, info) )
	  {
	  case ReorgOpT_Struct:    // "s"
	  case ReorgOpT_Deref:     // "*a"
	  case ReorgOpT_Array:     // "x[i]"
	    return REORG_RECOG_RET_ACT ( ReorgT_StrAssign);
	  default:
	    return REORG_RECOG_RET_ACT ( Not_Supported);
	  }
	case ReorgOpT_Temp:        // t
	case ReorgOpT_Scalar:      // "z"
	  {
	    DEBUG_LA ("case ReorgOpT_%s\n", lhs_op == ReorgOpT_Temp ? "Temp" : "Scalar");
	    INDENT(-4);
	    switch ( recognize_op( rhs, true, info) )
	      {
		// The type on the lhs is that of the rhs (by
		// definition) so its moving a pointer to a pointer.
	      case ReorgOpT_Pointer:  // p
	      case ReorgOpT_Scalar:      // "z"
	      case ReorgOpT_Temp:      // "t"
		//-- provisional
		//--
	      case ReorgOpT_Clobber:
	      case ReorgOpT_Cst0:
		return REORG_RECOG_RET_ACT ( ReorgT_Ignore);
	      case ReorgOpT_Indirect:  // "a->f"
	      case ReorgOpT_AryDir:    // "x[i].f"
		return REORG_RECOG_RET_ACT ( ReorgT_ElemAssign);
	      default:
		return REORG_RECOG_RET_ACT ( Not_Supported);
	      }
	  }
	case ReorgOpT_Indirect:    // "a->f"
	case ReorgOpT_AryDir:      // "x[i].f"
	  {
	    DEBUG_LA ("case ReorgOpT_%s\n", lhs_op == ReorgOpT_Indirect ? "Indirect" : "AryDir");
	    INDENT(-4);
	    switch ( recognize_op ( rhs, true, info) )
	      {
	      case ReorgOpT_Cst:       // k
	      case ReorgOpT_Temp:      // t
	      case ReorgOpT_Scalar:    // "z"
	      case ReorgOpT_Indirect:  // "a->f"
	      case ReorgOpT_AryDir:    // "x[i].f"
	      case ReorgOpT_Pointer:   // "a"
		return REORG_RECOG_RET_ACT ( ReorgT_ElemAssign);
	      case ReorgOpT_Cst0:
		{
		  if ( is_reorg_type ( TREE_TYPE (lhs), info) )
		    {
		      return REORG_RECOG_RET_ACT ( ReorgT_Ptr2Zero);
		    }
		  else
		    {
		      return REORG_RECOG_RET_ACT ( ReorgT_ElemAssign);
		    }
		}
	      default:
		return REORG_RECOG_RET_ACT ( Not_Supported);
	      }
	  }
	default:
	  return REORG_RECOG_RET_ACT ( Not_Supported);
	} // switch ( recognize_op ( lhs, true, info) )
      } else {
	DEBUG_LA("gimple_assign_single_p() = false\n");
	INDENT(2);
	tree op1 = gimple_assign_rhs1 ( stmt);
	tree op2 = gimple_assign_rhs2 ( stmt);
	DEBUG_A("op1 = %p, op2 = %p\n", op1, op2);
	DEBUG_A("");
	DEBUG_F( flexible_print, stderr, op1, 1, TDF_DETAILS);
	if ( CONVERT_EXPR_CODE_P ( gimple_assign_rhs_code ( stmt)))
	 { 
	    DEBUG_LA("CONVERT_EXPR_CODE_P (...)\n");
	    INDENT(-4);
	    return REORG_RECOG_RET_ACT ( ReorgT_Convert);
	  }

	if ( gimple_assign_rhs3 ( stmt) != NULL )
	  {
	    DEBUG_LA ("gimple_assign_rhs3 ( stmt) != NULL\n");
	    INDENT(-4);
	    return REORG_RECOG_RET_ACT ( Not_Supported);
	  }
	
	// TBD The parenthesis where a disaster here in the HL Design so
	// double check this!
	bool zero_case =
	  (    (POINTER_TYPE_P ( TREE_TYPE( op1)) && integer_zerop ( op2))
	    || (POINTER_TYPE_P ( TREE_TYPE( op2)) && integer_zerop ( op1)))
       && ( integer_zerop ( op1) || integer_zerop ( op2) );
	DEBUG_LA ("zero_case = %s\n", zero_case ? "true" : "false" );
	INDENT(-4);
	switch ( rhs_code )
	{
	case POINTER_PLUS_EXPR:
	  return REORG_RECOG_RET_ACT ( ReorgT_PtrPlusInt);
	case POINTER_DIFF_EXPR:
	  return REORG_RECOG_RET_ACT ( ReorgT_PtrDiff);
	case EQ_EXPR:
	  return REORG_RECOG_RET_ACT ( zero_case ? ReorgT_PtrNull : ReorgT_PtrEQ);
	case NE_EXPR:
	  return REORG_RECOG_RET_ACT ( zero_case ? ReorgT_PtrNotNull : ReorgT_PtrNE);
	case LE_EXPR:
	  return REORG_RECOG_RET_ACT ( ReorgT_PtrLE);
	case LT_EXPR:
	  return REORG_RECOG_RET_ACT ( ReorgT_PtrLT);
	case GE_EXPR:
	  return REORG_RECOG_RET_ACT ( ReorgT_PtrGE);
	case GT_EXPR:
	  return REORG_RECOG_RET_ACT ( ReorgT_PtrGT);
	default:
	  return REORG_RECOG_RET_ACT ( Not_Supported);
	}
      } // } else {
    }
  case GIMPLE_COND:  // Similar to assign cases
    {
      DEBUG_LA ("GIMPLE_COND:\n");
      INDENT(-2);
      //tree op1 = gimple_assign_rhs1 ( stmt);
      //tree op2 = gimple_assign_rhs2( stmt);
      tree op1 = gimple_cond_lhs ( stmt);
      tree op2 = gimple_cond_rhs ( stmt);
      enum tree_code cond_code = gimple_cond_code (stmt);
      // TBD The parenthesis were a disaster here in the HL Design so
      // double check this!
      bool zero_case =
	   ( POINTER_TYPE_P ( TREE_TYPE ( op1)) && integer_zerop ( op2))
	|| ( POINTER_TYPE_P ( TREE_TYPE ( op2)) && integer_zerop ( op1));
      switch ( cond_code )
      {
      case EQ_EXPR:
	return REORG_RECOG_RET_ACT ( zero_case ? ReorgT_If_Null : ReorgT_IfPtrEQ);
      case NE_EXPR:
	return REORG_RECOG_RET_ACT ( zero_case ? ReorgT_If_NotNull : ReorgT_IfPtrNE);
      case LE_EXPR:
	return REORG_RECOG_RET_ACT ( ReorgT_IfPtrLE);
      case LT_EXPR:
	return REORG_RECOG_RET_ACT ( ReorgT_IfPtrLT);
      case GE_EXPR:
	return REORG_RECOG_RET_ACT ( ReorgT_IfPtrGE);
      case GT_EXPR:
	return REORG_RECOG_RET_ACT ( ReorgT_IfPtrGT);
      default:
	return REORG_RECOG_RET_ACT ( Not_Supported);
      }
    }
  case  GIMPLE_CALL:
    {
      DEBUG_LA("GIMPLE_CALL:\n");
      struct cgraph_edge *edge = node->get_edge ( stmt);
      gcc_assert( edge);
      DEBUG_A("called function %s gimple_body\n",
      	      edge->callee->has_gimple_body_p() ? "has a" : "has no");
      DEBUG_A("called function inline_to %s\n",
      	      edge->callee->inlined_to ? "true" : "false");
      DEBUG_A("called function external %s\n",
      	      edge->callee->get_partitioning_class() == SYMBOL_EXTERNAL ? "true" : "false");
      
      INDENT(-2);
      if ( gimple_call_builtin_p( stmt, BUILT_IN_CALLOC ) ) return REORG_RECOG_RET_ACT ( ReorgT_Calloc);
      if ( gimple_call_builtin_p( stmt, BUILT_IN_MALLOC ) ) return REORG_RECOG_RET_ACT ( ReorgT_Malloc);
      if ( gimple_call_builtin_p( stmt, BUILT_IN_REALLOC) ) return REORG_RECOG_RET_ACT ( ReorgT_Realloc);
      if ( gimple_call_builtin_p( stmt, BUILT_IN_FREE   ) ) return REORG_RECOG_RET_ACT ( ReorgT_Free);
      
      // Instead of just returning Not_Supported we need to
      // determine if it's a user defined function in which case the
      // transformation is meaningless but the type still needs to be
      // adjusted (does transform really do this?)

      if ( is_user_function ( stmt, node, info) )
	{
	  DEBUG_A("  ReorgT_UserFunc\n");
	  return REORG_RECOG_RET_ACT ( ReorgT_UserFunc);
	}
      DEBUG_A("  Not_supported\n");
      // TBD Why is this commented out?
      //return Not_Supported;
    }
    break;
  case GIMPLE_RETURN:
    DEBUG_LA ("GIMPLE_RETURN:\n");
    INDENT(-2);
    return REORG_RECOG_RET_ACT ( ReorgT_Return);
    break;
  default:
    DEBUG_LA ( "didn't support: ");
    DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
    DEBUG( "\n");
    INDENT(-2);
    return REORG_RECOG_RET_ACT ( Not_Supported);
  }
}

static bool
is_user_function ( gimple *call_stmt, cgraph_node* node, Info *info)
{
  // I'm not 100% sure this is a great idea but it means that
  // if we know nothing about the contents of a function
  // then it shouldn't be considered a user written function
  // that is part of our program.
  struct cgraph_edge *ce;
  ce = node->get_edge ( call_stmt);
  return ce->callee->get_partitioning_class() != SYMBOL_EXTERNAL;
}

void
clear_deleted_types( Info *info)
{
  info->saved_reorg_type->clear ();
}

void
restore_deleted_types ( Info *info)
{
  while ( !info->saved_reorg_type->empty ()  )
  {
    info->reorg_type->push_back ( info->saved_reorg_type->back () );
    info->saved_reorg_type->pop_back ();
  }
}

// This routine disqualifies any and all reorg
// types in it that deserve disqualification.
// There are bizarre circumstances that could disqualify
// many types in a single statement.
void
reorg_forbidden ( gimple *stmt, Info *info )
{
  // The recognition of forbidden patterns must include casting a
  // pointer (i.e. a pointer into a Reorg field) to something of a
  // larger size (e.g. ints to longs) or doing arithmetic with them
  // (e.g. bar->foo + k) since the later assumes the structure
  // layout has not changed.
#if 0
  // TBD
  switch( gimple_code( stmt) ) {
  case :
  }
#endif
}

void 
remove_deleted_types ( Info *info, char *where, ReorgFn reorg_fn)
{
  DEBUG_LA( "remove_deleted_types:> %d to delete of %d types\n", info->num_deleted, info->reorg_type->size ());
  if ( info->show_delete )
  {
    fprintf ( info->reorg_dump_file, "DELETING REORG TYPES (%s):\n", where);
  }

  if ( info->num_deleted > 0 )
  {
    // Do delete here and run reorg_fn on each
    // deleted type
    int n = info->reorg_type->size ();
    int to = 0;
    //INDENT(2);
    for ( int from = 0; from < n; from++ )
    {
      //DEBUG_L( "%s ", type_name_to_str( TYPE_NAME( (*(info->reorg_type))[from].gcc_type)));
      //DEBUG( "< from %d, to %d > - ", from, to);
      
      if ( !(*(info->reorg_type))[from].delete_me )
      {
	//DEBUG( "NOT DELETED %d\n", from);
	
	// Save copy of removed entry
	(*(info->reorg_type))[from].delete_me = false;
	info->saved_reorg_type->push_back ( (*(info->reorg_type))[from]);

	// Delete by not copying deleted elements
	if ( from != to )
	{
	  if ( reorg_fn != NULL )
	  {
	    (*reorg_fn)( info, &(*(info->reorg_type))[ from]);
	  }
	  (*(info->reorg_type))[ to] = (*(info->reorg_type))[ from];
	  //DEBUG_A( "    move from -> to\n");
	}
	
	to++;
      } else {
	//DEBUG( "DELETE %d\n", from);
      }
    }
    //INDENT(-2);
    info->reorg_type->resize ( n - info->num_deleted);
    info->num_deleted = 0;
  }
}

#define RECOGNIZE_OP_RET_ACTION(e) recognize_op_ret_action(e,__LINE__)

static enum ReorgOpTrans
recognize_op_ret_action ( enum ReorgOpTrans e, unsigned ln )
{
  #if DEBUGGING && 1
  fprintf ( stderr, "L# %4d: %*s  recognize_op returns %s\n",
	    ln, debug_indenting, "", optrans_to_str (e));
  #endif
  return e;
}

enum ReorgOpTrans
recognize_op ( tree op,  bool lie, Info *info)
{
  DEBUG_LA("\n");
  DEBUG_A("recognize_op:> ");
  DEBUG_F( flexible_print, stderr, op, 1, TDF_DETAILS);
  enum tree_code op_code = TREE_CODE ( op);
  DEBUG_A("opcode = %s\n", code_str( op_code));
  switch ( op_code )
    {
    case INTEGER_CST:
      if ( integer_zerop ( op) )
	{
	  return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Cst0);
	}
    case REAL_CST:
    case FIXED_CST:
    case STRING_CST:
    case COMPLEX_CST:
    case VECTOR_CST:
    //case DEBUG_EXPR_DECL: // Debugging doesn't play well with reorg!
      {
	return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Cst);
      }
    case CONSTRUCTOR:
      {
	return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Clobber);
      }
    default:
      // Just drop through because we were simply picking
      // the low hanging fruit here.
      ;
    }
  
  tree type = TREE_TYPE ( op);
  DEBUG_A ( "type: ");
  DEBUG_F ( flexible_print, stderr, type, 1, (dump_flags_t)0);
  
  // From the ifdefed off code above it's probably
  // reasonable to retain the check for a reorg type
  bool a_reorg = is_reorg_type ( type, info);

  if ( op_code == SSA_NAME )
    {
      if ( POINTER_TYPE_P (type) )
	{
	  tree lower_type = TREE_TYPE ( type);
	  if ( TREE_CODE ( lower_type) == RECORD_TYPE
	       && is_reorg_type ( lower_type, info))
	    {
	      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Pointer);
	    }
	}
      // We tried returning ReorgOpT_Scalar.
      // It caused an assertion failue because
      // it was incorrectly triggering the ReorgT_Ptr2Zero
      // case with a bogus RHS.
      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Temp);
    }
  
  // This might not occur in practice
  if ( op_code == RECORD_TYPE )
    {
      // The assumption here is that this
      // is a reorg type.
      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Struct);
    }
  if ( op_code == VAR_DECL )
    {
      //tree type = TREE_TYPE ( op);
      gcc_assert ( type != NULL );
      if ( POINTER_TYPE_P (type) )
	{
	  //bool a_reorg = is_reorg_type ( type, info);
	  if ( a_reorg || !lie )
	    {
	      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Pointer);
	    }
	  else
	    {
	      // This would be for when the field of a struct
	      // element is a pointer that's not a reorg
	      // pointer. I.e. ReorgT_ElemAssign. That is
	      // this is while lie for a good purpose.	    
	      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
	    }
	}
      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
    }
  tree inner_op0 = TREE_OPERAND( op, 0);
  tree inner_op0_type = TREE_TYPE ( inner_op0);
  enum tree_code inner_op0_code = TREE_CODE ( inner_op0);
  switch ( op_code )
    {
    case ADDR_EXPR:
      {
	DEBUG_LA("op_code == ADDR_EXPR\n");
	if ( inner_op0_code == ARRAY_REF )
	  {
	    bool a_reorg = is_reorg_type ( inner_op0, info);
	    if ( a_reorg || !lie )
	      {
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Address);
	      }
	  }
	// TBD shouldn't we be testing for a reorg???
	if ( inner_op0_code == VAR_DECL )
	  {
	    tree var_type = TREE_TYPE ( inner_op0 );
	    bool a_reorg = is_reorg_type ( var_type, info);
	    if ( a_reorg || !lie )
	      {
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Address);
	      }
	  }
	return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
      }
    case COMPONENT_REF:
      {
	DEBUG_LA ( "process... COMPONENT_REF\n");
	tree inner_op1 = TREE_OPERAND( op, 1);
	enum tree_code inner_op1_code = TREE_CODE ( inner_op1);
	
	DEBUG_A ( "inner_op1 = ");
	DEBUG_F ( flexible_print, stderr, inner_op1, 0, (dump_flags_t)0);
	DEBUG ( ", TREE_CODE = %s\n", code_str( inner_op1_code));

	// Note, a reorg type can only occurr at the bottom or
	// the top, that is "rt.a.b..." or "a.b...z.rt".
	if ( tree deep_type = multilevel_component_ref ( op) )
	  {
	    DEBUG_LA ( "Is multilevel component ref: deep_type is ");
	    DEBUG_F ( flexible_print, stderr, deep_type, 1, (dump_flags_t)0);

	    tree base_type = base_type_of ( deep_type);
	    DEBUG_A("base_type = ");
	    DEBUG_F(flexible_print, stderr, base_type, 1, (dump_flags_t)0);
	    
	    bool a_deep_reorg = is_reorg_type ( base_type, info);
	    // TBD Test for modified too!

	    bool modified = find_modified ( base_type, false, info);
	    if ( a_deep_reorg || modified || !lie )
	      {
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Indirect);
	      }

	    // Just normal field reference otherwise...
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
	  }
	if ( tree_contains_a_reorgtype_p ( op, info))
	  {
	    DEBUG_LA ( "POINTER_TYPE_P ( type) : %s\n",
		       POINTER_TYPE_P ( type) ? "true" : "false");
	    if ( POINTER_TYPE_P ( type))
	      {
		DEBUG_A ( "TREE_CODE ( TREE_TYPE( type)) == RECORD_TYPE : %s\n",
			  TREE_CODE ( TREE_TYPE( type)) == RECORD_TYPE ? "true" : "false");
	      }
	  }
	if (    tree_contains_a_reorgtype_p ( op, info)
	     && POINTER_TYPE_P ( type)
	     && TREE_CODE ( TREE_TYPE( type)) == RECORD_TYPE )
	  {
	    
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Pointer);
	  }
	if ( inner_op0_code == INDIRECT_REF )
	  {
	    DEBUG_LA("TREE_CODE( inner_op) == INDIRECT_REF\n");
	    bool a_base_reorg = is_reorg_type ( base_type_of ( type), info);
	    if ( a_base_reorg || !lie )
	      {
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Indirect);
	      }
	    // Just normal field reference otherwise...
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
	  }
	if ( inner_op0_code == MEM_REF ) {
	  DEBUG_LA ( "TREE_CODE( inner_op) == MEM_REF\n");
	  tree base_of_inner_op0_type = base_type_of ( inner_op0_type);
	  DEBUG_A ( "base_of_inner_op0_type = ");
	  DEBUG_F ( flexible_print, stderr, base_of_inner_op0_type, 0, (dump_flags_t)0);
	  bool a_reorg = is_reorg_type ( base_type_of ( inner_op0_type), info);
	  if ( a_reorg || !lie )
	    {
	      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Indirect);
	    }
	  // Just normal field reference otherwise...
	  return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
	}
	if ( inner_op0_code == COMPONENT_REF )
	  {
	    DEBUG_LA ( "TREE_CODE( inner_op) == COMPONENT_REF\n");
	    tree inner_op0_0 = TREE_OPERAND ( inner_op0, 0);
	    tree inner_op0_0_type = TREE_TYPE ( inner_op0_0);
	    DEBUG_A ( "inner_op0_0 = ");
	    DEBUG_F ( flexible_print, stderr, inner_op0_0, 0, (dump_flags_t)0);
	    DEBUG ( " type = ");
	    DEBUG_F ( flexible_print, stderr, inner_op0_0_type, 0, (dump_flags_t)0);
	    DEBUG ( ", TREE_CODE = %s\n", code_str( inner_op0_code));
	    
	    bool a_reorg = is_reorg_type ( base_type_of ( inner_op0_0_type), info);
	    if ( a_reorg || !lie )
	      {
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Indirect);
	      }
	    // Just normal field reference otherwise...
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
	  }
	DEBUG_LA ( "TREE_CODE( inner_op) not indirect, component or mem ref\n");
	// Note, doesn't this ignore ARRAY_REF of this?
	// I think it's OK at least until we start supporting
	// multi-pools.
	bool a_reorg = is_reorg_type ( inner_op0_type, info);
	bool a_base_reorg = is_reorg_type ( base_type_of ( inner_op0_type), info);

	if ( ( !a_reorg && a_base_reorg) || !lie )
	  {
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_AryDir);
	  }
	// Just normal field reference otherwise...
	return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
      }
    case ARRAY_REF:
      {
	DEBUG_LA ( "process... ARRAY_REF\n");
	tree inner_op1 = TREE_OPERAND( op, 1);
	tree inner_op1_type = TREE_TYPE ( inner_op1);
	DEBUG_A ( "inner_op0, inner_op0_type = ");
	DEBUG_F ( flexible_print, stderr, inner_op0, 2, (dump_flags_t)0);
	DEBUG_F ( flexible_print, stderr, inner_op0_type, 1, (dump_flags_t)0);
	
	DEBUG_A ( "inner_op1, inner_op1_type = ");
	DEBUG_F ( flexible_print, stderr, inner_op1, 2, (dump_flags_t)0);
	DEBUG_F ( flexible_print, stderr, inner_op1_type, 1, (dump_flags_t)0);
	
	if ( tree deep_type = multilevel_component_ref ( op) )
	  {
	    DEBUG_LA ( "Is multilevel component ref (with array)\n");
	    bool a_reorg = is_reorg_type ( base_type_of ( deep_type), info);
	    if ( a_reorg || !lie )
	      {
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Indirect);
	      }
	    // Just normal field reference otherwise...
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
	  }
	
	bool a_reorg = is_reorg_type( base_type_of ( type), info);
	if ( a_reorg || !lie )
	  {
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Array);
	  }
	return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
      }
    case INDIRECT_REF:
      {
	DEBUG_LA ( "process... INDIRECT_REF\n");
	//  Do we want to chase the base type?
	// No, we care about (and transform) just
	// *r and not **...r (where r is a ReorgType.)
	bool a_reorg = is_reorg_type ( type, info);
	if( a_reorg || !lie )
	  {
	    return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Deref);
	  }
	return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
      }
    case MEM_REF:
      {
	// This needs to acknowledge that something is a reorgtype or
	// even a modified type and simply return ReorgOpT_Scalar
	// won't suffice (bug on *w_26(D) = iplus_25 where *w_26(D)
	// is ** to a reorg is where I'm seeing a problem.)
	DEBUG_LA ( "process... MEF_REF\n");
	DEBUG_A ( "inner_op0 = ");
	DEBUG_F ( flexible_print, stderr, inner_op0, 1, TDF_DETAILS);
	DEBUG_A ( "inner_op0_type = ");
	DEBUG_F ( flexible_print, stderr, inner_op0_type, 1, TDF_DETAILS);

	tree base_type = base_type_of ( type);
	DEBUG_A ( "base_type = ");
	DEBUG_F ( flexible_print, stderr, base_type, 1, TDF_DETAILS);
	bool a_reorg = is_reorg_type ( base_type, info);
	if ( a_reorg != is_reorg_type ( type, info) )
	  {
	    DEBUG_LA ( "DELTA 1!\n");
	  }

	if( a_reorg || !lie )
	  {
	    if ( POINTER_TYPE_P ( type) )
	      {
		DEBUG_LA ( "DELTA 2!\n");
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Pointer);
	      }
	    else
	      {
		return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Struct);
	      }
	  }
	return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
      }
    default:
      return RECOGNIZE_OP_RET_ACTION ( ReorgOpT_Scalar);
    }
}


// This is oddly close to find_deepest_comp_ref
tree
multilevel_component_ref ( tree op)
{
  DEBUG_A("multilevel_component_ref:> ");
  //DEBUG_F(flexible_print, stderr, op, 1, (dump_flags_t)0);
  //INDENT(2);
  tree inner_op0 = TREE_OPERAND( op, 0);
  //tree inner_op1 = TREE_OPERAND( op, 1);
  enum tree_code inner_op0_code = TREE_CODE ( inner_op0);
  //DEBUG_A("inner_op0_code = %s\n", code_str ( inner_op0_code ));
  if ( inner_op0_code == COMPONENT_REF || inner_op0_code == ARRAY_REF )
    {
      tree ret =  multilevel_component_ref ( inner_op0);
      //INDENT(-2);
      return ret;
    }
  else
    if ( inner_op0_code == MEM_REF || inner_op0_code == VAR_DECL )
      {
	if ( TREE_CODE ( op) == COMPONENT_REF )
	  {
	    tree type = TREE_TYPE (inner_op0);
	    //DEBUG_A("  found: %s, type: \n", code_str (inner_op0_code));
	    //DEBUG_F(flexible_print, stderr, type, 1, (dump_flags_t)0);
	    //INDENT(-2);
	    return type;
	  }
      }
  //DEBUG_A("  found: no deep type\n");
  //INDENT(-2);
  return NULL;
}

bool
is_reorg_type( tree rt, Info *info )
{
  tree tmv_type = TYPE_MAIN_VARIANT ( rt);
  tree type2check = tmv_type ? tmv_type : rt;
  return get_reorgtype_info ( rt, info) != NULL;  
}

tree
base_type_of ( tree type)
{
  //DEBUG_A("base_type_of:> ");
  //DEBUG_F(  flexible_print, stderr, type, 1, (dump_flags_t)0);
  
  for ( ; POINTER_TYPE_P ( type)          ||
	  TREE_CODE ( type) == ARRAY_TYPE ||
	  TREE_CODE ( type) == VAR_DECL   ||
	  TREE_CODE ( type) == PARM_DECL   
        ; type = TREE_TYPE ( type)           );

  //DEBUG_A("  returns... ");
  //DEBUG_F(flexible_print, stderr, type, 1, (dump_flags_t)0);

  return type;
}

tree
base_type_with_levels ( tree type, int *levels)
{
  //DEBUG_L("base_type_of:> ");
  //DEBUG_F( print_generic_expr, stderr, type, TDF_DETAILS);
  //DEBUG("\n");
  int lev = 0;
  bool indir;
  for ( ; (indir = POINTER_TYPE_P ( type)) ||
	  TREE_CODE ( type) == ARRAY_TYPE  ||
	  TREE_CODE ( type) == VAR_DECL    ||
	  TREE_CODE ( type) == PARM_DECL   
        ; type = TREE_TYPE ( type)            )
    {
      if ( indir ) lev++;
    }
  *levels = lev;
  return type;
}

// There are other allocations such as alloca or
// aligned allocs that I'm pretty sure are not
// a good fit for structure reorg optimization.
// Also, we do handle realloc but it doesn't
// create a new pool of memory so we ignore it here.
static bool
is_reorg_alloc_trigger ( gimple *stmt)
{
  return    gimple_call_builtin_p ( stmt, BUILT_IN_MALLOC)
         || gimple_call_builtin_p ( stmt, BUILT_IN_CALLOC);
}

static ReorgType_t * 
find_struct_type_ptr_to_struct ( tree type, Info *info)
{
  //DEBUG_L( "find_struct_type_ptr_to_struct:> ");
  if ( !POINTER_TYPE_P ( type) ) {
    //DEBUG("  bail\n");
    
    return NULL;
  }
  for ( ; POINTER_TYPE_P ( type); type = TREE_TYPE ( type) );
	  
  if ( TREE_CODE ( type) == RECORD_TYPE ) {
    //DEBUG( "  look for info\n");
    
    return get_reorgtype_info ( type, info);
  }
  //DEBUG("  fell through\n");
  
  return NULL;
}

// The applied function func can be used to search because it forces
// a return if it returns true;
void
apply_to_all_gimple ( bool (*function)(gimple *, void *), bool phis_too, void *data )
{
  struct cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
    {
      struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
      push_cfun ( func);
      basic_block bb;
      FOR_EACH_BB_FN ( bb, func)
	{
	  if ( phis_too )
	    {
	      gimple_seq seq = bb->il.gimple.phi_nodes;
	      if ( seq )
		{
		  gimple_stmt_iterator phii;
		  for ( phii = gsi_start (seq); !gsi_end_p (phii); gsi_next (&phii))
		    {
		      gimple *phi_stmt = gsi_stmt ( phii);
		      if ( (*function) ( phi_stmt, data )) return;
		    }
		}
	    }
	  gimple_stmt_iterator gsi;
	  for ( gsi = gsi_start_bb ( bb); !gsi_end_p ( gsi); gsi_next ( &gsi) )
	    {
	      gimple *stmt = gsi_stmt ( gsi);
	      // If we are searching for something then return here because
	      // it's found.
	      if ( (*function) ( stmt, data )) return;
	    }
	}
      pop_cfun ();
    }
}

// TBD Garbage just so it will compile
// What's dicey about this is it may sort of work but then I
// can see places where it wouldn't... The language has a say
// in what types are equal so maybe language hooks are involved???
bool
same_type_p ( tree a, tree b )
{
  //DEBUG_A( "same_type_p:>\n");
  //DEBUG_A( "  a: TREE_CODE = %s, name = %p\n  ",code_str(TREE_CODE(a)),TYPE_NAME(a));
  //DEBUG_A( "    ");
  //DEBUG_F(  flexible_print, stderr, a, 1, (dump_flags_t)0);
  //DEBUG_A( "  b TREE_CODE = %s, name = %p\n  ",code_str(TREE_CODE(b)),TYPE_NAME(b));
  //DEBUG_A( "    ");
  //DEBUG_F(  flexible_print, stderr, b, 1, (dump_flags_t)0);

  // This replaces part of the below
  bool a_rec = TREE_CODE ( a ) == RECORD_TYPE;
  bool b_rec = TREE_CODE ( b ) == RECORD_TYPE;
  if ( !(a_rec && a_rec) ) return false;

  // This is too strict
  //gcc_assert ( TREE_CODE ( a ) == RECORD_TYPE && TYPE_NAME ( a) != 0);
  //gcc_assert ( TREE_CODE ( b ) == RECORD_TYPE && TYPE_NAME ( b) != 0);
  
  // We could barf here iff the type names of records are missing.
  // But we'll do something dubious instead since that seems to not work.
  // Isn't that grand!
  if ( TYPE_NAME ( a) == 0 || TYPE_NAME ( b) == 0 )
    {
      //DEBUG_A( "returns %s\n", a == b ? "true" : "false");
      return a == b;
    }
	      
  bool ret = TYPE_NAME ( a) == TYPE_NAME ( b);
  
  //DEBUG_A( "returns %s\n", ret ? "true" : "false");
  
  return ret;
}

bool
is_reorg_pointer_type ( tree type, Info* info)
{
  for ( std::vector<ReorgType_t>::iterator ri = info->reorg_type->begin ();
	ri != info->reorg_type->end ();
	ri++                                                              )
    {
      // This deliberately doesn't use same_type_p because the reorg pointer
      // type's main type variant is a long int and it would be a disater to
      // look for that.
      if ( ri->pointer_rep == type ) return true;
    }
  return false;
}

// May need to add secondary map container to
// look them up or even modify the container
// type of ReorgType
ReorgType_t *
get_reorgtype_info ( tree type, Info* info)
{
  //DEBUG_A ( "get_reorgtype_info:> type = ");
  //DEBUG_F(  flexible_print, stderr, type, 1, (dump_flags_t)0);
  //
  
  // Note, I'm going to use the most stupid and slowest possible way
  // to do this. The advanage is it will be super easy and almost
  // certainly correct. It will also almost certainly need to be
  // improved but I get something out there now.

  tree tmv_type = TYPE_MAIN_VARIANT ( type);
  tree type2check = tmv_type ? tmv_type : type;
  for ( std::vector<ReorgType_t>::iterator ri = info->reorg_type->begin ();
	ri != info->reorg_type->end ();
	ri++                                                              )
  {
    // TBD the internal docs lie and same_type_p doesn't exist
    // (at least it's not available here at LTO time)
    // so this is just a place holder until I can get an answer
    // from the gcc community. Note, this is a big issue.
    // Remember, the same_type_p here is my own temporary hack.
    //DEBUG_L("");
    //DEBUG_F( print_generic_expr, stderr, type, TDF_DETAILS);
    //DEBUG("\n");
    if ( same_type_p ( ri->gcc_type, type2check ) )
    {
      //DEBUG_A( "  returns %p\n", &(*ri));
      
      return &(*ri);
    }
  }
  //DEBUG_A( "  returns NULL\n");
  return NULL;
}

// These are only used by the following to routines to pass
// information through a walking function
typedef struct hidden_info hidden_info_t;
struct hidden_info {
  ReorgType_t *found_reorg;
  Info *info;
};

static tree
detect_reorg ( tree *tp, int *dummy, void *data)
{
  DEBUG_A("detect_reorg:>\n");
  struct walk_stmt_info *walk_data = ( struct walk_stmt_info *)data;
  hidden_info_t *hi = ( hidden_info_t *)walk_data->info;
  DEBUG_L( "*tp = ");
  DEBUG_F(  flexible_print, stderr, *tp, 1, (dump_flags_t)-1);
  tree operand = base_type_of ( TREE_TYPE ( *tp));
  ReorgType_t *ri = get_reorgtype_info ( operand, hi->info);
  if ( ri != NULL )
    {
      DEBUG_A("found\n");
      hi->found_reorg = ri;
    }

  DEBUG_A("not found\n");
  return NULL_TREE;
}

ReorgType_t *
contains_a_reorgtype ( gimple *stmt, Info *info)
{
  DEBUG_L ( "contains_a_reorgtype:> ");
  DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
  //INDENT(2);

  if ( gimple_code ( stmt) == GIMPLE_PHI )
    {
      //INDENT(-2);
      tree base = base_type_of ( TREE_TYPE ( PHI_RESULT ( stmt)));
      return get_reorgtype_info ( base, info);
    }
  else if ( is_gimple_call ( stmt) )
    {
      DEBUG_A("a call\n");
      ReorgType_t *just_walker = the_reorg_walker ( stmt, info);
      
      //DEBUG_L("");
      //wolf_fence ( info);

      if ( just_walker != NULL )
	{
	  DEBUG_A("walker found something\n");
	  return just_walker;
	}
      tree return_type = function_return_type ( stmt, info);

      //DEBUG_L("");
      //wolf_fence ( info);

      DEBUG_A("return_type = ");
      DEBUG_F(  flexible_print, stderr, return_type, 1, (dump_flags_t)0);
      //if ( TREE_CODE ( return_type) == VOID_TYPE)
      if ( VOID_TYPE_P ( return_type) )
	{
	  DEBUG_A("VOID\n");
	  return NULL;
	}
      else
	{
	  DEBUG_A("Not VOID.. looking it up\n");
	  tree base = base_type_of ( return_type);
	  ReorgType_t *ret_val = get_reorgtype_info ( base, info );
	  //DEBUG_L("");
	  //wolf_fence ( info);

	  return ret_val;
	}
    }
  else
    {
      return the_reorg_walker ( stmt, info);
    }
}

static ReorgType_t *
the_reorg_walker ( gimple *stmt, Info *info )
{
  // Note walk_stmt_info is compilcated, use it's info
  // field for hidden_info
  hidden_info_t hi = { NULL, info };
  struct walk_stmt_info walk_info;     // expt
  memset ( &walk_info, 0, sizeof ( walk_info));
  walk_info.info = ( void*)&hi; //expt
  walk_gimple_op ( stmt,
		   detect_reorg,
		   &walk_info);
  //INDENT(-2);
  return hi.found_reorg;
}

static tree
detect_reorg_in_expr ( tree *tp, int *w_s, void *data)
{
  //DEBUG_A("detect_reorg_in_expr:>\n");
  //DEBUG_L("*tp = ");
  //DEBUG_F(  flexible_print, stderr, *tp, 1, (dump_flags_t)0);
  hidden_info_t *tre_hi = ( hidden_info_t *)data;
  //DEBUG_L("TREE_TYPE ( *tp) = ");
  //DEBUG_F(  flexible_print, stderr, TREE_TYPE ( *tp), 1, (dump_flags_t)0);
  tree operand = base_type_of ( TREE_TYPE ( *tp));
  //DEBUG_L("operand = %p, ", operand);

  //DEBUG_F(  flexible_print, stderr, operand, 1, (dump_flags_t)0);
  ReorgType_t *ri = get_reorgtype_info ( operand, tre_hi->info);
  if ( ri != NULL )
    {
      // If we found a reorg type save it and
      // return with a non null value to signify
      // to trip the return from the tree walk
      tre_hi->found_reorg = ri;
      return *tp;
    }
  
  return NULL_TREE;
}

// If this gets a void it goes looking for the effective type
tree
function_return_type ( gimple *stmt, Info *info )
{
  DEBUG_A( "function_return_type:> ");
  DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
  INDENT(2);
  tree frt = void_type_node; // default to void
  // next line has issues but the mechanism is sound
  tree t = *gimple_call_lhs_ptr ( stmt);
  
  DEBUG_A( "t %p\n", t);
  // Calls to a function returning void are skipped.
  if ( t != NULL )
    {
      DEBUG_A( "t = ");
      DEBUG_F( flexible_print, stderr, t, 1, (dump_flags_t)0);
      
      tree type = TREE_TYPE( t);
      DEBUG_A( "type = ");
      DEBUG_F( flexible_print, stderr, type, 1, (dump_flags_t)0);

      //tree frt = base_type_of ( type);
      frt = type;
      DEBUG_A( "frt = ");
      DEBUG_F( flexible_print, stderr, frt, 1, (dump_flags_t)0);
      tree bt = base_type_of ( type);
      bool found = false;

      if ( TREE_CODE( bt) == VOID_TYPE )
	{
	  // Find the use of lhs.  If is an assign
	  // get use the base type of it's lhs.
	  // Otherwise never mind. Note, there
	  // can be multiple uses but find the "one"
	  // that's a simple assignment to a typed
	  // variable.
	  
	  tree ssa_name = gimple_call_lhs( stmt);
	  DEBUG_A( "ssa_name = ");
	  DEBUG_F( flexible_print, stderr, ssa_name, 1, (dump_flags_t)0);

	  gimple *use_stmt;
	  imm_use_iterator iter;
	  FOR_EACH_IMM_USE_STMT ( use_stmt, iter, ssa_name)
	    {
	      if ( found ) continue;
	      DEBUG_A("use_stmt: ");
	      DEBUG_F ( print_gimple_stmt, stderr, use_stmt, 0);

	      if ( is_assign_from_ssa ( use_stmt ) )
		{
		  DEBUG_A("is assign from ssa\n");
		  tree lhs_assign = gimple_assign_lhs( use_stmt);
		  DEBUG_A( "lhs_assign = ");
		  DEBUG_F( flexible_print, stderr, lhs_assign, 1, (dump_flags_t)0);

		  tree lhs_type = TREE_TYPE ( lhs_assign);
		  DEBUG_A( "lhs_type = ");
		  DEBUG_F( flexible_print, stderr, lhs_type, 1, (dump_flags_t)0);

		  tree lhs_base_type = base_type_of ( lhs_type);
		  DEBUG_A( "lhs_base_type = ");
		  DEBUG_F( flexible_print, stderr, lhs_base_type, 1, (dump_flags_t)0);

		  //if ( TREE_CODE( lhs_base_type) != VOID_TYPE )
		  if ( !VOID_TYPE_P ( lhs_base_type) )
		    {
		      DEBUG_A("not void\n");
		      INDENT(-2);

		      // Do not a return in a FOR_EACH_IMM_USE_STMT loop
		      frt = lhs_type;
		      DEBUG_A( "frt (from lhs_type) = ");
		      DEBUG_F( flexible_print, stderr, frt, 1, (dump_flags_t)0);
		      found = true;
		    }
		}
	    }
	}
    }
  INDENT(-2);
  DEBUG_A( "return frt = ");
  DEBUG_F( flexible_print, stderr, frt, 1, (dump_flags_t)0);

  return frt;
}

bool
tree_contains_a_reorgtype_p ( tree expr, Info *info)
{
  hidden_info_t tre_hi = { NULL, info };
  // The stuff comment out was for gimple walks. Yikes!
  //struct walk_stmt_info walk_info;
  //memset ( &walk_info, 0, sizeof ( walk_info));
  //walk_info.info = ( void*)&tre_hi;
  walk_tree_1 ( &expr, detect_reorg_in_expr, (void *)&tre_hi, NULL, NULL);

  return tre_hi.found_reorg != NULL;
}

ReorgType_t *
tree_contains_a_reorgtype ( tree expr, Info *info)
{
  hidden_info_t tre_hi = { NULL, info };
  // The stuff comment out was for gimple walks. Yikes!
  //struct walk_stmt_info walk_info;
  //memset ( &walk_info, 0, sizeof ( walk_info));
  //walk_info.info = ( void*)&tre_hi;
  walk_tree_1 ( &expr, detect_reorg_in_expr, (void *)&tre_hi, NULL, NULL);

  return tre_hi.found_reorg;
}

void
print_reorg_with_msg ( FILE *file,
		       ReorgType_t *reorg,
		       int leading_space,
		       const char *msg    )
{
  fprintf ( file, "%*s%s:\n", leading_space, "", msg);
  print_reorg ( file, leading_space + 2, reorg);
}

static void
dump_reorg ( ReorgType_t *reorg)
{
  print_reorg ( stderr, 0, reorg);
}

void
print_base_reorg ( FILE *file, int leading_space, ReorgType_t *reorg, bool detailed )
{
  // TBD
  // note if reorg_perf & regular_perf are nonzero
  // synthesize and display absolute_effect & raw_effect
  if ( reorg == NULL )
    {
      fprintf ( file, "%*s(nil)\n", leading_space, "");
      return;
    }      
  
  // Note, the following is a stub.
  if ( reorg->gcc_type != NULL && TYPE_NAME ( reorg->gcc_type) != NULL  )
    {
      const char *text
	= identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( reorg->gcc_type)));
      fprintf ( file, "%*s{ type:%s, #%d, ", leading_space, "",text, reorg->id);
    }
  else
    {
      fprintf ( file, "%*s{ type:NONAME, #%d, ", leading_space, "",reorg->id);
    }
      
  if ( reorg->do_dead_field_elim )
    {
      fprintf ( file, "elim:{ ");
      // TBD
      fprintf ( file, "}, ");
    }
  
  if ( reorg->do_field_reorder )
    {
      fprintf ( file, "reorder:{ ");
      // TBD
      fprintf ( file, "}, ");
    }

  #if USE_DO_INSTANCE_INTERLEAVE
  if ( reorg->do_instance_interleave )
    {
      fprintf ( file, "inter:{ ");
      // TBD
      fprintf ( file, "%s, ",
		reorg->instance_interleave.multi_pool ? "multi" : "single" );
      // TBD When multi-pool implemented (and found) emit pointer_rep.
      fprintf ( file, "}, ");
    }
  else
    {
      fprintf ( file, "no interleave, ");
    }
  #else
  fprintf ( file, "inter:{ ");
  // TBD
  fprintf ( file, "%s, ",
	    reorg->instance_interleave.multi_pool ? "multi" : "single" );
  // TBD When multi-pool implemented (and found) emit pointer_rep.
  fprintf ( file, "}, ");
  #endif

  //DEBUG_L("reorg->reorg_ver_type = %p\n", reorg->reorg_ver_type);

  if ( reorg->reorg_ver_type != NULL && TYPE_NAME ( reorg->reorg_ver_type) != NULL )
    {
      // TBD does this belong here? How will the clone be done with elim and
      // reorder
      //DEBUG_A("%p, %p\n", reorg->reorg_ver_type, TYPE_NAME ( reorg->reorg_ver_type));
      const char *clone_name =
	identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( reorg->reorg_ver_type)));
      fprintf ( file, "%s%s", clone_name, reorg->pointer_rep ? ", " : "");
    }
  
  if ( reorg->pointer_rep != NULL && TYPE_NAME ( reorg->pointer_rep) != NULL )
    {
      // TBD does this belong here? How will the clone be done with elim and
      // reorder
      const char *pointer_name =
	identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( reorg->pointer_rep)));
      fprintf ( file, "%s", pointer_name);
    }
  
  fprintf ( file, "}\n");
  if ( detailed && reorg->reorg_ver_type != NULL )
    {
      tree field;
      for ( field = TYPE_FIELDS( reorg->reorg_ver_type); 
	    field; 
	    field = DECL_CHAIN( field))
	{
	  fprintf ( file, "%*s", leading_space + 4, "");
	  print_generic_expr ( file, field, (dump_flags_t)0);
	  fprintf ( stderr, ": ");
	  print_generic_expr ( file, TREE_TYPE ( field), (dump_flags_t)0);
	  fprintf ( file, "\n");
	}
    }
}

static void
print_base_reorgs ( FILE *file, int leading_space, Info *info, bool detailed)
{
  for ( int i = 0; i < info->reorg_type->size (); i++ ) {
    print_base_reorg ( file, leading_space, &(*(info->reorg_type))[i], detailed);
  }
}


static void
print_detailed_reorgs (FILE *file, int leading_space, Info *info)
{
  print_base_reorgs ( file, leading_space, info, true);
}

static void
print_reorgs ( FILE *file, int leading_space, Info *info)
{
  print_base_reorgs ( file, leading_space, info, false);
}

void
print_reorg ( FILE *file, int leading_space, ReorgType_t *reorg )
{
  print_base_reorg ( file, leading_space, reorg, false);
}

static void
print_progdecls ( FILE *file, int leading_space, Info * info)
{
  for ( int i = 0; i < info->prog_decl->size (); i++ ) {
    print_progdecl ( file, leading_space, &(*(info->prog_decl))[i]);
  }
}

static void
print_progdecl ( FILE *file, int leading_space, ProgDecl_t *progdecl )
{
  //INDENT(leading_space);
  //DEBUG_L( "print_progdecl check TREE_CODE = %s\n", code_str( TREE_CODE( progdecl->gcc_decl)));
  //INDENT(-leading_space);
  //DEBUG_A("");
  fprintf ( file, "%*s", leading_space, "");
  //print_generic_decl ( file, progdecl->gcc_decl, (dump_flags_t)-1);
  print_generic_decl ( file, progdecl->gcc_decl, TDF_DETAILS);
  fprintf ( file, "\n");
}

void
print_program ( FILE *file, char *msg, bool my_format,  bool internal, int leading_space, Info_t *info)
{
  struct cgraph_node *node;
  fprintf ( file, "%*s%s%s%s%s:\n",
	    leading_space, "",
	    my_format && internal ? "Program-with-internals" : "Program",
	    msg ? " (" : "", msg ? msg : "", msg ? ")" : "");
  
  // Print Global Decls
  //
  varpool_node *var;
  FOR_EACH_VARIABLE ( var)
  {
    tree decl = var->decl;
    fprintf ( file, "%*s", leading_space, "");
    print_generic_decl ( file, decl, (dump_flags_t)0);
    fprintf ( file, "\n");
  }
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
  {
    struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
    if ( my_format )
      {
	flexible_print ( file, TREE_TYPE( func->decl), 1, (dump_flags_t)0);
	print_function ( file, internal, leading_space + 4, func, info);
      }
    else
      {
	flexible_print ( file, TREE_TYPE( func->decl), 1, (dump_flags_t)0);
	dump_function_header ( file, func->decl, (dump_flags_t)0);
	dump_function_to_file ( func->decl, file, (dump_flags_t)0);
      }
  }
  
  //DEBUG ("INTERNALS PRINT:\n");
  //DEBUG_F (apply_to_all_gimple, print_internals, true, (void*)info);
}

static void
print_function ( FILE *file,
		 bool internals,
		 int leading_space,
		 struct function *func,
		 Info *info)
{
  basic_block bb;
  
  fprintf ( file, "%*sFunc: ", leading_space + 2, "");
  // Print Func Type
  print_generic_expr ( file, TREE_TYPE(TREE_TYPE( func->decl)), (dump_flags_t)0);

  // Print Func Name
  fprintf ( file, " %s ( ", lang_hooks.decl_printable_name ( func->decl, 2));

  // Print Parameter Decls
  tree parm;
  for ( parm = DECL_ARGUMENTS ( func->decl);
	parm;
	parm = DECL_CHAIN ( parm) )
    {
      print_generic_expr ( file, TREE_TYPE( parm), (dump_flags_t)0);
      fprintf ( file, "  ");
      print_generic_expr ( file, parm, (dump_flags_t)0);
      fprintf ( file, ";  ");
    }
  fprintf ( file, ")\n");
      
  // Print Local Decls
  tree decl;
  unsigned i;
  FOR_EACH_LOCAL_DECL ( func, i, decl)
    {
      fprintf ( file, "%*s", leading_space + 6, "");
      print_generic_expr ( file, TREE_TYPE( decl), (dump_flags_t)0);
      fprintf ( file, "  ");
      print_generic_expr ( file, decl, (dump_flags_t)0);
      fprintf ( file, ";\n");
    }

  FOR_EACH_BB_FN ( bb, func)
  {
    // print bb num
    fprintf ( file, "%*sBB %d:", leading_space + 4, "", bb->index );
    
    // Tried to use function gimple_dump_bb is here instead of
    // the following loop but it's worthless.

    edge e;
    edge_iterator ei;

    FOR_EACH_EDGE ( e, ei, bb->succs )
      {
	basic_block succ_bb = e->dest;
	fprintf ( file, ", BB%d", succ_bb->index);
	if ( e->flags & EDGE_TRUE_VALUE  ) fprintf ( file, " true");
	if ( e->flags & EDGE_FALSE_VALUE ) fprintf ( file, " false");
	if ( e->flags & EDGE_FALLTHRU    ) fprintf ( file, " fallthru");
      }
    fprintf ( file, "\n");

    gimple_seq seq = bb->il.gimple.phi_nodes;
    if ( seq )
      {
	gimple_stmt_iterator phii;
	for ( phii = gsi_start (seq); !gsi_end_p (phii); gsi_next (&phii))
	  {
	    fprintf ( file, "%*s", leading_space + 6, "" );
	    if ( internals )
	      {
		print_internals ( gsi_stmt ( phii), (void*) info);
	      }
	    else
	      {
		print_gimple_stmt ( file, gsi_stmt ( phii), 0, TDF_DETAILS);
	      }
	  }
      }

    gimple_stmt_iterator gsi;
      for ( gsi = gsi_start_bb ( bb);
	    !gsi_end_p ( gsi);
	    gsi_next ( &gsi) )
      {
	gimple *stmt = gsi_stmt ( gsi);
	fprintf ( file, "%*s", leading_space + 6, "" );
	// Issue: for "if" this does not print the gotos.
	if ( internals )
	  {
	    print_internals ( stmt, (void*) info);
	  }
	else
	  {
	    print_gimple_stmt ( file, stmt, 0, TDF_DETAILS);
	  }
      }
  }
}

ReorgType_t *
get_reorgtype( gimple *stmt, Info *info, int i)
{
  DEBUG_A("get_reorgtype:> i = %d, stmt = ", i);
  DEBUG_F(print_gimple_stmt, stderr, stmt, 0);
  // Looking at operands of statement, when we get to
  // the ith one, return it.
  int num_reorgs = 0;
  unsigned num_ops = gimple_num_ops ( stmt);
  unsigned j;
  for ( j = 0; j < num_ops; j++ )
    {
      tree op = gimple_op ( stmt, j);
      if ( tree_contains_a_reorgtype_p ( op, info) ) {
	if ( num_reorgs == i )
	  {
	    //DEBUG_A("op = ");
	    //DEBUG_F(  flexible_print, stderr, op, 1, (dump_flags_t)0);

	    if ( TREE_CODE ( op) == COMPONENT_REF )
	      {
		tree field_op = TREE_OPERAND ( op, 1);
		tree field_type = TREE_TYPE ( field_op);
		tree base_field_type = base_type_of ( field_type);
		//DEBUG_A("field_op = ");
		//DEBUG_F(  flexible_print, stderr, field_op, 1, (dump_flags_t)0);
		//DEBUG_A("field_type = ");
		//DEBUG_F(  flexible_print, stderr, field_type, 1, (dump_flags_t)0);
		//DEBUG_A("base_field_type = ");
		//DEBUG_F(  flexible_print, stderr, base_field_type, 1, (dump_flags_t)0);
		return get_reorgtype_info ( base_field_type, info);
	      }
	    else
	      {
		tree op_type = TREE_TYPE ( op);
		tree op_base_type = base_type_of ( op_type);
		//DEBUG_A("op_type = ");
		//DEBUG_F(  flexible_print, stderr, op_type, 1, (dump_flags_t)0);
		//DEBUG_A("op_base_type = ");
		//DEBUG_F(  flexible_print, stderr, op_base_type, 1, (dump_flags_t)0);

		return get_reorgtype_info ( op_base_type, info);
	      }
	  }
	num_reorgs++;
      }
    }
  gcc_assert ( 0);
}

int
num_reorgtypes( gimple *stmt, Info *info)
{
  DEBUG_LA("num_reorgtypes:> ");
  DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
  // Looking at operands of statement, count
  // the number that have reorg types.
  // Note, they may be (most likely are) the same as other
  // reorg types in the statement.
  int num_reorgs = 0;
  unsigned num_ops = gimple_num_ops ( stmt);
  unsigned i;
  for ( i = 0; i < num_ops; i++ )
    {
      tree op = gimple_op ( stmt, i);
      //DEBUG_A("op%d: ",i);
      //DEBUG_F( print_generic_expr, stderr, op, (dump_flags_t)-1);
      if ( tree_contains_a_reorgtype_p ( op, info) )
	{
	  num_reorgs++;
	  //DEBUG(" reorg");
	}
      //DEBUG( "\n");
    }
  return num_reorgs;
}

void
print_type ( FILE *file, tree type)
{
  const char *text
    = identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( type)));
  fprintf ( file, "type: %s\n", text);
}

bool
uses_field_of_reorgtypes( gimple *stmt, Info * info)
{
  // TBD
  // This is a bit more sophisticated than needed
  // for "hello world" so I'll defer this for a bit.
  return false;
}

void
modify_ssa_name_type ( tree ssa_name, tree type)
{
  // This rips off the code in make_ssa_name_fn.
  // Note, this probabily be made into special function
  // that is part of tree-ssanames.
  //DEBUG_L("modify_ssa_name_type ssa_name ");
  //DEBUG_F(print_generic_expr, stderr, ssa_name, (dump_flags_t)0);
  //DEBUG("\n");
  //gcc_assert ( TREE_TYPE ( type));
  //DEBUG_A("type: ");
  //DEBUG_F(print_generic_expr, stderr, type, (dump_flags_t)0);
  //DEBUG("\n");
  //DEBUG_A("TREE_TYPE(type): ");
  //DEBUG_F(print_generic_expr, stderr, TREE_TYPE(type), (dump_flags_t)0);
  //DEBUG("\n");
  //DEBUG_A("TYPE_MAIN_VARIANT(type) :");
  //DEBUG_F(print_generic_expr, stderr, TYPE_MAIN_VARIANT(type), (dump_flags_t)0);//
  //DEBUG("\n");
  //DEBUG_A("ssa_defined_default_def_p(ssa_name) %s\n",
  //  ssa_defined_default_def_p(ssa_name) ? "true" : "false");

  if ( TYPE_P ( type) )
    {
      //DEBUG_L("TYPE_P true\n");
      gcc_assert ( TYPE_MAIN_VARIANT ( type));
      TREE_TYPE ( ssa_name) = TYPE_MAIN_VARIANT ( type);
      if ( SSA_NAME_IS_DEFAULT_DEF ( ssa_name) )
	{
	  // Again this is what breaks (pretty-print) something
	  // about expecting an integer_type.
	  // Our _reorg_SP_ptr_type_type_t really is an integer
	  // type but it doesn't know it.
	  SET_SSA_NAME_VAR_OR_IDENTIFIER ( ssa_name, TYPE_MAIN_VARIANT ( type));
	}
      else
	{
	  // The following breaks defaults defs hence the check above.
	  SET_SSA_NAME_VAR_OR_IDENTIFIER ( ssa_name, NULL_TREE);
	}
    }
  else
    {
      //DEBUG_L("TYPE_P false\n");
      gcc_assert ( TREE_TYPE ( type));
      TREE_TYPE ( ssa_name) = TREE_TYPE ( type);
      SET_SSA_NAME_VAR_OR_IDENTIFIER ( ssa_name, type);
    }
}

bool
is_assign_from_ssa ( gimple *stmt )
{
  if ( is_gimple_assign ( stmt) )
    {
      //DEBUG_A("is gimple assign\n");
      if ( gimple_assign_rhs_class ( stmt) == GIMPLE_SINGLE_RHS )
	{
	  //DEBUG_A("has single rhs\n");
	  if ( TREE_CODE ( gimple_assign_rhs1 ( stmt)) == SSA_NAME )
	    return true;
	}
    }
  return false;
}

//-- debugging only --
//static const char *
#if DEBUGGING
const char *
code_str( enum tree_code tc)
{
  switch ( tc )
  {
  case POINTER_TYPE:
    return "POINTER_TYPE";
  case RECORD_TYPE:
    return "RECORD_TYPE";
  case UNION_TYPE:
    return "UNION_TYPE";
  case ARRAY_TYPE:
    return "ARRAY_TYPE";
  case REFERENCE_TYPE:
    return "REFERENCE_TYPE";
  case VOID_TYPE:
    return "VOID_TYPE";
  case VAR_DECL:
    return "VAR_DECL";
  case TYPE_DECL:
    return "TYPE_DECL";
  case CONST_DECL:
    return "CONST_DECL";
  case PARM_DECL:
    return "PARM_DECL";
  case FIELD_DECL:
    return "FIELD_DECL";
  case FUNCTION_DECL:
    return "FUNCTION_DECL";
  case RESULT_DECL:
    return "RESULT_DECL";
  case COMPONENT_REF:
    return "COMPONENT_REF";
  case INDIRECT_REF:
    return "INDIRECT_REF";
  case MEM_REF:
    return "MEM_REF";
  case DEBUG_EXPR_DECL: // We should never ever see this!
    return "DEBUG_EXPR_DECL";
  default:
    return get_tree_code_name ( tc);
  }
}
#endif

const char *
type_name_to_str ( tree tn)
{
  gcc_assert ( tn != NULL );
  gcc_assert ( IDENTIFIER_POINTER ( tn) != NULL );
  return identifier_to_locale ( IDENTIFIER_POINTER ( tn));
}

const char *
optrans_to_str ( enum ReorgOpTrans e )
{
  switch ( e )
    {
    case ReorgOpT_Temp:
      return "ReorgOpT_Temp";
    case ReorgOpT_Address:
      return "ReorgOpT_Address";
    case ReorgOpT_Pointer:
      return "ReorgOpT_Pointer";
    case ReorgOpT_Struct:
      return "ReorgOpT_Struct";
    case ReorgOpT_Deref:
      return "Deref";
    case ReorgOpT_Array:
      return "ReorgOpT_Array";
    case ReorgOpT_Scalar:
      return "ReorgOpT_Scalar";
    case ReorgOpT_Indirect:
      return "ReorgOpT_Indirect";
    case ReorgOpT_AryDir:
      return "ReorgOpT_AryDir";
    case ReorgOpT_Cst:
      return "ReorgOpT_Cst";
    case ReorgOpT_Cst0:
      return "ReorgOpT_Cst0";
    case ReorgOpT_Clobber:
      return "ReorgOpT_Clobber";
    default:
      gcc_assert (0);
    }
  return NULL;
}

char *
reorgtrans_to_str ( enum ReorgTransformation t)
{
  switch ( t )
    {
    case ReorgT_StrAssign:
      return "ReorgT_StrAssign";
    case ReorgT_ElemAssign:
      return "ReorgT_ElemAssign";
    case ReorgT_PtrAssign:
      return "ReorgT_PtrAssign";
    case ReorgT_If_Null:
      return "ReorgT_If_Null";
    case ReorgT_If_NotNull:
      return "ReorgT_If_NotNull";
    case ReorgT_IfPtrEQ:
      return "ReorgT_IfPtrEQ";
    case ReorgT_IfPtrNE:
      return "ReorgT_IfPtrNE";
    case ReorgT_IfPtrLT:
      return "ReorgT_IfPtrLT";
    case ReorgT_IfPtrGT:
      return "ReorgT_IfPtrGT";
    case ReorgT_IfPtrLE:
      return "ReorgT_IfPtrLE";
    case ReorgT_IfPtrGE:
      return "ReorgT_IfPtrGE";
    case ReorgT_PtrPlusInt:
      return "ReorgT_PtrPlusInt";
    case ReorgT_Ptr2Zero:
      return "ReorgT_Ptr2Zero";
    case ReorgT_PtrDiff:
      return "ReorgT_PtrDiff";
    case ReorgT_Adr2Ptr:
      return "ReorgT_Adr2Ptr";
    case ReorgT_PtrNull:
      return "ReorgT_PtrNull";
    case ReorgT_PtrNotNull:
      return "ReorgT_PtrNotNull";
    case ReorgT_PtrEQ:
      return "ReorgT_PtrEQ";
    case ReorgT_PtrNE:
      return "ReorgT_PtrNE";
    case ReorgT_PtrLT:
      return "ReorgT_PtrLT";
    case ReorgT_PtrLE:
      return "ReorgT_PtrLE";
    case ReorgT_PtrGT:
      return "ReorgT_PtrGT";
    case ReorgT_PtrGE:
      return "ReorgT_PtrGE";
    case ReorgT_Malloc:
      return "ReorgT_Malloc";
    case ReorgT_Calloc:
      return "ReorgT_Calloc";
    case ReorgT_Realloc:
      return "ReorgT_Realloc";
    case ReorgT_Free:
      return "ReorgT_Free";
    case ReorgT_UserFunc:
      return "ReorgT_UserFunc";
    case ReorgT_Convert:
      return "ReorgT_Convert";
    case ReorgT_Return:
      return "ReorgT_Return";
    case ReorgT_Ignore:
      return "ReorgT_Ignore";
    case Not_Supported:
      return "Not_Supported";
    }
}

#if DEBUGGING
void
handle_debug_indenting ( int amount )
{
  debug_indenting += amount;
  debug_indenting = MAX ( debug_indenting, 0);
}
#endif

#if DEBUGGING
#if 0
// A Wolf Fence is whatever it needs to be whenever it needs to be it.
int
wf_func ( tree *slot, tree *dummy)
{
  tree t_val = *slot;
  gcc_assert( t_val->ssa_name.var);
  return 0;
}
#endif

#if 0
void
wolf_fence (
	     Info *info // Pass level gobal info (might not use it)
	   )
{
  struct cgraph_node *node;

  //fprintf( stderr,
  //	   "Wolf Fence: Find wolf via gcc_assert(t_val->ssa_name.var)\n");
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
    {
      struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
      push_cfun ( func);
      
      DEFAULT_DEFS ( func)->traverse_noresize < tree *, wf_func> ( NULL);

      pop_cfun ();
    }
  fprintf( stderr, "No Wolf\n");
}
#endif

#if 0
void
wolf_fence (
	     Info *info // Pass level gobal info (might not use it)
	   )
{
  struct cgraph_node *node;

  //fprintf( stderr,
  //	   "Wolf Fence: Find wolf via gcc_assert(t_val->ssa_name.var)\n");
  
  fprintf( stderr, "Wolf?\n");
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
    {
      struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
      push_cfun ( func);
      
      unsigned int len = SSANAMES ( func)->length ();
      for ( unsigned int i = 0; i < len; i++)
	{
	  tree ssa_name = (*SSANAMES ( func))[i];
	  if ( ssa_name == NULL ) continue;
	  if ( SSA_NAME_IS_DEFAULT_DEF ( ssa_name) )
	    {
	      gimple *def_stmt = SSA_NAME_DEF_STMT ( ssa_name);
	      if ( !gimple_nop_p ( def_stmt) )
		{
		  fprintf ( stderr, "Wolf! : ");
		  print_gimple_stmt ( stderr, def_stmt, 0);
		  gcc_assert (0);
		}
	    }
	}
      pop_cfun ();
    }
  fprintf( stderr, "No Wolf\n");
}
#endif

#if 0
void
wolf_fence (
	     Info *info // Pass level gobal info (might not use it)
	   )
{
  if ( ssa_check ( stderr, Show_failures, Fail_1st_bad, false, false) )
    {
      fprintf ( stderr, "Wolf!\n");
      gcc_assert (0);
    }

  fprintf( stderr, "No Wolf\n");  
}
#endif

#if 0
void
wolf_fence (
	     Info *info // Pass level gobal info (might not use it)
	   )
{
  for ( auto ri = info->reorg_type->begin ();
	ri != info->reorg_type->end ();
	ri++                                                              )
    {
      if ( ri->reorg_ver_type != NULL )
	{
	  fprintf ( stderr, "Wolf!\n");
	  gcc_assert (0);
	}
    }
  fprintf( stderr, "No Wolf\n");
}
#endif

void
wolf_fence (
	     Info *info // Pass level gobal info (might not use it)
	   )
{
  struct cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
    {
      struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
      push_cfun ( func);
      basic_block bb;
      FOR_EACH_BB_FN ( bb, func)
	{
	  gimple_stmt_iterator gsi;
	  for ( gsi = gsi_start_bb ( bb); !gsi_end_p ( gsi); gsi_next ( &gsi) )
	    {
	      gimple *stmt = gsi_stmt ( gsi);

	      if ( !is_gimple_call ( stmt) ) continue;
	      
	      DEBUG_L("stmt = ");
	      DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);

	      imm_use_iterator iter;
	      use_operand_p use_p;
	      tree var = gimple_call_lhs( stmt);
	      if ( var == NULL ) continue;

	      DEBUG_A("var = ");
	      DEBUG_F(  flexible_print, stderr, var, 1, (dump_flags_t)0);

	      FOR_EACH_IMM_USE_FAST (use_p, iter, var)
		{
		  gimple *use_stmt = USE_STMT (use_p);
		  if ( use_stmt == (gimple *)0x40ec8348e5894855 )
		    {
		      fprintf ( stderr, "Wolf!\n");
		      gcc_assert (0);
		    }
		  DEBUG_L("use_stmt = ");
		  DEBUG_F ( print_gimple_stmt, stderr, use_stmt, 0);
		}
	    }
	}
      pop_cfun ();
    }
  fprintf( stderr, "No Wolf\n");
}

// returns true for failure
bool
ssa_check ( FILE *file, Display display, Failure failure, bool types, bool header )
{
  if ( header ) fprintf ( file, "ssa_check:\n");
  struct cgraph_node *node;
  bool has_a_failure = false;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
    {
      struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
      push_cfun ( func);
      
      unsigned int len = SSANAMES ( func)->length ();
      for ( unsigned int i = 0; i < len; i++)
	{
	  tree ssa_name = (*SSANAMES ( func))[i];
	  if ( ssa_name == NULL ) continue;
	  bool a_default_def = SSA_NAME_IS_DEFAULT_DEF ( ssa_name);
	  gimple *defining_stmt = SSA_NAME_DEF_STMT ( ssa_name);;
	  bool no_defining_stmt = defining_stmt == NULL;
	  bool defined_by_nop = defining_stmt && gimple_code ( defining_stmt) == GIMPLE_NOP;
	  bool has_type = TREE_TYPE ( ssa_name) != NULL;
	  tree type = TREE_TYPE ( ssa_name);
	  tree bottom_type = base_type_of ( type);
	  bool fails = !has_type                           ||
	                no_defining_stmt                   ||
	                (a_default_def && !defined_by_nop) ||
	                (!a_default_def && defined_by_nop);
	  if (fails) has_a_failure = true;
	  if ( display == Show_everything || (fails && display == Show_failures) )
	    {
	      fprintf ( file, "ssa_name = ");
	      print_generic_expr ( file, ssa_name, (dump_flags_t)0);
	      fprintf ( file, "%s", has_type ? ", has no type" : "");
	      fprintf ( file, "%s", a_default_def ? ", is default_def" : "");
	      fprintf ( file, "%s", no_defining_stmt ? ", has no defining stmt" : "");
	      fprintf ( file, "%s", defined_by_nop ? ", defined by a nop" : "");
	      if (types)
		{
		  fprintf ( file, ", type = ");
		  print_generic_expr ( file, type, (dump_flags_t)0);
		  fprintf ( file, ", bottom_type = ");
		  print_generic_expr ( file, bottom_type, (dump_flags_t)0);
		}
	      fprintf ( file, "\n");
	    }
	  if ( has_a_failure && Fail_1st_bad == failure ) break;
	}
      pop_cfun ();
      if ( has_a_failure && Fail_1st_bad == failure ) break;
    }
  return failure != Do_not_fail && has_a_failure;
}

#endif
void
flexible_print( FILE *f, tree t, int nl, dump_flags_t d)
{
  if ( t == NULL )
    {
      fprintf ( f, "(nil)");
    }
  else
    {
      if ( DECL_P( t) )
	{
	  print_generic_decl(f,t,d);
	}
      else
	{
	  print_generic_expr(f,t,d);
	}
    }
  if ( nl == 1 ) fprintf ( f, "\n");
  if ( nl == 2 ) fprintf ( f, ", ");
}

//---------------- Pass Control Follows ----------------

const pass_data pass_data_ipa_structure_reorg =
{
  IPA_PASS, /* type */
  "structure-reorg", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_IPA_STRUCTURE_REORG, /* tv_id */
  0, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  //( TODO_dump_symtab | TODO_remove_functions ), /* todo_flags_finish */ // ???
  0, /* todo_flags_finish */ // ???
};

class pass_ipa_structure_reorg : public simple_ipa_opt_pass
{
public:
  pass_ipa_structure_reorg ( gcc::context *ctxt)
    : simple_ipa_opt_pass ( pass_data_ipa_structure_reorg, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate ( function *)
    {
      return ( ( flag_ipa_structure_reorg     ||
		 flag_ipa_instance_interleave ||
		 flag_ipa_field_reorder       ||
		 flag_ipa_dead_field_eliminate   )
	       && in_lto_p );
    }

  virtual unsigned int execute ( function *) { return ipa_structure_reorg (); }

}; // class ipa_structure_reorg

//ipa_opt_pass_d *
simple_ipa_opt_pass *
make_pass_ipa_structure_reorg ( gcc::context *ctxt)
{
  return new pass_ipa_structure_reorg ( ctxt);
}
