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

#include <vector>
#include <map>
#include <set>
#include <list>
#include <algorithm>
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "tree-ssa.h"
#include "tree-ssa-loop-ivopts.h"
#include "tree-dfa.h"
#include "gimple.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "gimple-iterator.h"
#include "pretty-print.h"
#include "ipa-structure-reorg.h"
#include "dumpfile.h"
#include "tree-pretty-print.h"
#include "gimple-pretty-print.h"
#include "langhooks.h"
#include "stringpool.h"
#include "stor-layout.h"
#include "diagnostic-core.h"
#include "ssa.h"
#include "tree-ssanames.h"
#include "cfghooks.h"
#include "function.h"
#include "cfgloop.h"
#include "wide-int.h"

typedef struct acc_base_info acc_base_info_t;
typedef struct acc_info acc_info_t;
typedef struct varInfo varInfo_t;

static void wrangle_ssa_type( tree, Info_t*);
//static bool print_internals (gimple *, void *);
static void str_reorg_instance_interleave_qual_part ( Info *);
static void str_reorg_instance_interleave_type_part ( Info *);
static void header ( bool);
static void print_var_infos ( FILE *, std::vector<varInfo_t> &);
static void compress_acc_infos ( std::vector <acc_info_t>);
static void print_acc_info ( FILE *, acc_info_t *);
static void print_acc_infos ( FILE *, std::vector <acc_info_t>);
static bool acc_lt ( const acc_info_t&, const acc_info_t&);
static bool acc_eq ( const acc_info_t&, const acc_info_t&);
static bool all_but_field_eq ( const acc_info_t&, const acc_info_t&);
static double cut_off_eq_single_pool( double);
static double alignment_effect( unsigned HOST_WIDE_INT);
static void tell_me_about_ssa_name ( tree, int);
static void analyze_access ( tree , acc_info_t *);
static void create_new_types ( Info_t *);
static void create_a_new_type ( Info_t *, tree);
static unsigned int reorg_perf_qual ( Info *);
static tree find_coresponding_field ( tree, tree);
static void remove_default_def ( tree, struct function *);
static void set_lhs_for ( gimple *, tree);
static basic_block make_bb ( char *, basic_block);

// These are local to this file by design
#define REORG_SP_PTR_PREFIX "_reorg_SP_ptr_type_"
#define REORG_SP_PREFIX "_reorg_base_type_"
#define REORG_SP_BASE_PREFIX "_reorg_base_var_"

// TBD Delete all this after sending a note on it
// to the gcc mailing list.
#define USE_BUILT_IN_FREE 1

/*
0000 ssa_verify error loc_3
0001 tree_class_check err in interleave
0010 ssa_verify error arr_46
0011 ssa_verify error loc_3
0100 ssa_verify error loc_3
0101 ssa_verify error arr_46
0110 ssa_verify error arr_46
0111 ssa_verify error arr_46
1000 ssa_verify error loc_3
1001 bad failure: walk return bogus op
1010 bad failure: walk return bogus op
1011 bad failure: walk return bogus op
1100 bad failure: walk return bogus op
1101 bad failure: walk return bogus op
1110 bad failure: walk return bogus op
1111 bad failure: walk return bogus op
*/

// These are dummy values tha alway result the reorganization
#define SINGLE_POOL_RAW_SKIP_IT      0.05
#define SINGLE_POOL_RAW_DO_IT_ALWAYS 0.90
#define SINGLE_POOL_ABS_SKIP_IT      0.02
#define SINGLE_POOL_ABS_DO_IT_ALWAYS 0.10
  
int
str_reorg_instance_interleave_qual ( Info *info)
{
  // this is the qualification code for instance interleaving
  //
  str_reorg_instance_interleave_qual_part ( info);

  //if ( BYPASS_TRANSFORM ) return 0;
  // this modifiies the qualified types.
  //
  str_reorg_instance_interleave_type_part ( info);
  return 0;
}

int
str_reorg_instance_interleave_trans ( Info *info)
{
  if ( BYPASS_TRANSFORM )
    {

      fprintf ( stderr, "Bypassing str_reorg_instance_interleave_trans for experiment\n");
      return 0;
    }
  
  if ( info->show_all_reorg_cands )
  {
    fprintf ( info->reorg_dump_file, "Start of str_reorg_instance_interleave_trans:\n");
    print_program ( info->reorg_dump_file, PRINT_FORMAT, 4, info);
  }

  //DEBUG ("INTERNALS PRINT\n");
  //DEBUG_F (apply_to_all_gimple, print_internals, true, (void *)info);
  
  struct cgraph_node *node;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)  {
    struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
    // Ulgy GCC idiom with global pointer to current function.
    push_cfun ( func);
    if ( info->show_transforms )
      {
        fprintf( info->reorg_dump_file, "Function \"%s\":\n",
		 //IDENTIFIER_POINTER( DECL_NAME( func)));
		 //IDENTIFIER_POINTER( DECL_NAME( func->decl)));
		 lang_hooks.decl_printable_name ( node->decl, 2));
      }

    basic_block bb;
    FOR_EACH_BB_FN ( bb, func)
      {

	if( info->show_transforms )
	  {
	    fprintf( info->reorg_dump_file, "  Transforming BB%i:\n",
		     bb->index);
	  }

	gimple_stmt_iterator outer_gsi;
	gimple_stmt_iterator next_gsi;
	for ( outer_gsi = gsi_start_bb ( bb); !gsi_end_p ( outer_gsi); outer_gsi = next_gsi )
	  {
	    next_gsi = outer_gsi;
	    gsi_next ( &next_gsi);
	    // Every statement that uses a reorg type needs to
	    // be examined. Some are harmless and are skipped
	    // whereas others are transformed. However, anything
	    // else is an error.
	    gimple *stmt = gsi_stmt ( outer_gsi);
	    ReorgType_t *ri = contains_a_reorgtype( stmt, info);
	    if ( ri == NULL )
	      {
		//DEBUG_L("No Transfrom on: ");
		//DEBUG_F( print_gimple_stmt, stderr, stmt, 4, TDF_SLIM);
	      }
	    else
	      {
		//DEBUG_F( print_reorg_with_msg, stderr, ri, 0,
		//	 "reorg from str_reorg_instance_interleave_trans");
		
		enum ReorgTransformation trans = 
		  reorg_recognize ( stmt, node, info);
		// print out trans and stmt if dumping
		if ( info->show_transforms )
		  {
		    print_gimple_stmt( info->reorg_dump_file, stmt, 0);
		  }

		switch ( trans)
		  { 
		  case ReorgT_StrAssign:
		    DEBUG_L("ReorgT_StrAssign\n");
		    // TBD
		    /*
	      tree lhs = gimple_assign_lhs( stmt);
	      tree rhs = gimple_assign_rhs( stmt);
	      ReorgOpTrans lope = recognize_op( lhs, info);
	      ReorgOpTrans rope = recognize_op( rhs, info);
	      for each field in ri {
	        // lhs: ReorgT_Array & rhs ReorgT_Struct, ReorgT_Deref, ReorgT_Array
		// lhs: ReorgT_Struct & rhs ReorgT_Deref, ReorgT_Array
		// lhs ReorgT_Deref & rhs ReorgT_Struct, ReorgT_Array, ReorgT_Deref
		A is new ssa
		// Gimple for loading this element
		// Question? What if the element is large? Answer is it's OK.
		switch( rope) {
		// Not implemented in single pool
		//case ReorgT_Array:
		case ReorgT_Struct:
		  generate A <- rhs.field  
		  break;
		case ReorgT_Deref:
		  B,C is new SSA
		  // Note simplification with type_name( rhs)
		  generate B <- concat( REORG_SP_PREFIX, type_name( rhs))
		    and insert before stmt
		  generate C <- B->"f"
		    and insert before stmt
		  generate A <- C[rhs]
		    and insert before stmt
		  break
		default:
		  internal_error(
		    "Reached operand default in RHS enum ReorgOpTrans");
		}
		// Gimple for storing this element
		switch( lope)
		// Not implemented in single pool
		//case ReorgT_Array:
		case ReorgT_Deref:
		  B,C is new SSA
		  // Note simplification with type_name( lhs)
		  generate B <- concat( REORG_SP_PREFIX, type_name( lhs))
		    and insert before stmt
		  generate C <- B->"f"
		    and insert before stmt
		  // lhs here is a simplification
		  generate A <- C[lhs]
		    and insert before stmt
		  break;
		case ReorgT_Struct:
		  generate lhs.field <- A
		  break;
		default:
		  internal_error(
		    "Reached operand default in LHS enum ReorgOpTrans");
		}
	     }
		    */
		    break;
		  case ReorgT_ElemAssign:
		    {
		      //break;
		      gimple_stmt_iterator gsi = gsi_for_stmt( stmt);
		      
		      DEBUG_L("ReorgT_ElemAssign: ");
		      DEBUG_F( print_gimple_stmt, stderr, stmt, 0);
		      //INDENT(2);
		      // Needed for helloworld
		      tree lhs = gimple_assign_lhs( stmt);
		      tree rhs = gimple_assign_rhs1( stmt);

		      bool ro_on_left = tree_contains_a_reorgtype_p ( lhs, info);
		      
		      tree ro_side = ro_on_left ? lhs : rhs;
		      tree nonro_side = ro_on_left ? rhs : lhs;
		      
		      switch ( recognize_op ( ro_side, true, info) )  // "a->f"
			{
			case ReorgOpT_Indirect:
			  {
			    tree orig_field = TREE_OPERAND( ro_side, 1);
			    tree field_type = TREE_TYPE( orig_field);
			    tree base = ri->instance_interleave.base;
			    
			    tree base_field =
			      find_coresponding_field ( base, orig_field);
			    
			    tree base_field_type = TREE_TYPE( base_field);

			    gcc_assert ( field_type);
			    tree field_val_temp =
			      make_temp_ssa_name( field_type, NULL, "field_val_temp");

			    tree inner_op = TREE_OPERAND( ro_side, 0);
			    inner_op = TREE_OPERAND( inner_op, 0);
			    //DEBUG_L("inner_op: ");
			    //DEBUG_F( print_generic_expr, stderr, inner_op, (dump_flags_t)0);
			    //DEBUG("\n");
			    
			    // For either case generate common code:
			    
			    // field_array = _base.f
			    gcc_assert ( base_field_type);
			    tree field_arry_addr =
			      make_temp_ssa_name( base_field_type, NULL, "field_arry_addr");

			    tree rhs_faa = build3 ( COMPONENT_REF,
						    // ???
						    base_field_type, 
						    //ptr_type_node, // This seems bogus
						    base,
						    base_field,
						    // This almost certainly is bogus
						    // If this "works" the the types
						    // of fields are messed up.
						    //orig_field,
						    NULL_TREE);

		            // Use this to access the array of element.
			    gimple *get_field_arry_addr =
			      gimple_build_assign( field_arry_addr, rhs_faa);
			    SSA_NAME_DEF_STMT ( field_arry_addr) = get_field_arry_addr;

			    // index = a
			    gcc_assert ( sizetype);
			    tree index =
			      make_temp_ssa_name( sizetype, NULL, "index");
			    gimple *get_index =
			      gimple_build_assign( index, CONVERT_EXPR, inner_op);
			    SSA_NAME_DEF_STMT ( index) = get_index;

			    gimple *temp_set;
			    gimple *final_set;

			    // offset = index * size_of_field
			    tree size_of_field = TYPE_SIZE_UNIT ( base_field_type);
			    gcc_assert ( sizetype);
			    tree offset = make_temp_ssa_name( sizetype, NULL, "offset");

			    gimple *get_offset = gimple_build_assign ( offset, MULT_EXPR, index, size_of_field);
			    SSA_NAME_DEF_STMT ( offset) = get_offset;

			    // field_addr = field_array + offset
			    gcc_assert ( base_field_type);
			    tree field_addr =
			      make_temp_ssa_name( base_field_type, NULL, "field_addr");

			    gimple *get_field_addr = 
			      gimple_build_assign ( field_addr, POINTER_PLUS_EXPR, field_arry_addr, offset);
			    SSA_NAME_DEF_STMT ( field_addr) = get_field_addr;

			    if ( ro_on_left )
			      {
				// With:    a->f = rhs
				// Generate:
				
				//           temp = rhs
				temp_set = gimple_build_assign( field_val_temp, rhs);
				SSA_NAME_DEF_STMT ( field_val_temp) = temp_set;
				
				////           field_array[index] = temp
				//tree elem_to_set =
				//  build4 ( ARRAY_REF, field_type, field_arry_addr, index,
				//	   NULL_TREE, NULL_TREE);
				//final_set =
				//  gimple_build_assign( elem_to_set, field_val_temp);

				//                 *field_addr = temp
				tree lhs_ref = build2 ( MEM_REF, field_type, field_addr,
							build_int_cst (ptr_type_node, 0));
				
				final_set =
				  gimple_build_assign( lhs_ref, field_val_temp);
			      }
			    else
			      {
				// With:    lhs = a->f
				// Generate:
				
				// Tried other idioms here (tricky)
				tree rhs_ref = build2 ( MEM_REF, field_type, field_addr,
							build_int_cst (ptr_type_node, 0));
				

				// If these will actually print then things are likely sane
				//DEBUG_L("rhs_ref: ");
				//DEBUG_F(print_generic_expr, stderr, rhs_ref, (dump_flags_t)0);
				//DEBUG("\n");
				
				tree op0 = TREE_OPERAND ( rhs_ref, 0);
				tree op1 = TREE_OPERAND ( rhs_ref, 1);
				tree op1type = TYPE_MAIN_VARIANT (TREE_TYPE (op1));
				tree op1type_type = TREE_TYPE ( op1type);
				
				temp_set =
				  gimple_build_assign( field_val_temp, rhs_ref);
				SSA_NAME_DEF_STMT ( field_val_temp) = temp_set;
				
				//          lhs = temp
				final_set = gimple_build_assign( lhs, field_val_temp);
				SSA_NAME_DEF_STMT ( lhs) = final_set;
			      }
			    
			    //DEBUG_L("get_field_arry_addr: ");
			    //DEBUG_F( print_gimple_stmt, stderr, get_field_arry_addr, 0);
			    //DEBUG("\n");
			    
			    //DEBUG_L("get_index: ");
			    //DEBUG_F( print_gimple_stmt, stderr, get_index, 0);
			    //DEBUG("\n");

			    //DEBUG_L("get_offset: ");
			    //DEBUG_F( print_gimple_stmt, stderr, get_offset, 0);
			    //DEBUG("\n");

			    //DEBUG_L("get_field_addr: ");
			    //DEBUG_F( print_gimple_stmt, stderr, get_field_addr, 0);
			    //DEBUG("\n");
			    
			    //DEBUG_L("temp_set: ");
			    //DEBUG_F( print_gimple_stmt, stderr, temp_set, 0);
			    //DEBUG("\n");
			    
			    //DEBUG_L("final_set: ");
			    //DEBUG_F( print_gimple_stmt, stderr, final_set, 0);
			    //DEBUG("\n");

			    gsi_insert_before( &gsi, get_field_arry_addr, GSI_SAME_STMT);
			    gsi_insert_before( &gsi, get_index, GSI_SAME_STMT);
			    gsi_insert_before( &gsi, get_offset, GSI_SAME_STMT);
			    gsi_insert_before( &gsi, get_field_addr, GSI_SAME_STMT);
			    gsi_insert_before( &gsi, temp_set, GSI_SAME_STMT);
			    gsi_insert_before( &gsi, final_set, GSI_SAME_STMT);
			    
			      
			    //delete stmt
			    gsi_remove ( &gsi, true);
			  } // end ReorgOpT_Indirect case
			  break;
			case ReorgOpT_AryDir:  // "x[i].f"
			  // Not implemented in single pool
			  internal_error ( "ReorgOpT_AryDir not possible");
			default:
			  internal_error (
					  "Reached operand default for ReorgOpT_Indirect");
			  
			} // end recognize_op ( rhs, info) switch

		      //INDENT(-2);
		    } // end ReorgT_ElemAssign case
		    break;
		  case ReorgT_If_Null:
		  case ReorgT_If_NotNull:
		    {
		      //DEBUG_L("ReorgT_If_(Not)Null: ");
		      //DEBUG_F( print_gimple_stmt, stderr, stmt, 0);
		      /*
			gimple_cond_set_rhs( stmt, 
			TYPE_MAX_VALUE( pointer_sized_int_node));
		      */
		      // TYPE_MAX_VALUE ( TREE_TYPE ( fail_val)
		      gimple_stmt_iterator gsi = gsi_for_stmt( stmt);
		      ReorgType_t *ri = contains_a_reorgtype( stmt, info);
		      //tree null_val = 
		      //  make_temp_ssa_name ( ri->pointer_rep, NULL, "if_cond_null");
		      gcond *cond_stmt = as_a <gcond *> (stmt);
		      
		      //tree max = TYPE_MAX_VALUE ( TREE_TYPE ( ri->pointer_rep));
		      tree max = TYPE_MAX_VALUE ( ri->pointer_rep);
		      
		      //DEBUG_L("max: ");
		      //DEBUG_F(print_generic_expr, stderr, max, (dump_flags_t)0);
		      //DEBUG("\n");
		      
		      gimple_cond_set_rhs( cond_stmt, max);
		      
		      //DEBUG_L("after: ");
		      //DEBUG_F( print_gimple_stmt, stderr, stmt, 0);
		    }
		    break;
		  case ReorgT_IfPtrEQ:
		  case ReorgT_IfPtrNE:
		  case ReorgT_IfPtrLT:
		  case ReorgT_IfPtrGT:
		  case ReorgT_IfPtrLE:
		  case ReorgT_IfPtrGE:
		    //DEBUG_L("ReorgT_IfPtr*\n");
		    // Not needed for single pool. TBD test this
		    break;
		  case ReorgT_PtrPlusInt:   // "a = b + i"
		    {
		      //DEBUG_L("ReorgT_PtrPlusInt: ");
		      //DEBUG_F( print_gimple_stmt, stderr, stmt, 0);
		      // Needed for hellowotrld
		      
		      // Does the type of stmt need to be adjusted? I assume so.
		      // The ReorgType contains the type of the pointer
		      // if so that should probably be used. Note, the variables
		      // should all be of the correct type (but maybe that's
		      // not reflected here. Punting and assigning the types to
		      // the type of pointer_sized_int_node is probably not correct
		      // even though that's the representation.

		      tree PPI_orig_lhs = gimple_assign_lhs ( stmt);

		      //tree offset_type = TREE_TYPE ( TYPE_SIZE_UNIT (ri->gcc_type)); // not needed
		      tree type = ri->pointer_rep;

		      tree str_siz =
			build_int_cst ( type, int_cst_value ( TYPE_SIZE_UNIT (ri->gcc_type)));
		      
		      tree rhs1 = gimple_assign_rhs1( stmt);
		      tree rhs2 = gimple_assign_rhs2( stmt);

		      gcc_assert ( type);
		      tree PPI_rhs1_cast = make_temp_ssa_name( type, NULL, "PPI_rhs1_cast");
		      gimple *gPPI_rhs1_cast = gimple_build_assign ( PPI_rhs1_cast, CONVERT_EXPR, rhs1);
		      SSA_NAME_DEF_STMT ( PPI_rhs1_cast) = gPPI_rhs1_cast;
		      
		      tree PPI_rhs2_cast = make_temp_ssa_name( type, NULL, "PPI_rhs2_cast");
		      gimple *gPPI_rhs2_cast = gimple_build_assign ( PPI_rhs2_cast, CONVERT_EXPR, rhs2);
		      SSA_NAME_DEF_STMT ( PPI_rhs2_cast) = gPPI_rhs2_cast;

		      tree PPI_adj =  make_temp_ssa_name( type, NULL, "PtrPlusInt_Adj");
		      gimple *gPPI_adj =
			gimple_build_assign ( PPI_adj, TRUNC_DIV_EXPR, PPI_rhs2_cast, str_siz);
		      SSA_NAME_DEF_STMT ( PPI_adj) = gPPI_adj;

		      tree ptrplusint =  make_temp_ssa_name( type, NULL, "PtrPlusInt");
		      gimple *gPPI =
		      	gimple_build_assign ( ptrplusint, PLUS_EXPR, PPI_rhs1_cast, PPI_adj);
		      SSA_NAME_DEF_STMT ( ptrplusint) = gPPI;
		      
		      gimple *gPPI_cast = 
			gimple_build_assign ( PPI_orig_lhs, CONVERT_EXPR, ptrplusint);
		      SSA_NAME_DEF_STMT ( PPI_orig_lhs) = gPPI_cast;
		      
		      //gimple_set_op( stmt, 2, tmp);
		      gimple_stmt_iterator gsi = gsi_for_stmt( stmt);
		      gsi_insert_before( &gsi, gPPI_rhs1_cast, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPPI_rhs2_cast, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPPI_adj, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPPI, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPPI_cast, GSI_SAME_STMT);

		      gsi_remove ( &gsi, true);

		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPPI_rhs2_cast, 0);
		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPPI_adj, 0);
		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPPI, 0);
		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPPI_cast, 0);
		    }
		    break;
		  case ReorgT_Ptr2Zero:   //  "a = 0"
		    DEBUG_L("ReorgT_Ptr2Zero\n");
		    /*
		    // TBD
		    // Note, this is way too simple... just saying.
		    gimple_set_op( stmt, 1, 
		    TYPE_MIN_VALUE( pointer_sized_int_node));
		    */
		    break;
		  case ReorgT_PtrDiff:    //  "i = a - b"
		    {
		      DEBUG_L("ReorgT_PtrDiff\n");
		      // We basically need to modify the gimple code
		      // but that also means adding converts.
		      tree type = ri->pointer_rep;
		      tree str_siz =
			build_int_cst ( type, int_cst_value ( TYPE_SIZE_UNIT (ri->gcc_type)));
		      tree rhs1 = gimple_assign_rhs1( stmt);
		      tree rhs2 = gimple_assign_rhs2( stmt);
		      tree PD_orig_lhs = gimple_assign_lhs ( stmt);
		      
		      tree PD_rhs1_cast = make_temp_ssa_name( type, NULL, "PD_rhs1_cast");
		      gimple *gPD_rhs1_cast = gimple_build_assign ( PD_rhs1_cast, CONVERT_EXPR, rhs1);
		      SSA_NAME_DEF_STMT ( PD_rhs1_cast) = gPD_rhs1_cast;
		      
		      tree PD_rhs2_cast = make_temp_ssa_name( type, NULL, "PD_rhs2_cast");
		      gimple *gPD_rhs2_cast = gimple_build_assign ( PD_rhs2_cast, CONVERT_EXPR, rhs2);
		      SSA_NAME_DEF_STMT ( PD_rhs2_cast) = gPD_rhs2_cast;
		      
		      tree ptrdiff =  make_temp_ssa_name( type, NULL, "PtrDiff");
		      gimple *gPD =
		      	gimple_build_assign ( ptrdiff, MINUS_EXPR, PD_rhs1_cast, PD_rhs2_cast);
		      SSA_NAME_DEF_STMT ( ptrdiff) = gPD;

		      tree PD_adjust =  make_temp_ssa_name( type, NULL, "PD_adjust");
		      gimple *gPD_adjust =
			gimple_build_assign ( PD_adjust, MULT_EXPR, ptrdiff, str_siz);

		      gimple *gPD_cast = 
			gimple_build_assign ( PD_orig_lhs, CONVERT_EXPR, PD_adjust);
		      SSA_NAME_DEF_STMT ( PD_orig_lhs) = gPD_cast;
		      
		      gimple_stmt_iterator gsi = gsi_for_stmt( stmt);
		      gsi_insert_before( &gsi, gPD_rhs1_cast, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPD_rhs2_cast, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPD, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPD_adjust, GSI_SAME_STMT);
		      gsi_insert_before( &gsi, gPD_cast, GSI_SAME_STMT);

		      gsi_remove ( &gsi, true);

		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPD_rhs1_cast, 0);
		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPD_rhs2_cast, 0);
		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPD, 0);
		      //DEBUG_L("");
		      //DEBUG_F( print_gimple_stmt, stderr, gPD_cast, 0);
		  }
		    break;
		  case ReorgT_Adr2Ptr:    //  "a = &x[i]"
		    DEBUG_L("ReorgT_Adr2Ptr\n");
		    // TBD
		    /*
	      tree *add_stmt = 
	        gimple_build_assign( 
        	  gimple_assign_lhs( stmt);, 
		  PLUS_EXPR, 
		  gimple_assign_rhs1( stmt), 
		  gimple_assign_rhs2( stmt), 
		  NULL_TREE, NULL_TREE);
	      gimple_stmt_iterator gsi = gsi_for_stmt( stmt);
	      gsi_insert_before( gsi, add_stmt, GSI_SAME_STMT);
	      // delete stmt
	      gsi_remove( gsi, true);
		    */
		    break;
		  case ReorgT_PtrNull:     //  "x = a == 0"
		  case ReorgT_PtrNotNull:  //  "x = a != 0"
		    DEBUG_L("ReorgT_Ptr(Not)Null\n");	\
		    // TBD
		    /*
	      gimple_set_op( stmt, 2, 
	      		     TYPE_MIN_VALUE( pointer_sized_int_node));
		    */
		    break;
		  case ReorgT_PtrEQ:       //  "i = a == b"
		  case ReorgT_PtrNE:       //  "i = a != b"
		  case ReorgT_PtrLT:       //  "i = a < b"
		  case ReorgT_PtrLE:       //  "i = a <= b"
		  case ReorgT_PtrGT:       //  "i = a > b"
		  case ReorgT_PtrGE:       //  "i = a >= b"
		    DEBUG_L("ReorgT_Ptr*\n");
		    // Not needed for single pool. TBD test this
		    break;
		  case ReorgT_Malloc:
		    {
		      DEBUG_L("Transform ReorgT_Malloc\n");
		      //INDENT(2);

		      // We need to use the user malloc function
		      // declaration rather than the builtin!!!
		      tree fndecl_malloc = gimple_call_fndecl ( stmt);
		      
		      // We need to synthesize the free function
		      //
		      tree param_type_list = NULL;
		      tree void_pointer_type_node = build_pointer_type ( void_type_node);
		      param_type_list =
			tree_cons ( NULL_TREE, void_pointer_type_node, param_type_list);
		      //DEBUG_L("param_type_list: ");
		      //DEBUG_F(print_generic_expr, stderr, param_type_list, (dump_flags_t)0);
		      //DEBUG("\n");
		      #if !USE_BUILT_IN_FREE
		      tree free_return_type = void_type_node;
		      #endif
		      //DEBUG_L("free_return_type: ");
		      //DEBUG_F(print_generic_expr, stderr, free_return_type, (dump_flags_t)0);
		      //DEBUG("\n")
		      #if USE_BUILT_IN_FREE
		      tree fndecl_free = builtin_decl_implicit ( BUILT_IN_FREE);
		      #else
		      tree fntype = build_function_type ( free_return_type, param_type_list);
		      tree fnname = get_identifier ( "free");
		      tree fndecl_free = build_decl ( input_location, FUNCTION_DECL, fnname, fntype);
		      #endif
		      // Note, add it to the call graph at each call site
		      
		      // Note, unlike other simpler transformations,
		      // this must build new basic blocks to add new
		      // gimple to and use a phi for the final result.
		      // See appendix on malloc transformation for
		      // each comment starting with "FROM."
		      ReorgType_t *ri = contains_a_reorgtype( stmt, info);
		      // FROM len = val/size
		      tree arg = gimple_call_arg( stmt, 0);
		      // TBD: len is new SSA
		      tree val = gimple_call_lhs( stmt);
		      //DEBUG_L("val is: ");
		      //DEBUG_F( print_generic_expr, stderr, val, (dump_flags_t)-1);
		      //DEBUG(", tree code type: %s\n", code_str(TREE_CODE(TREE_TYPE(val))));
		      //gcc_assert( TREE_CODE( TREE_TYPE(val)) == INDIRECT_REF);
		      gcc_assert( TREE_CODE( TREE_TYPE(val)) == POINTER_TYPE);
		      tree size = TYPE_SIZE_UNIT( TREE_TYPE( TREE_TYPE( val)));
		      // FROM len = val/size (insert before stmt) <== maybe arg/size
		      //tree len = make_temp_ssa_name( sizetype, NULL, "fail_val");
		      // The above segfaulted ??? note, it's not an idiom seen in gcc
		      tree int_ptrsize_type = signed_type_for ( ptr_type_node);
		      //DEBUG_L("int_ptrsize_type = %p\n", TREE_TYPE ( size));
		      gcc_assert ( TREE_TYPE ( size));
		      tree len = make_temp_ssa_name ( TREE_TYPE ( size), NULL, "malloc_len");
		      gimple_stmt_iterator gsi = gsi_for_stmt( stmt);
		      //gimple *glen = 
		      //  gimple_build_assign ( len, TRUNC_DIV_EXPR, val, size);

		      // Cast arg to compatible type
		      gcc_assert( TREE_TYPE ( size));
		      TREE_TYPE (arg) = TREE_TYPE ( size);
		      tree cast_arg =
			    make_temp_ssa_name( TREE_TYPE ( size), NULL, "cast_arg");
		      gimple *gcast_arg = gimple_build_assign ( cast_arg, CONVERT_EXPR, arg);
		      SSA_NAME_DEF_STMT ( cast_arg) = gcast_arg;
		      gsi_insert_before( &gsi, gcast_arg, GSI_SAME_STMT);
			
		      gimple *glen = 
			gimple_build_assign ( len, TRUNC_DIV_EXPR, cast_arg, size);
		      SSA_NAME_DEF_STMT ( len) = glen;
		      
		      gsi_insert_before( &gsi, glen, GSI_SAME_STMT);
		      // Note in other places in this doc this would
		      // be "insert glen before stmt" instead of this but
		      // here we need to create new basic blocks.
		      edge new_edge = split_block ( bb, stmt);
		      // FROM before_bb = edge->src // same as this bb
		      basic_block before_bb = new_edge->src; // 
		      basic_block after_bb = new_edge->dest;
		      remove_edge ( new_edge);
		      basic_block prev_bb = before_bb;
		      
		      // FROM failure_bb = create_empty_block(prev_bb)
		      basic_block failure_bb = make_bb ( "failure_bb", prev_bb);
		      // I need to set the count to zero and there doesn't
		      // seem to be direct way of doing this...
		      failure_bb->count = prev_bb->count - prev_bb->count;
		      
		      // set edge probability and flags
		      edge fail_to_after_e = make_edge ( failure_bb,
						      after_bb, EDGE_FALLTHRU);
		      fail_to_after_e->probability = profile_probability::very_unlikely ();
		      fail_to_after_e->count () = failure_bb->count;

		      // Note, this should remove this call from the call graph
		      cgraph_update_edges_for_call_stmt ( stmt, gimple_call_fndecl ( stmt), NULL);
		      // Now it's safe to remove it!
		      gsi_remove ( &gsi, true);
		      
		      tree field;
		      tree reorg_type = ri->gcc_type; // is this useful here?
		      tree reorg_pointer_type = ri->pointer_rep;
		      //tree base = ri->reorg_ver_type; //nopers
		      tree base = ri->instance_interleave.base;

		      // ??? We are faking the malloc so the following seemed dubious
		      //tree malloc_return_type = TREE_TYPE ( arg);
		      //tree fail_val = 
		      //	make_temp_ssa_name ( malloc_return_type, NULL, "malloc_fail_val");
		      gcc_assert ( reorg_pointer_type);
		      tree fail_val = 
		      	make_temp_ssa_name ( reorg_pointer_type, NULL, "malloc_fail_val");
		      
		      // loop setup trickery for gimple idioms
		      //
		      // FROM prev_order = failure_bb
		      basic_block prev_order = failure_bb;
		      // FROM prev_bb = before_bb
		      prev_bb = before_bb;
		      int edge_flags = EDGE_FALLTHRU;
		      
		      // Generate all the real allocation code
		      //
		      // Note, I think there are ramifications of built in malloc (and free)
		      // so I'm going try and use the malloc in the call transformed!!!
		      // Actually, I ended up using the built in free and it was
		      // the way to go.
		      //
		      
		      //tree fndecl_malloc = builtin_decl_explicit( BUILT_IN_MALLOC);
		      
		      // This, after the following loop, will hold the start of the
		      // field related code.
		      tree new_ok_field_L;
		      
		      // FROM (for fields) {
		      bool first = true;
		      for( field = TYPE_FIELDS( reorg_type); 
			   field; 
			   field = DECL_CHAIN( field))
			{
			  basic_block new_bb = make_bb ( "new_bb", prev_order);
			  new_bb->count = prev_order->count;
			  // Nope! Don't do this.
			  //set_immediate_dominator ( CDI_DOMINATORS, new_bb, prev_bb);
			  //if ( first )
			  //  {
			  //    first = false;
			  //    set_immediate_dominator ( CDI_DOMINATORS, failure_bb, new_bb);
			  //    set_immediate_dominator ( CDI_DOMINATORS, after_bb, new_bb);
			  //  }
			    
			  tree base_field =
			      find_coresponding_field ( base, field);

			  //DEBUG_L("base_field: %p\n", base_field);
			  //DEBUG_A("  : ");
			  //DEBUG_F(print_generic_expr, stderr, base_field, (dump_flags_t)0);
			  //DEBUG("\n");

			  tree base_field_type = TREE_TYPE( base_field);
			  //DEBUG_L("base_field_type: %p\n", base_field_type);
			  //DEBUG_A("  : ");
			  //DEBUG_F(print_generic_expr, stderr, base_field_type, (dump_flags_t)0);
			  //DEBUG("\n");
			  
			  gimple_stmt_iterator gsi = gsi_start_bb ( new_bb);
			  // Note, switching the order of edge creation and
			  // setting dominator seems to make no difference
			  
			  // set edge probability and flags
			  
			  // edge_flags depends on whether or not the predecessor
			  // block was created in this loop.
			  edge ok_edge = make_edge ( prev_bb, new_bb, edge_flags);
			  edge_flags = EDGE_TRUE_VALUE;

			  ok_edge->probability = profile_probability::very_likely ();
			  ok_edge->count () = prev_bb->count;
			  add_bb_to_loop ( new_bb, before_bb->loop_father);
			  
			  // Don't mess with the dominators.
			  //set_immediate_dominator ( CDI_DOMINATORS, new_bb, prev_bb);

			  // create edge and set edge probability and flags
			  edge fail_edge = make_edge ( new_bb, failure_bb, EDGE_FALSE_VALUE);
			  fail_edge->probability = profile_probability::very_unlikely ();
			  fail_edge->count () = new_bb->count - new_bb->count;

			  //tree lhs_ass =
			  //  build3( COMPONENT_REF, ptr_type_node, base, field, NULL_TREE);
			  tree lhs_ass = build3( COMPONENT_REF,
						 base_field_type,
						 base,
						 base_field, NULL_TREE);

			  //DEBUG_L("base: %p\n", base);
			  //DEBUG_A("  base: ");
			  //DEBUG_F(print_generic_expr, stderr, base, (dump_flags_t)0);
			  //DEBUG("\n");
			  
			  //DEBUG_L("field: %p\n", field);
			  //DEBUG_A("  : ");
			  //DEBUG_F(print_generic_expr, stderr, field, (dump_flags_t)0);
			  //DEBUG("\n");

			  tree field_type = TREE_TYPE( field);
			  //DEBUG_L("field_type: %p\n", field_type);
			  //DEBUG_A("  : ");
			  //DEBUG_F(print_generic_expr, stderr, field_type, (dump_flags_t)0);
			  //DEBUG("\n");
			  
			  //DEBUG_L("lhs_ass: %p\n", lhs_ass);
			  //DEBUG_A("  lhs_ass: ");
			  //DEBUG_F(print_generic_expr, stderr, lhs_ass, (dump_flags_t)0);
			  //DEBUG("\n");

			  tree lhs_ass_type = TREE_TYPE ( lhs_ass);
			  //DEBUG_L("lhs_ass_type: %p\n", lhs_ass_type);
			  //DEBUG_A("  lhs_ass_type: ");
			  //DEBUG_F(print_generic_expr, stderr, lhs_ass_type, (dump_flags_t)0);
			  //DEBUG("\n");

			  gcc_assert ( sizetype);
			  tree mem_size = 
			    make_temp_ssa_name( sizetype, NULL, "malloc_mem_size");
			  
			  // We need field_size to be of the correct type so
			  // we type cast.
			  gcc_assert ( TREE_TYPE ( mem_size));
			  tree field_size =
			    make_temp_ssa_name( TREE_TYPE ( mem_size), NULL, "field_size");

			  // Move gprev_ok_field here
			  
			  // Move gfield_size here
			  gimple *gfield_size =
			    gimple_build_assign ( field_size,
						  CONVERT_EXPR,
						  TYPE_SIZE ( TREE_TYPE ( field)));
			  SSA_NAME_DEF_STMT ( field_size) = gfield_size;

			  // Move gsize here
			  gimple *gsize = 
			    gimple_build_assign ( mem_size,
						  MULT_EXPR,
						  field_size,
						  len);
			  SSA_NAME_DEF_STMT ( mem_size) = gsize;

			  gcc_assert ( ptr_type_node);
			  tree res = 
			    make_temp_ssa_name ( ptr_type_node, NULL, "res");

			  gcc_assert ( TREE_TYPE ( base_field));
			  tree cast_res =
			    make_temp_ssa_name ( TREE_TYPE ( base_field), NULL, "cast_res");

			  tree res_type = TREE_TYPE( res);
			  //DEBUG_L("res_type: %p\n", res_type);
			  //DEBUG_A("  : ");
			  //DEBUG_F(print_generic_expr, stderr, res_type, (dump_flags_t)0);
			  //DEBUG("\n");

			  // Move malloc_call here
			  gcall *malloc_call = gimple_build_call( fndecl_malloc, 1, mem_size);
			  gimple_call_set_lhs( malloc_call, res);
			  SSA_NAME_DEF_STMT ( res) = malloc_call;

			  cgraph_node::get ( cfun->decl)->
			    create_edge ( cgraph_node::get_create ( fndecl_malloc),
					  malloc_call,
					  // Nah... lets do this a bit differently
					  //gimple_bb ( free_call)->count
					  new_bb->count
					  );
			  
			  gimple *gcast_res =
			    gimple_build_assign ( cast_res, CONVERT_EXPR, res);
			  SSA_NAME_DEF_STMT ( cast_res) = gcast_res;
			  
			  // Move gset_field here
			  gimple *gset_field = gimple_build_assign ( lhs_ass, cast_res);

			  // Move gcond here
			  gimple *gcond =
			    gimple_build_cond ( NE_EXPR, res, null_pointer_node,
						NULL, NULL
						);

			  // In execution order
			  gsi_insert_after ( &gsi, gfield_size, GSI_NEW_STMT);
			  gsi_insert_after ( &gsi, gsize, GSI_CONTINUE_LINKING);
			  gsi_insert_after ( &gsi, malloc_call, GSI_CONTINUE_LINKING);
			  gsi_insert_after ( &gsi, gcast_res, GSI_CONTINUE_LINKING);
			  gsi_insert_after ( &gsi, gset_field, GSI_CONTINUE_LINKING);
			  gsi_insert_after ( &gsi, gcond, GSI_CONTINUE_LINKING);
			  
			  prev_bb = new_bb;
			  prev_order = new_bb;
			}

		      // Loop cleaup fo failure code bb here. There is loop state
		      // overhead having nothing to do with the transformation
		      // that never the less must be updated.
		      add_bb_to_loop ( failure_bb, before_bb->loop_father);
		      
		      // create basic block for success
		      //
		      // FROM success_bb = create_empty_block(prev_bb_order);
		      basic_block success_bb = make_bb ( "succ_bb", prev_bb);
		      success_bb->count = prev_bb->count;
		      
		      // NOTE, it seems I shouldn't be attempting
		      // to diddle the dominator information on the fly.
		      // set_immediate_dominator ( CDI_DOMINATORS, success_bb, prev_bb);
		      
		      edge success_e = make_edge ( prev_bb, success_bb, EDGE_TRUE_VALUE );
		      edge succ_to_after_e = make_edge ( success_bb, after_bb, EDGE_FALLTHRU);
		      success_e->probability = profile_probability::very_likely ();
		      succ_to_after_e->probability = profile_probability::always ();
		      success_e->count () = prev_bb->count;
		      succ_to_after_e->count () = prev_bb->count;
		      add_bb_to_loop ( success_bb, before_bb->loop_father);
		      
		      // code in success_bb
		      //
		      gcc_assert ( reorg_pointer_type);
		      tree success_val = 
			make_temp_ssa_name( reorg_pointer_type, NULL, "malloc_success_val");

		      gsi = gsi_start_bb ( success_bb);  // used to be failure_bb

		      gimple *set_succ =
			gimple_build_assign ( success_val,
					      build_int_cst ( reorg_pointer_type, 0));
		      SSA_NAME_DEF_STMT ( success_val) = set_succ;
		      
		      gsi_insert_after( &gsi, set_succ,	GSI_NEW_STMT);

		      // FROM gsi_insert_after( &gsi, new_ok_field )
		      //gimple *gnew_ok_field = gimple_build_label ( new_ok_field_L);
		      //gsi_insert_after ( &gsi, gnew_ok_field, GSI_SAME_STMT);
		      
		      // add code to after_bb
		      //
		      // FROM gsi = gsi_start_bb( after_bb)
		      // Reuse gsi
		      //gimple_stmt_iterator gsi = gsi_start_bb( after_bb);
		      gsi = gsi_start_bb( after_bb);

		      // FROM gsi_insert_after( &gsi, "lhs = "phi(success_val, fail_val)
		      
		      // Note, BBs have a sequence of phis which create_phi_node takes care of
		      // adding this phi too.
		      gcc_assert ( TREE_TYPE ( success_val));
		      tree m_phi_val =
			make_temp_ssa_name ( TREE_TYPE ( success_val), NULL, "m_phi_val");
		      gphi *der_phi = create_phi_node( m_phi_val, after_bb);
		      add_phi_arg( der_phi, success_val, succ_to_after_e, UNKNOWN_LOCATION);
		      add_phi_arg( der_phi, fail_val, fail_to_after_e, UNKNOWN_LOCATION);

		      gimple *gm_cast_phi_val =
			gimple_build_assign ( val, CONVERT_EXPR, m_phi_val);
		      SSA_NAME_DEF_STMT ( val) = gm_cast_phi_val;

		      //gsi_insert_after( &gsi, gm_cast_phi_val, GSI_NEW_STMT);
		      // TBD What does GSI_NEW_STMT do if  the block isn't emply?
		      gsi_insert_before( &gsi, gm_cast_phi_val, GSI_NEW_STMT);

		      //// FROM gsi_insert_after( &gsi, after_label)
		      //gimple *gafter_label = gimple_build_label( after_label_L);
		      //gsi_insert_after( &gsi, gafter_label, GSI_SAME_STMT);


		      // failure_bb code here

		      //
		      // FROM fail_val is new SSA
		      //tree return_type = TREE_TYPE ( arg);
		      //tree fail_val = 
		      //  make_temp_ssa_name ( return_type, NULL, "fail_val");
		      // FROM gsi = gsi_start_bb ( failure_bb)
		      gsi = gsi_start_bb ( failure_bb);

		      // FROM gsi_insert_after( &gsi, "fail_val = minint")
		      gimple *gretnull =
			gimple_build_assign ( fail_val, CONVERT_EXPR,
					      TYPE_MAX_VALUE ( TREE_TYPE ( fail_val)));
		      SSA_NAME_DEF_STMT ( fail_val) = gretnull;
		      
		      gsi_insert_after( &gsi, gretnull, GSI_NEW_STMT);
		      
		      for( field = TYPE_FIELDS( reorg_type); 
			   field; 
			   field = DECL_CHAIN( field)) {

			tree base_field =
			      find_coresponding_field ( base, field);
			tree base_field_type = TREE_TYPE( base_field);
			
			gcc_assert ( base_field_type);
			tree m_to_free = 
			  make_temp_ssa_name( base_field_type, NULL, "malloc_to_free");
			
			tree rhs_ass = build3( COMPONENT_REF,
			    base_field_type,
			    base,
			    base_field, NULL_TREE);
			
			gimple *gaddr2free = gimple_build_assign( m_to_free, rhs_ass);
			SSA_NAME_DEF_STMT ( m_to_free) = gaddr2free;
			
			gsi_insert_after( &gsi, gaddr2free, GSI_CONTINUE_LINKING);

			gcc_assert ( ptr_type_node);
			tree m_cast2free =
			  make_temp_ssa_name( ptr_type_node, NULL, "m_cast2free");
			
			gimple *gm_cast2free =
			  gimple_build_assign( m_cast2free, CONVERT_EXPR, m_to_free);
			SSA_NAME_DEF_STMT ( m_cast2free) = gm_cast2free;

			gsi_insert_after( &gsi, gm_cast2free, GSI_CONTINUE_LINKING);
			
			gcall *free_call = gimple_build_call( fndecl_free, 1, m_cast2free);
			gsi_insert_after( &gsi, free_call, GSI_CONTINUE_LINKING);
			
			cgraph_node::get ( cfun->decl)->
			create_edge ( cgraph_node::get_create ( fndecl_free),
				      free_call,
				      failure_bb->count
				    );

			tree lhs_ass = build3( COMPONENT_REF,
					       base_field_type,
					       base,
					       base_field, NULL_TREE);
			
			gimple *gzero = gimple_build_assign( lhs_ass, null_pointer_node);
			gsi_insert_after( &gsi, gzero, GSI_CONTINUE_LINKING);
		      }
		      
		      //// FROM gsi_insert_after( &gsi, bad_field )

		      //DEBUG_L("End of malloc:\n");
		      //DEBUG_F( print_program, PRINT_FORMAT, stderr, 4);
		    }
		    //INDENT(-2);
		    break;
		  case ReorgT_Calloc:
		    // TBD
		    DEBUG_L("ReorgT_Calloc\n");
		    /*
		    // This used to be almost a clone of the old version of
		    // the malloc code above and needs to transformed just like
		    // what was done above to malloc.
		    tree arg = gimple_call_arg( stmt, 0);
		    len is new SSA
		      tree val = gimple_call_lhs( stmt);
		    gcc_assert( TREE_CODE( TREE_TYPE(val)) == INDIRECT_REF);
		    tree size = TYPE_SIZE_UNIT( TREE_TYPE( TREE_TYPE( val)));
		    gimple *glen = 
		      gimple_build_assign( 
					  len, 
					  TRUNC_DIV_EXPR, 
					  val, 
					  size,
					  NULL_TREE, NULL_TREE);
		    //insert glen before stmt
		    gimple_stmt_iterator stmt_gsi = gsi_for_stmt ( stmt);
		    gsi_link_before( stmt_gsi, glen, GSI_SAME_STMT);
		    tree lfial = create_artificial_label( UNKNOWN_LOCATION);
		    gimple *gfail = gimple_build_label( lfail);
		    tree lnotfial = create_artificial_label( UNKNOWN_LOCATION);
		    gimple *gnotfail = gimple_build_label( lnotfail);
		    tree base = ri->reorg_ver_type;
		    for (each element of base) // TBD <==
		      {
			// call malloc
			tree lok = create_artificial_label( UNKNOWN_LOCATION);
			gimple *glok = gimple_build_label( lok);
			tree *fndecl = builtin_decl_explicit( BUILT_IN_MALLOC);
			mem_size is new SSA
			  gimple *gsize = 
			  gimple_build_assign( 
					      mem_size, 
					      MULT_EXPR, 
					      TYPE_SIZE(element), 
					      len,
					      NULL_TREE, NULL_TREE);
			insert gsize before stmt
			  gcall *call = gimple_build_call( fndecl, 1, mem_size);
			mres is new SSA
			  gimple_call_set_lhs( call, mres)
			  insert call before stmt
			  // Set element to return value of malloc.
			  // Note, the devil is in the details here.
			  gen concat( REORG_SP_PREFIX,
				      type_name( lhs)  ).element <- mres
			  and insert before stmt
			  // gen test of return
			  gimple *gcond =
			  gimple_build_cond( EQ_EXPR, mres,
					     null_pointer_node, lfail, lok);
			insert gcond before stmt
			  insert glok before stmt
			  // call memset
			  fndecl = builtin_decl_explicit( BUILT_IN_MEMSET);
			call =
			  gimple_build_call( fndecl, 3, mres, int_node_zero, mem_size);
			insert call before stmt
			  }
	      
		    // fake return value of zero
		    gimple *gretzero =
		      gimple_build_assign( lhs,
					   build_int_cst(
							 TYPE_MIN_VALUE( TREE_TYPE(lhs)), 0));
		    insert gretzero before stmt
		      gimple *ggoto = gimple_build_goto( lnotfail);
		    insert ggoto before stmt
		      insert glab1 before stmt
		      for each element of base {
			  tree fndecl = builtin_decl_explicit( BUILT_IN_FREE);
			  gcall *call = gimple_build_call( fndecl, 1, element);
			  insert call before stmt
			    set element to null
			    }
		    // fake return value of null
		    gimple *gretnull =
		      gimple_build_assign( lhs,
					   build_int_cst(
							 TYPE_MIN_VALUE( TREE_TYPE(lhs))));
		    insert gretnull before stmt
		      insert gnotfail before stmt
		      delete stmt
		      */
		    break;
		  case ReorgT_Realloc:
		    // TBD
		    DEBUG_L("ReorgT_Realloc\n");
		    /*
		// This used to be closely related to the old version of
		// the malloc code above and needs to transformed just like
		// what was done above to malloc.
		tree arg = gimple_call_arg( stmt, 0);
		len is new SSA
		tree val = gimple_call_lhs( stmt);
		gcc_assert( TREE_CODE( TREE_TYPE(val)) == INDIRECT_REF);
		tree size = TYPE_SIZE_UNIT( TREE_TYPE( TREE_TYPE( val)));
		gimple *glen = 
	        gimple_build_assign( 
		  len, 
		  TRUNC_DIV_EXPR, 
		  val, 
		  size,
		  NULL_TREE, NULL_TREE);
		insert glen before stmt
		tree lfial = create_artificial_label( UNKNOWN_LOCATION);
		gimple *gfail = gimple_build_label( lfail);
		tree lnotfial = create_artificial_label( UNKNOWN_LOCATION);
		gimple *gnotfail = gimple_build_label( lnotfail);
		for each field of base {
	      	  // call malloc
		  tree lok = create_artificial_label( UNKNOWN_LOCATION);
		  gimple *gok = gimple_build_label( lok);
		  tree fndecl = builtin_decl_explicit( BUILT_IN_REALLOC);
		  // but first compute how much to malloc
		  mem_size, var, ptr are new SSA
		  gimple *gsize = 
	          gimple_build_assign( 
        	    mem_size, 
		    MULT_EXPR, 
		    TYPE_SIZE(field), 
		    len,
		    NULL_TREE, NULL_TREE);
		  insert gsize before stmt
		  generate ptr = base.field & insert before stmt
		  gcall *call
		    = gimple_build_call( fndecl, 3, ptr,
		    		         len, TYPE_SIZE( field));
		  gimple_call_set_lhs( call, var);
		  insert call before stmt
		  // gen test of return
		  gimple *gcond =
		  gimple_build_cond( EQ_EXPR, var,
		  null_pointer_node, lfail, lok);
		  insert gcond before stmt
		  insert gok before stmt
		  generate base.field = var & insert before stmt
		}
		// fake return value of starting address (an index of zero)
		gimple *gretzero =
	          gimple_build_assign( lhs, //
		    build_int_cst(
		    TYPE_MIN_VALUE( TREE_TYPE(lhs)), 0));
		insert gretzero before stmt
		gimple *ggoto = gimple_build_goto( lnotfail);
		insert ggoto before stmt
		insert glab1 before stmt
		for each element of base {
	      	tree fndecl = builtin_decl_explicit( BUILT_IN_FREE);
	        gcall *call = gimple_build_call( fndecl, 1, element);
		insert call before stmt
		set element to null
	      }
	      // fake return value of null (minimum value under this scheme)
	      gimple *gretnull =
	        gimple_build_assign( lhs,
				     build_int_cst(
				       TYPE_MIN_VALUE( TREE_TYPE(lhs))));
	      insert gretnull before stmt
	      insert gnotfail before stmt
	      delete stmt
		    */
		    break;
		  case ReorgT_Free:
		    // TBD
		    DEBUG_L("ReorgT_Free\n");
		    // We won't free the base because it a global.
		    /*
		  for each element of base {
		    tree fndecl = builtin_decl_explicit( BUILT_IN_FREE);
		    gcall *call = gimple_build_call( fndecl, 1, element);
		    insert call before stmt
		  }
		  delete stmt
		    */
		    break;
		  case ReorgT_UserFunc:
		    {
		      DEBUG_L("ReorgT_UserFunc: ");
		      DEBUG_F( print_gimple_stmt, stderr, stmt, 0);
		      // Needed for helloworld.
		      // The type must be adjusted, but not here.

		      // Note, what is done is there is a case (for
		      // GIMPLE_CALL) in the mini-pass to adjust a
		      // call's "dangling" SSA temp.  I mean dangling
		      // in the sense that there are pointers to a
		      // reorg type on the left hand side of
		      // statements and they haven't been modified to
		      // use the correct reorg pointer
		      // represenatation, even though the right hand
		      // side has been.
		    }
		    break;
		  case ReorgT_Convert:
		    // Ignore type casting because another
		    // mini-pass sweeps up any ugly dangling types.
		    // TBD test this
		    DEBUG_L("ReorgT_Convert\n");
		    break;
		  case ReorgT_Return:
		    // TBD This case probably is unnecessary.
		    DEBUG_L("ReorgT_Return\n");
		    break;
		  default:
		    internal_error( "Invalid transformation");
		  }
	      }
	  }
	// Iterate over the PHIs and for any PHI that is a reorgtype,
	// transform any constant zero into it's new repersentation.
	// OR MAYBE... use FOR_EACH_PHI_ARG for the iterator...
	
	//DEBUG_L("Phis with constant operands:\n");
	//INDENT(4);
	gphi_iterator pi;
	for ( pi = gsi_start_phis (bb); !gsi_end_p (pi); gsi_next (&pi))
	  {
	    //gimple_stmt_iterator gsi = as_a <gimple_stmt_iterator> pi;
	    gphi *phi = pi.phi ();
	    gimple *stmt = static_cast <gimple *> (phi);
	    
	    DEBUG_A("phi: ");
	    DEBUG_F( print_gimple_stmt, stderr, stmt, 0);

	    ReorgType_t *ri = contains_a_reorgtype( stmt, info);
	    if ( ri != NULL && number_of_levels ( TREE_TYPE ( PHI_RESULT ( stmt))) == 1 )
	      {
		for (int i = 0; i < gimple_phi_num_args (phi); i++)
		  {
		    tree *arg = gimple_phi_arg_def_ptr (phi, i);
		    //DEBUG_A("arg[%d] = ",i);
		    //DEBUG_F(flexible_print, stderr, *arg, 1, (dump_flags_t)0);
		    bool int_cst = TREE_CODE ( *arg) == INTEGER_CST;
		    //DEBUG_A("is %sinteger constant\n", int_cst ? "" : "not ");
		    if ( int_cst && integer_zerop ( *arg) )
		      {
			*arg = TYPE_MAX_VALUE ( ri->pointer_rep);
			//DEBUG_L("arg after = ");
			//DEBUG_F(flexible_print, stderr, *arg, 1, (dump_flags_t)0);
		      }
		  }
	      }
	  }
	//INDENT(-4);
      }
    pop_cfun ();
  }

  //DEBUG_L("after bulk of transformations\n");

  //DEBUG_F( print_program, info->reorg_dump_file, PRINT_FORMAT, 4, info);
  
  //DEBUG ("INTERNALS PRINT\n");
  //DEBUG_F (apply_to_all_gimple, print_internals, true, (void *)info);

  // A mini-pass to fixup dangling SSA temps.
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)
    {
      struct function *func = DECL_STRUCT_FUNCTION ( node->decl);
      push_cfun ( func);

      std::vector <tree> ssa_to_delete;

      //DEBUG_L("Mini-Pass on Function %s:\n", lang_hooks.decl_printable_name ( func->decl, 2));
      
      //DEBUG_L("\n");
      //DEBUG_F( wolf_fence, info);

      // We need a map of old ssa_name to new ssa_name. Not currently used.
      std::map <tree,tree> ssa_map;

      // Walk function decl.
      // Modify declaractions modifies the formal parameter types.
      // TBD That needs to be moved here
      // Never the less we need to associate new default defs with them.
      // At the end of find_decls_and_types adds parameter decls.
      // We'll use get_or_create_ssa_default_def on the ssa name and
      // create a new decl for the decls that are pointers to
      // reorg types.
      //
      // Then walk the gimple looking for dangling types. When we find
      // one on the right hand side check to see if it's on the
      // map. If not we create a ssa name and add it to the map.  We
      // subsitute it into operand.  For the left hand side we also
      // set the statement definition.

      // Default defs case first
      
      // For parameters

      // Note, we don't do anything unless it's necessary and it's
      // not necessay if it's been done before. Hence the possibility
      // of many functions mapping onto one declaration (which I
      // even doubt is possible in thi case) can't be a problem.
      
      //DEBUG_L("Dangling Types for Function Params (default defs).\n");
      //INDENT(4);
      tree parm;
      for ( parm = DECL_ARGUMENTS ( func->decl);
	    parm;
	    parm = DECL_CHAIN ( parm) )
	{
	  //DEBUG_A("param: ");
	  //DEBUG_F( print_generic_decl, stderr, parm, (dump_flags_t)0);
	  //DEBUG("\n");
	  //INDENT(2);
	  tree old_default_def = ssa_default_def ( func, parm);
	  //DEBUG_A("old_default_def: ");
	  //DEBUG_F( print_generic_expr, stderr, old_default_def, (dump_flags_t)0);
	  //DEBUG("\n");
	  tree new_default_def;

	  // Modify prameter and do the default def stuff
	  
	  // Need to create a new decl rather than modify the existing
	  // one.
	  
	  if ( modify_decl_core ( &parm, info) )
	    {
	      //DEBUG_A("double check new param: ");
	      //DEBUG_F( print_generic_decl, stderr, parm, (dump_flags_t)0);
	      //DEBUG("\n");
	      
	      // New default def here

	      // If the variable of the decl is initialized
	      // then it shouldn't be associated with a default def.
	      if ( old_default_def )
		{
		  // We must delete the old one or tranversing ssa names of
		  // the function will stumple across an SSA name associated
		  // with nothing, causing grief.
		  //
		  // We do it at the bottom of this code using a
		  // something I had to write (there was no existing
		  // mechanism.)

		  // Create new default def here.
		  // NOTE parm looks correct here but type of new_default_def
		  // is that of old_default_def so this might just look up
		  // old_default_def!
		  //new_default_def = get_or_create_ssa_default_def ( func, parm);
		  // ??? try this (in conjunction with the above) to fix things... NOPE
		  //set_ssa_default_def ( func, parm, new_default_def);
		  // ??? these instead
		  new_default_def = make_ssa_name_fn ( func, parm, gimple_build_nop ());
		  set_ssa_default_def ( func, parm, new_default_def);

		  //DEBUG_A("new_default_def: ");
		  //DEBUG_F(print_generic_expr, stderr, new_default_def, (dump_flags_t)0);
		  //DEBUG(", TYPE: ");
		  //DEBUG_F(print_generic_expr, stderr, TREE_TYPE(new_default_def), (dump_flags_t)0);
		  //DEBUG("\n");

		  // TBD REMOVE DUPLICATE!
		  // Replace old one (not really totally hence the
		  // remove_default_def below.)
		  set_ssa_default_def ( func, parm, new_default_def);

		  imm_use_iterator iter;
		  gimple *stmt;
		  use_operand_p use;
		  // Modify stmts using old default def to use
		  // new default def
		  FOR_EACH_IMM_USE_STMT ( stmt, iter, old_default_def)  // <== use other form??? Not
		    {
		      //DEBUG_A("before: ");
		      //DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
		      use_operand_p use_p;
		      ssa_op_iter ssa_iter;
		      // The F_E_S_U_O macro was blowing up on a phi
		      //FOR_EACH_SSA_USE_OPERAND( use_p, stmt, ssa_iter, SSA_OP_USE )
		      FOR_EACH_PHI_OR_STMT_USE ( use_p, stmt, ssa_iter, SSA_OP_USE )
			{
			  if ( use_p == NULL ) continue;
			  tree use = USE_FROM_PTR (use_p);
			  //DEBUG_A("use to replace: ");
			  //DEBUG_F( print_generic_expr, stderr, use, (dump_flags_t)0);
			  //DEBUG("\n");
			  if (use == old_default_def)
			    SET_USE ( use_p, new_default_def);
			}
		      //DEBUG_A("after: ");
		      //DEBUG_F ( print_gimple_stmt, stderr, stmt, 0);
		    }
		  // Get rid of the old default def because it confuses
		  //
		  //remove_default_def ( old_default_def, func);
		  ssa_to_delete.push_back ( old_default_def);
		  release_ssa_name_fn ( func, old_default_def);
		}
	    }
	  //INDENT(-2);
	}
      //INDENT(-4);
      
      //DEBUG_L("Dangling Types for Function Local (default defs).\n");
      //INDENT(4);
      //DEBUG_L("\n");
      //DEBUG_F( wolf_fence, info);
      
      // For locals
      //
      // Note, the code below looks half baked and might simply not
      // encountered anything that breaks it (see the code for the
      // parameters above how it might need to look.)
      //
      
      unsigned i;
      tree decl;
      FOR_EACH_LOCAL_DECL ( func, i, decl)
	{
	  //DEBUG_A("local: ");
	  //DEBUG_F( print_generic_decl, stderr, decl, (dump_flags_t)0);
	  //DEBUG("\n");
	  tree old_default_def = ssa_default_def ( func, decl);
	  tree new_default_def;
	  
	  // Modify prameter and do the default def stuff
	  
	  // Again we'll need to create a new decl rather than modify
	  // the existing one.
	  
	  if ( modify_decl_core ( &decl, info) )
	    {
	      // New default def here

	      // If the variable of the decl is initialized
	      // then it shouldn't be associated with a default def.
	      if ( old_default_def )
		{
		  // TBD Do we delete the old one and if so,
		  // do we do it here and how do we do it?

		  // Create new default def here.
		  new_default_def = get_or_create_ssa_default_def ( func, decl);

		  imm_use_iterator iter;
		  gimple *stmt;
		  use_operand_p use;
		  // Modify stmts using old default def to use
		  // new default def
		  FOR_EACH_IMM_USE_STMT ( stmt, iter, old_default_def)
		    {
		      FOR_EACH_IMM_USE_ON_STMT ( use, iter)
			{
			  SET_USE ( use, new_default_def);
			}
		      // Possibly do this???
		      //update_stmt ( stmt);
		    }
		}
	    }
	}
      //INDENT(-4);      
      
      // Normal ssa name case
      //DEBUG_L("Dangling Types for Normal SSA Names:\n");
      //DEBUG_L("\n");
      //DEBUG_F( wolf_fence, info);
      
      //INDENT(4);
      // We use len instead of using func->length() in the for loop test
      // because new ssa names are created in the loop body and we
      // shouldn't process them.
      unsigned int len = SSANAMES ( func)->length ();
      //DEBUG_L("len = %d\n",len);
      for ( unsigned int i = 0; i < len; i++)
	{
	  //DEBUG_L("SSANAMES(func)[%d]\n",i);

	  tree ssa_name = (*SSANAMES ( func))[i];

	  if( ssa_name == NULL )
	    {
	      //DEBUG_L("Skip, ssa_name == NULL\n");
	      continue;
	    }

	  bool a_default_def = SSA_NAME_IS_DEFAULT_DEF ( ssa_name);
	  gimple *defining_stmt = SSA_NAME_DEF_STMT ( ssa_name);;
	  bool no_defining_stmt = defining_stmt == NULL;
	  bool defined_by_nop = defining_stmt && gimple_code ( defining_stmt) == GIMPLE_NOP;
	  tree type = TREE_TYPE ( ssa_name);
	  tree bottom_type = base_type_of ( type);
	  ReorgType_t *ri = get_reorgtype_info ( bottom_type, info);
	  //DEBUG_L("ssa_name = ");
	  //DEBUG_F(print_generic_expr, stderr, ssa_name, (dump_flags_t)0);
	  //DEBUG(" %s", a_default_def ? "is default_def" : "");
	  //DEBUG(" %s", no_defining_stmt ? "has no defining stmt" : "");
	  //DEBUG(" %s", defined_by_nop ? "defined by a nop" : "");
	  //DEBUG(", type = ");
	  //DEBUG_F(print_generic_expr, stderr, type, (dump_flags_t)0);
	  //DEBUG(", bottom_type = ");
	  //DEBUG_F(print_generic_expr, stderr, bottom_type, (dump_flags_t)0);
	  //DEBUG(", ri = %p\n",ri);

	  // If it's not a dangling type we don't care
	  if ( ri == NULL )
	    {
	      //DEBUG_L("Skip, ri == NULL\n");
	      continue;
	    }

	  // A default def is processed seperately
	  if ( a_default_def )
	    {
	      //DEBUG_L("Skip default_def\n");
	      continue;
	    }

	  gcc_assert ( !no_defining_stmt);
	  gcc_assert ( !defined_by_nop);

	  //DEBUG_L("Defining stmt: ");
	  //DEBUG_F ( print_gimple_stmt, stderr, defining_stmt, 0);

	  tree new_type = ri->pointer_rep;
	  tree new_ssa_name = make_temp_ssa_name( new_type, NULL, "dedangled");
	  //DEBUG_L("new_ssa_name = ");
	  //DEBUG_F(print_generic_expr, stderr, new_ssa_name, (dump_flags_t)0);
	  //DEBUG("\n");
	  #if DEBUGGING
	  for ( unsigned int j = 0; j < SSANAMES ( func)->length (); j++)
	    {
	      if ( (*SSANAMES ( func))[j] == new_ssa_name )
		{
		  DEBUG_L("new name at j = %d\n",j);
		  break;
		}
	    }
	  #endif
	  
	  gimple *use_stmt;
	  imm_use_iterator iter;
	  FOR_EACH_IMM_USE_STMT ( use_stmt, iter, ssa_name)
	    {
	      //DEBUG_L("use_stmt before: ");
	      //DEBUG_F ( print_gimple_stmt, stderr, use_stmt, 0);
	      
	      // Deal with the uses
	      use_operand_p use_p;
	      ssa_op_iter ssa_iter;
	      // The F_E_S_U_O macro was blowing up on a phi
	      //FOR_EACH_SSA_USE_OPERAND( use_p, use_stmt, ssa_iter, SSA_OP_USE )
	      FOR_EACH_PHI_OR_STMT_USE ( use_p, use_stmt, ssa_iter, SSA_OP_USE )
		{
		  //DEBUG_L("use_p = %p\n",use_p);
		  if ( use_p == NULL ) continue;
		  tree use = USE_FROM_PTR (use_p);
		  if (use == ssa_name)
		    SET_USE ( use_p, new_ssa_name);
		}
	      //DEBUG_L("use_stmt after: ");
	      //DEBUG_F ( print_gimple_stmt, stderr, use_stmt, 0);
	      
	      // Should update_stmt be called here?
	      // It does not seem either harm or help so I'll
	      // leave it in.
	      update_stmt ( use_stmt);
	    }
	  // Modify the LHS too
	  // TBD This code needs to be more general.
	  //DEBUG_L("What is ssa_name? ");
	  //DEBUG_F(flexible_print, stderr, ssa_name, 1, (dump_flags_t)0);
	  gimple *def = SSA_NAME_DEF_STMT ( ssa_name);
	  
	  //DEBUG_L("def: ");
	  //DEBUG_F ( print_gimple_stmt, stderr, def, 0);
	  
	  set_lhs_for ( def, new_ssa_name);

	  update_stmt ( def);

	  // This is where we know that ssa_name needs to be replaced
	  release_ssa_name_fn ( func, ssa_name);
	  
	}
      //INDENT(-4);

      // Might be a bad idea.
      #if 0
      for ( auto iter = ssa_to_delete.begin ();iter != ssa_to_delete.end (); iter++ )
	{
	  remove_default_def ( *iter, func);
	}
      #endif

      pop_cfun ();
    }

  // This used to be off of.. "if ( info->show_all_reorg_cands ) { ..."
  // I'm leaning towards deleting this as redundnt.
  //DEBUG ( info->reorg_dump_file,
  //  "\nEnd of str_reorg_instance_interleave_trans (after mini-psasses):\n\n");
  //DEBUG_F ( print_program, info->reorg_dump_file, PRINT_FORMAT, 4, info);

  // TBD Should this be a diagnostic or not?
  //DEBUG ("INTERNALS PRINT\n");
  //DEBUG_F (apply_to_all_gimple, print_internals, true, (void *)info);
  
  // NOTE, spinning through all the functions and recomputing all the
  // dominace info here is a really bad idea.

  return 0;
}

// Note, the following code might be a bit overly simplistic.
static void
set_lhs_for ( gimple *stmt, tree ssa_name)
{
  switch ( gimple_code ( stmt))
    {
    case GIMPLE_ASSIGN:
      gimple_assign_set_lhs ( stmt, ssa_name);
      break;
    case GIMPLE_CALL:
      gimple_call_set_lhs ( stmt, ssa_name);
      break;
    case GIMPLE_PHI:
		  {
		    gphi *phi_stmt = as_a <gphi *> ( stmt);
		    gimple_phi_set_result ( phi_stmt, ssa_name);
		  }
		  break;
    default:
      fprintf ( stderr, "error: unprecidented gimple for set_lhs_for\n    ");
      print_gimple_stmt( stderr, stmt, 0);
      gcc_assert ( 0);
    }
}

// TBD no longer used... preserve it for a bit, then remove it.
static void
wrangle_ssa_type( tree side, Info_t *info )
{
  tree side_type = TREE_TYPE ( side);
  tree bottom_type = base_type_of ( side_type);
  //DEBUG_L("op: ");
  //DEBUG_F(print_generic_expr, stderr, side, (dump_flags_t)0);
  //DEBUG("\n");
  //DEBUG_L("bottom_type: ");
  //DEBUG_F(print_generic_expr, stderr, bottom_type, (dump_flags_t)0);
  //DEBUG("\n");

  // Maybe we sould pass in ri as an argument???
  ReorgType_t *ri = get_reorgtype_info ( bottom_type, info);
  tree prev_type = side_type;
  tree type = TREE_TYPE ( prev_type);
  //DEBUG_L( "prev_type: %p, type: %p\n", prev_type, type);
  int levels;
  for ( levels = 0; TREE_CODE ( type) == POINTER_TYPE; levels++ )
    {
      prev_type = type;
      type = TREE_TYPE ( prev_type);
      //DEBUG_L( "prev_type: %p, type: %p\n", prev_type, type);
    }

  // I thought about doing this:
  //   Modify type of ssa temp (dicey!)
  //   This changes every instance of * reorg_type to the
  //   new pointre rep in one fell swoop.
  //   I sweat just thinking how crazy this is....
  //
  //   TREE_TYPE ( prev_type) = ri->pointer_rep;

  // TBD might use build_pointer_type to build new type for *(N)reorg_type
  // to *(N-1)ri->pointer_rep
  // Fakes this for levels == 1
  if ( levels == 0)
    {
      //DEBUG_L( "LEVELS  ZERO\n");
      modify_ssa_name_type ( side, ri->pointer_rep);
      //DEBUG_L("after modify_ssa_name_type\n");
    }
  else
    {
      //DEBUG_L( "LEVELS > ZERO\n");
      gcc_assert(0);
    }
}

void
print_internal_op ( tree op)
{
  tree type = TREE_TYPE ( op);
  print_generic_expr ( stderr, op, (dump_flags_t)0);
  fprintf( stderr, "  TYPE:  ");
  print_generic_expr ( stderr, type, (dump_flags_t)0);
  fprintf( stderr, "  MAIN_TYPE:  ");
  print_generic_expr ( stderr, TYPE_MAIN_VARIANT ( type), (dump_flags_t)0);
  fprintf( stderr, "\n");
}

bool
print_internals (gimple *stmt, void *data)
{
  Info_t *info = (Info_t*)data;

  print_gimple_stmt ( stderr, stmt, TDF_SLIM);

  if ( gimple_code ( stmt) == GIMPLE_ASSIGN )
    {
      tree lhs = gimple_assign_lhs( stmt);
      tree rhs1 = gimple_assign_rhs1( stmt);
      tree rhs2 = gimple_assign_rhs2( stmt);
      tree rhs3 = gimple_assign_rhs3( stmt);
      gcc_assert ( lhs);
      gcc_assert ( rhs1);
      
      bool lhs_reorg = tree_contains_a_reorgtype_p ( lhs, info);
      //DEBUG_L("rhs1 = ");
      //DEBUG_F(flexible_print, stderr, rhs1, 1, (dump_flags_t)0);
      bool rhs1_reorg = tree_contains_a_reorgtype_p ( rhs1, info);
      bool rhs2_reorg = tree_contains_a_reorgtype_p ( rhs2, info);
      bool rhs3_reorg = tree_contains_a_reorgtype_p ( rhs3, info);
      
      bool lhs_ssa = lhs_reorg && TREE_CODE(lhs) == SSA_NAME;
      bool rhs1_ssa = rhs1_reorg && TREE_CODE(rhs1) == SSA_NAME;
      bool rhs2_ssa = rhs2_reorg && TREE_CODE(rhs2) == SSA_NAME;
      bool rhs3_ssa = rhs3_reorg && TREE_CODE(rhs3) == SSA_NAME;

      fprintf( stderr, "  LHS%s: ", lhs_ssa ? "*" : "");
      print_generic_expr ( stderr, TREE_TYPE ( lhs), (dump_flags_t)0);

      fprintf( stderr, ",  RHS1%s: ", rhs1_ssa ? "*" : "");
      print_generic_expr ( stderr, TREE_TYPE ( rhs1), (dump_flags_t)0);

      if ( rhs2 )
	{
	  fprintf( stderr, ",  RHS2%s: ", rhs2_ssa ? "*" : "");
	  print_internal_op ( rhs2);
	}

      if ( rhs3 )
	{
	  fprintf( stderr, ",  RHS3%s: ", rhs3_ssa ? "*" : "");
	  print_internal_op ( rhs3);
	}
      fprintf ( stderr, "\n");
    } else
    if ( gimple_code ( stmt) == GIMPLE_PHI )
      {
	use_operand_p phi_op;
	ssa_op_iter iter;
	gphi *phi_stmt = dyn_cast <gphi *> ( stmt);

	tree def = PHI_RESULT ( phi_stmt);
	fprintf( stderr, "  OP: ");
	print_internal_op ( def);
	
	FOR_EACH_PHI_ARG ( phi_op, phi_stmt, iter, SSA_OP_ALL_OPERANDS)
	  {
	    tree op = USE_FROM_PTR ( phi_op);
	    fprintf( stderr, "  OP: ");
	    print_internal_op ( op);
	  }
      }
    else
      {
	ssa_op_iter iter;
	tree op_def;
	use_operand_p opu;
	FOR_EACH_SSA_TREE_OPERAND ( op_def, stmt, iter, SSA_OP_ALL_DEFS)
	  {
	    fprintf( stderr, "  DEF OP: ");
	    print_internal_op ( op_def);
	  }
	FOR_EACH_SSA_USE_OPERAND ( opu, stmt, iter, SSA_OP_ALL_USES)
	  {
	    tree use_op = USE_FROM_PTR ( opu);
	    fprintf( stderr, "  USE OP: ");
	    print_internal_op ( use_op);
	  }
      }
  
  return false;
}

static void
str_reorg_instance_interleave_qual_part ( Info *info)
{
  // TBD save the return value so we can bypass further
  // instance interleaving if none of it is profitable.
  reorg_perf_qual ( info);
}

static void
str_reorg_instance_interleave_type_part ( Info *info)
{
  create_new_types ( info);
}

// Typse for performance qualification

typedef struct reorg_bb_info reorg_bb_info_t;
struct reorg_bb_info {
  basic_block *bb;
};

typedef struct perf_bb_info perf_bb_info_t;

struct acc_base_info {
  bool a_def_def;
  bool a_decl;
  bool a_func;
  bool has_induct_var_acc;
  bool multi_induct;
  bool complicated;
  // TBD Note could look at sign of operation for the
  // induction. Variables moving forward are different (cache access
  // wise) that those moving backward do the sort/compress shouldn't
  // lump them together.
  tree acc_base;
  tree induct_base;
  gimple *function;
};

struct varInfo {
  // Varpool_nodes are a pain to get at so I'll just
  // use the first entry in a run of access info enties
  // where all of the information but the field is the
  // same.
  //varpool_node *var;
  acc_info_t *rep_access;
  // This seems bit map scheme seems tedious and unnecessay.
  // just use the fields
  // sbitmap *bits;
  std::list<tree> fields;
  // The count doesn't vary in the simplified scheme
  //double  count;
};

struct acc_info {
  // trying to get to the varpool seems too hard
  // so I'll try for he decl
  //varpool_node *v;
  tree access;
  tree field; // int field_num;
  acc_base_info_t base_info;
  ReorgType_t *reorg;
};

//struct perf_loop_info {
//  std::vector <varInfo_t*> *vari;
//  class loop *gcc_loop;
//};

static void account_for_access( tree, tree, std::vector <acc_info_t> *, Info_t *);
static bool is_array_access( tree);

static unsigned int
reorg_perf_qual ( Info *info)
{
  if ( info->show_perf_qualify )
    {
      fprintf ( info->reorg_dump_file, "Doing Performance Qualification\n");
    }
  //DEBUG_L("reorg_perf_qual:\n");
  #if 1
  // TBD use design in doc but mark ReorgTypes
  // (do_instance_interleave) that qualify instead of deleting them
  // unless both dead field elimination and field reorderig are not
  // viable (use do_dead_field_elim and do_field_reorder in
  // Reorg_type_t.)
  
  // For the mean time assume if a ReorgType made it here then it's qualified.
  for ( int i = 0; i < info->reorg_type->size (); i++ )
    {
      (*(info->reorg_type))[i].do_instance_interleave = true;
    }
  #endif
  #if 1
  // We are doing a quick and dirty version of performance
  // qualification for testing purposes and possibly the
  // initial version of for the main branch.
  auto reorg_types = info->reorg_type;

  // These are floating point numbers because of the fractional
  // accesses associated the probabilistic approach taken
  // below. By taken below I refer to full algorithm and the
  // quick and dirty initial version.
  
  // reorg was not possible for these
  double cache_accesses = 0.0;

  // reorg possible for these. This doesn't mean it is
  // profitable or even legal for all the types lumped
  // in together here. However, the sum of this and
  // cache_accesses should account for all the array
  // accesses in the program.
  double cache_accesses_noreorg = 0.0;

  // Perf Analysis
  struct cgraph_node *node;
  
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY ( node)  {
    struct function *func = DECL_STRUCT_FUNCTION ( node->decl);

    if ( info->show_perf_qualify )
      {
	fprintf ( info->reorg_dump_file, "Function: ");
	print_generic_expr ( info->reorg_dump_file,
			     TREE_TYPE(TREE_TYPE( func->decl)),
			     (dump_flags_t)0);
      }
    
    // Ulgy GCC idiom with global pointer to current function.
    // However, the dominace calculations other things need it.
    push_cfun ( func);

    #if 1
    if ( dom_info_available_p ( CDI_DOMINATORS) )
      {
	free_dominance_info ( CDI_DOMINATORS);
      }
    calculate_dominance_info (CDI_DOMINATORS);
    #endif

    if ( info->show_perf_qualify )
      {
	fprintf ( info->reorg_dump_file,"  Function: %s\n",
		  lang_hooks.decl_printable_name ( func->decl, 2));
      }

    
    // TBD
    //std::vector<perf_loop_info> loop_perf;
    //loop_perf.reserve ( number_of_loops ( func));
    class loop *loop;
    bool missing_cases = false;
    FOR_EACH_LOOP_FN ( func, loop, LI_ONLY_INNERMOST )
      {
	//We don't need these
	//loop_perf [ loop->num ].vari = new std::vector<varInfo_t*>; // ???
	//loop_perf [ loop->num ].gcc_loop = loop;

	std::vector<acc_info_t> acc_info;
	std::vector<varInfo_t> var_info;
	
        size_t num_bbs = loop->num_nodes;
	basic_block *bbs = get_loop_body ( loop);

	// For the basic blocks in the the loop
	for ( unsigned i = 0; i < loop->num_nodes; i++)
	  {
	    basic_block bb = bbs [i];
	    //DEBUG_A("BB %i:\n", bb->index);
	    //INDENT(4);
	    for ( auto gsi = gsi_start_bb ( bb); !gsi_end_p ( gsi); gsi_next ( &gsi) )
	      {
		gimple *stmt = gsi_stmt ( gsi);
		//DEBUG_A("examine: ");
		//DEBUG_F ( print_gimple_stmt, stderr, stmt, TDF_DETAILS);
		//INDENT(4);

		if ( gimple_code ( stmt) == GIMPLE_LABEL  ||
		     gimple_code ( stmt) == GIMPLE_SWITCH    ) continue;
		  		
		unsigned n_ops = gimple_num_ops( stmt);
		tree op;
		unsigned ith_op;
		for ( ith_op = 0; ith_op < n_ops; ith_op++ )
		  {
		    op = gimple_op ( stmt, ith_op);
		    // It's lieing about the number of operands... so...
		    if ( op == NULL ) continue;
		    //DEBUG_A("op[%d]: %p, ", ith_op, op);
		    //DEBUG_F(flexible_print, stderr, op, 1, (dump_flags_t)0);
		    ReorgType_t *tri = tree_contains_a_reorgtype ( op, info);
		    enum ReorgOpTrans optran = recognize_op ( op, false, info);
		    // TBD This is where we need to remember
		    // each germane access
		    const char *s = optrans_to_str( optran);
		    // Commenting out these 3 debug commands causes a
		    // regression
		    //DEBUG_A(", %s\n", s);
		    if ( tri != NULL )
		      {
			//DEBUG(", ");
			//DEBUG_F(print_reorg, stderr, 0, tri);
		      }
		    else
		      {
			//DEBUG("\n");
			;
		      }
		    switch ( optran)
		      {
		      case ReorgOpT_Indirect:
			{
			  // TBD
			  // Is the var an induction variable for this loop?
			  // If so find the assocaite varpool_node and push
			  // it and the field onto var_acc_info;
			  tree op_var = TREE_OPERAND( op, 0);
			  tree op_field = TREE_OPERAND( op, 1);
			  // Since doesn't have an easily exposed mechanism
			  // for induction variable I'm hand waving here.
			  if ( !expr_invariant_in_loop_p ( loop, op_var) )
			    {
			      account_for_access ( op_var, op_field, &acc_info, info);
			    }
			}
			break;
		      case ReorgOpT_Array:
			{
			  // TBD
			  // Is the var an induction variable for this loop?
			  // If so find the assocaite varpool_node and push
			  // it and the field onto var_acc_info;
			  tree op_var = TREE_OPERAND( op, 0);
			  tree op_field = TREE_OPERAND( op, 1);
			  // Since doesn't have an easily exposed mechanism
			  // for induction variable I'm hand waving here.
			  if ( !expr_invariant_in_loop_p ( loop, op_var) )
			    {
			      account_for_access ( op_var, op_field, &acc_info, info);
			    }
			}
		      case ReorgOpT_AryDir:
		      case ReorgOpT_Deref: // ??
			missing_cases = true;
		      }
		  }
		//INDENT(-4);
	      }
	    //INDENT(-4);
	  }

	//DEBUG_L("Dumping acc_info:\n");
	for ( auto aci = acc_info.begin (); aci != acc_info.end (); aci++ )
	  {
	    //DEBUG_A("variable:\n");
	    //DEBUG_F( tell_me_about_ssa_name, (*aci).access, debug_indenting + 4);
	    //DEBUG_A("field: ");
	    //DEBUG_F( flexible_print, stderr, (*aci).field, 1, (dump_flags_t)0);
	  }

	//DEBUG_A("before sort: \n");
	//DEBUG_F(print_acc_infos, stderr, acc_info );

	// Sort and compact the access infos.
	stable_sort ( acc_info.begin (), acc_info.end (), acc_lt);

	//DEBUG_A("before compress: \n");
	//DEBUG_F(print_acc_infos, stderr, acc_info );

	// Sort and compact the access infos.
	std::stable_sort ( acc_info.begin (), acc_info.end (), acc_lt);
	
	compress_acc_infos ( acc_info );

	//DEBUG_A("after compress: \n");
	//DEBUG_F(print_acc_infos, stderr, acc_info );
	
	// Obtain loop count by looking at all the block counts.
	unsigned loop_count = 0;
	for ( unsigned i = 0; i < loop->num_nodes; i++)
	  {
	    basic_block bb = bbs [i];
	    loop_count = MAX( loop_count, bb->count.value ());
	  }
	//DEBUG_L("loop_count = %d, nb_iterations_estimate = %ld\n",
	//	loop_count, loop->nb_iterations_estimate);

	// Create the variable infos
	varInfo_t var_entry;
	var_entry.rep_access = &acc_info[0];
	unsigned len = acc_info.size ();

	// If no accesses detected, never for this loop.
	if ( len == 0 ) continue;
	
 	if ( len == 1 )
	  {
	    var_entry.fields.push_front ( acc_info[0].field);
	  }
	else
	  {
	    unsigned i, j;
	    for ( i = 0, j = 1; j < len; j++ )
	      {
		acc_info_t *a_of_i = &acc_info[i];
		acc_info_t *a_of_j = &acc_info[j];
		var_entry.fields.push_front ( a_of_i->field);
		if ( !all_but_field_eq ( *a_of_i, *a_of_j ) )
		  {
		    var_info.push_back( var_entry);
		    var_entry.rep_access = a_of_j;
		    var_entry.fields.clear ();
		    a_of_i = a_of_j;
		  }
	      }
	  }
	var_info.push_back( var_entry);

	if ( info->show_perf_qualify )
	  {
	    fprintf ( stderr, "%d VarInfos\n", var_info.size ());
	  }
	//DEBUG_F(print_var_infos, stderr, var_info);

	//
	// Model the performance
	//
	//DEBUG_A("Model The Performance\n");

	// Originally this was done per bb but now it has to be per
	// loop. TBD But perf_bb is per loop so we need something similar
	// per loop.

	for ( auto pvi = var_info.begin (); pvi != var_info.end (); pvi++ )
	  { // 676
	    //tree base_type = base_type_of( pvi->rep_access.access);
	    ReorgType_t *ri = pvi->rep_access->reorg;
	    
	    // Reorg accounting
	    //DEBUG_L("\n");
	    //DEBUG_A("Reorg Accounting\n");
	    
	    if( ri != NULL )
	      {
		double reorg_nca = 0.0;

		//DEBUG_A("  for: ");
		//DEBUG_F( flexible_print, stderr, ri->gcc_type, 1, (dump_flags_t)0);
		//INDENT(4);
		for ( auto fldi = pvi->fields.begin (); fldi != pvi->fields.end (); fldi++ )
		  {
		    unsigned HOST_WIDE_INT fld_width =
		      tree_to_uhwi ( DECL_SIZE ( *fldi));
		    double effect = alignment_effect ( fld_width);
		    double product = loop_count * effect;
		    reorg_nca += product;
		    //DEBUG_A("Add loop_count * effect (%d * %f = %f) to reorg_nca (now %f)\n",
		    //	    loop_count, effect, product, reorg_nca);
		  }
		//INDENT(-4);
		ri->instance_interleave.reorg_perf += reorg_nca;
		//DEBUG_A("Add reorg_nca (%f) to reorg_perf (now %e)\n",
		//	reorg_nca, ri->instance_interleave.reorg_perf);
              } // 699

	    // regular accounting
	    //DEBUG_L("\n");
	    //DEBUG_A("Regular Accounting\n");
	    
	    double regular_nca = 0.0;
	    sbitmap cache_model = sbitmap_alloc(1);
	    
	    for( auto pv2i = var_info.begin (); pv2i != var_info.end (); pv2i++ )
	      { // 704
		tree access = pv2i->rep_access->base_info.acc_base;
		tree base_type; // = base_type_of ( access);
		if ( pv2i->rep_access->reorg != NULL )
		  {
		    //DEBUG_A("Base type from reorg: ");
		    base_type = pv2i->rep_access->reorg->gcc_type;
		  }
		else
		  {
		    //DEBUG_A("Base type from access: ");
		    if ( TREE_TYPE ( access ) != NULL )
		      {
			base_type = base_type_of ( access);
		      }
		    else
		      {
			gcc_assert (0);
		      }
		  }
		//DEBUG_F( flexible_print, stderr, base_type, 1, (dump_flags_t)0);

		bool base_type_isa_decl = DECL_P ( base_type );

		// create a tiny model of the cache big
		// enough for this record.
		#if 0
		tree base_type_size = base_type_isa_decl ?
		  DECL_SIZE ( base_type )
		  :
		  TYPE_SIZE ( base_type);
		#else
		tree base_type_size;
		if ( base_type_isa_decl )
		  {
		    //DEBUG_A("decl\n");
		    switch ( TREE_CODE (base_type) )
		      {
		      case VAR_DECL:
			{
			  //DEBUG_A("VAR_DECL\n");
			  base_type_size = TYPE_SIZE ( base_type);
			  break;
			}
		      case FIELD_DECL:
			{
			  //DEBUG_A("VAR_DECL\n");
			  base_type_size = TYPE_SIZE ( TREE_TYPE ( base_type));
			  break;
			}
		      default:
			{
			  //DEBUG_A("other decl %s\n", code_str(TREE_CODE (base_type)));
			  gcc_assert(0);
			}
		      }
		  }
		else
		  {
		    //DEBUG_A("nondecl %s\n", code_str(TREE_CODE (base_type)));
		    if ( TREE_CODE ( base_type) == SSA_NAME )
		      {
			base_type_size = TYPE_SIZE ( TREE_TYPE( base_type));
		      }
		    else
		      {
			base_type_size = TYPE_SIZE ( base_type);
		      }
		  }
		#endif
		    
		unsigned HOST_WIDE_INT len =
		  (( tree_to_uhwi ( base_type_size)
		     +
		     param_l1_cache_line_size -1)
		   /
		   param_l1_cache_line_size)
		  +
		  1;
		//DEBUG_L("\n");
		//DEBUG_A("cache len = %d (lines), for: ", len);
		//DEBUG_F( flexible_print, stderr, base_type, 0, (dump_flags_t)0);
		//DEBUG("%sstruct\n")
	      
		// TBD Does this clear the bits??? It needs to.
		// Each bit represents a cache line.
		cache_model = sbitmap_resize( cache_model, (unsigned) len, 0);
		double accum = 0.0;
		int nrbo = 0;
		tree type = TREE_TYPE ( base_type);
		
		//bool a_record = type == NULL && TREE_CODE ( type) == RECORD_TYPE;
		bool a_record = type != NULL && TREE_CODE ( type) == RECORD_TYPE;
		if ( base_type_isa_decl && a_record )
		  {
		    for ( auto field_ex = TYPE_FIELDS ( base_type);
			  field_ex; 
			  field_ex = DECL_CHAIN ( field_ex) )
		      {
			nrbo++;
			// Looking back on my design I don't have a clue
			// why this is here and what it does. Sigh...
			unsigned HOST_WIDE_INT base_offset =
			  tree_to_uhwi ( DECL_FIELD_OFFSET( field_ex));
			//DEBUG_L("\n");
			//DEBUG_A("For field_ex: ");
			//DEBUG_F( flexible_print, stderr, field_ex, 0, (dump_flags_t)0);
			//DEBUG(", nrbo %d, base_offset %d\n", nrbo, base_offset);
			
			// Access accounting

			//INDENT(4);
			for ( auto fldi = pv2i->fields.begin ();
			      fldi != pv2i->fields.end (); fldi++ )
			  {
			    tree field = *fldi;
			    unsigned HOST_WIDE_INT fld_width, fld_offset;
			    fld_width = tree_to_uhwi ( DECL_SIZE ( field));
			    fld_offset = tree_to_uhwi ( DECL_FIELD_OFFSET ( field));
			    //DEBUG_A("Field: ");
			    //DEBUG_F( flexible_print, stderr, field, 0, (dump_flags_t)0);
			    //DEBUG(", width = %d, offset = %d\n", fld_width, fld_offset);
			    int chari;
			    //INDENT(4);
			    for ( chari = 0; chari < fld_width; chari++ )
			      {
				int loc = (chari + fld_offset + base_offset)
				  /
				  param_l1_cache_line_size;
				//DEBUG_A("loc: %d\n", loc);
				bitmap_set_bit ( cache_model, loc);
			      }
			    //INDENT(-4);
			  }
			//INDENT(-4);
			unsigned bcount = bitmap_count_bits ( cache_model);
			accum += bcount;
			//DEBUG_L("\n");
			//DEBUG_A("Add popcount of cache (%d) to accum (now %f)\n",
			//	bcount, accum);
			bitmap_clear ( cache_model);
		      }
		  }
		else
		  {
		    nrbo = 1;
		    accum++;
		    //DEBUG_L("\n");
		    //DEBUG_A("nrbo = 1, increment accum to %f\n", accum);
		  }
		#if 1	
		double amount = accum / nrbo;
		double product = amount * loop_count;
		regular_nca += product;
		//DEBUG_L("\n");
		//DEBUG_A("Add loop_count*accum/nrbo (%f*%f/%d = %f) to regular_nca (now %e)\n",
		//	loop_count, accum, nrbo, product, regular_nca);
		#else
		double amount = accum / nrbo;
		regular_nca += amount;
		//DEBUG_L("\n");
		//DEBUG_A("Add accum/nrbo (%f/%d = %f) to regular_nca (now %e)\n",
		//	accum, nrbo, amount, regular_nca);
		#endif
	      } // 739
	    sbitmap_free ( cache_model);
	    
	    if( ri != NULL ) {
	      ri->instance_interleave.regular_perf += regular_nca;
              cache_accesses_noreorg += regular_nca;
	      //DEBUG_L("\n");
	      //DEBUG_A("Add regular_nca (%f) to regular_perf (now %e)",
	      //	      regular_nca, ri->instance_interleave.regular_perf);
	      //DEBUG_A("  and to cache_accesses_noreorg (now %e)\n",
	      //	      cache_accesses_noreorg);
            } else {
	        cache_accesses += regular_nca;
            }
	  } // end for prop_var
      } //

    if ( info->show_perf_qualify && missing_cases )
      {
	fprintf ( info->reorg_dump_file,
		  "    Ignored unimplemented cases when finding accesses.\n");
      }
    
    free_dominance_info ( CDI_DOMINATORS);
    pop_cfun ();
  }

  // TBD Somebody somewhere needs to compute:
  //   reorg_perf
  //   regular_perf
  //   cache_accesses
  //   cache_accesses_noreorg

  double total_cache_accesses =
    cache_accesses + cache_accesses_noreorg;

  info->total_cache_accesses = total_cache_accesses;

  if ( info->show_perf_qualify )
    {
      fprintf ( info->reorg_dump_file, "total_cache_accesses: %e\n\n",
		total_cache_accesses);
    }

  if ( info->show_perf_qualify )
    {
      fprintf ( info->reorg_dump_file,
		"Decide which reorgTypes fail performance qualification\n");
    }
  //
  // Decide which reorgTypes fail performance qualification
  //

  // We have the total accesses per type. Use that to see
  // if the type qualifies.
  for ( auto reorgi = reorg_types->begin ();
	reorgi != reorg_types->end (); reorgi++ )
    {
      double with_opt = reorgi->instance_interleave.reorg_perf;
      double without_opt = reorgi->instance_interleave.regular_perf;
      double raw_effect = with_opt/without_opt;
      double absolute_effect =
        (without_opt - with_opt) / total_cache_accesses;
      //DEBUG_A("For ");
      //DEBUG_F(flexible_print, stderr, reorgi->gcc_type, 0, (dump_flags_t)0);
      //DEBUG(" with_opt: %e, without_opt %e, total_cache_accesses %e\n",
      //	    with_opt, without_opt, total_cache_accesses);
      //DEBUG_A("  Raw Effect: %5.4f, Absolute Effect %5.4f\n",
      //	      raw_effect, absolute_effect);

      // Note, there would need to be a multi-pool case here if
      // that is every done.

      // If the relative effect is small enough don't bother.
      if ( raw_effect < SINGLE_POOL_RAW_SKIP_IT )
	{
	  if ( info->show_perf_qualify )
	    {
	      fprintf ( info->reorg_dump_file, "  Disqualified: ");
	      flexible_print ( info->reorg_dump_file, reorgi->gcc_type, 0,
			       (dump_flags_t)0);
	      fprintf ( info->reorg_dump_file, ": Very small effect:\n");
	      fprintf ( info->reorg_dump_file,
			"    raw_effect %5.4f < SINGLE_POOL_RAW_SKIP_IT %5.4f\n",
			raw_effect, SINGLE_POOL_RAW_SKIP_IT);
	    }
	  reorgi->do_instance_interleave = false;
	  continue;
	}
      // the relative effect is big enough do it anyway
      // otherwise look at the absolute effect.
      if ( raw_effect >= SINGLE_POOL_RAW_DO_IT_ALWAYS )
	{

	  if ( info->show_perf_qualify )
	    {
	      fprintf ( info->reorg_dump_file, "  Qualified: ");
	      flexible_print ( info->reorg_dump_file, reorgi->gcc_type, 0,
			       (dump_flags_t)0);
	      fprintf ( info->reorg_dump_file, ": Large raw effect:\n");
	      fprintf ( info->reorg_dump_file,
			"    raw_effect %5.4f >= SINGLE_POOL_RAW_DO_IT_ALWAYS %5.4f\n",
			raw_effect, SINGLE_POOL_RAW_DO_IT_ALWAYS);
	    }
	  
	  reorgi->do_instance_interleave = true;
	  continue;
	}
      if ( absolute_effect < SINGLE_POOL_ABS_SKIP_IT )
	{
	  if ( info->show_perf_qualify )
	    {
	      fprintf ( info->reorg_dump_file, "  Disqualified: ");
	      flexible_print ( info->reorg_dump_file, reorgi->gcc_type, 0,
			       (dump_flags_t)0);
	      fprintf ( info->reorg_dump_file, ": Very small absolute effect:\n");
	      fprintf ( info->reorg_dump_file,
			"    absolute_effect %5.4f < SINGLE_POOL_ABS_SKIP_IT %5.4f\n",
			absolute_effect, SINGLE_POOL_ABS_SKIP_IT);
	    }
	  
	  reorgi->do_instance_interleave = false;
	  continue;
	}
      if ( absolute_effect >= SINGLE_POOL_ABS_DO_IT_ALWAYS )
	{

	  if ( info->show_perf_qualify )
	    {
	      fprintf ( info->reorg_dump_file, "  Qualified: ");
	      flexible_print ( info->reorg_dump_file, reorgi->gcc_type, 0,
			       (dump_flags_t)0);
	      fprintf ( info->reorg_dump_file, ": Large absolute effect:\n");
	      fprintf ( info->reorg_dump_file,
			"    absolute_effect %5.4f >= SINGLE_POOL_ABS_DO_IT_ALWAYS %5.4f\n",
			absolute_effect, SINGLE_POOL_ABS_DO_IT_ALWAYS);
	    }
	  
	  reorgi->do_instance_interleave = true;
	  continue;
	}
      
      // We fitted a linear equation to the corners of the
      // effects above and use it to determine when
      // to disqualify a type
      double cut_off = cut_off_eq_single_pool ( absolute_effect);
      if ( raw_effect < cut_off )
	{

	  if ( info->show_perf_qualify )
	    {
	      fprintf ( info->reorg_dump_file, "  Disqualified: ");
	      flexible_print ( info->reorg_dump_file, reorgi->gcc_type, 0,
			       (dump_flags_t)0);
	      fprintf ( info->reorg_dump_file, ": Failed cut off equations:\n");
	      fprintf ( info->reorg_dump_file,
			"    raw_effect %5.4f < cut_off %5.4f\n",
			raw_effect, cut_off);
	    }
	  
	  reorgi->do_instance_interleave = false;
	  continue;
	}

      if ( info->show_perf_qualify )
	{
	  fprintf ( info->reorg_dump_file, "  Qualified: ");
	  flexible_print ( info->reorg_dump_file, reorgi->gcc_type, 0,
			   (dump_flags_t)0);
	  fprintf ( info->reorg_dump_file, ": Passed cut off equations:\n");
	  fprintf ( info->reorg_dump_file,
			"    raw_effect %5.4f >= cut_off %5.4f\n",
			raw_effect, cut_off);
	}
      
    }
  #endif
}

static void
print_var_info ( FILE *file, varInfo_t &vinfo)
{
  print_acc_info ( file, vinfo.rep_access );
  for ( auto fi = vinfo.fields.begin (); fi != vinfo.fields.end (); fi++ )
    {
      if ( fi != vinfo.fields.begin () ) fprintf ( stderr,", ");
      flexible_print (  stderr, *fi, 0, (dump_flags_t)0);
    }
  fprintf ( stderr,"\n");
}

static void
print_var_infos ( FILE *file, std::vector<varInfo_t> &vinfo)
{
  fprintf( stderr, "print_var_infos:\n");
  for ( auto vi = vinfo.begin (); vi != vinfo.end (); vi++ )
    {
      print_var_info ( file, *vi);
    }
}

static void
compress_acc_infos ( std::vector <acc_info_t> ainfo )
{
  unsigned len = ainfo.size ();
  //DEBUG_L("compress_acc_infos: len in %d, ",len);
  if ( len <= 1 ) return;
  unsigned i, j;
  for ( i = j = 1; j < len; j++ )
    {
      ainfo[i] = ainfo[j];
      if ( !acc_eq ( ainfo[i], ainfo[i - 1]) ) i++;
    }
  if ( i == j ) return;
  ainfo.resize ( len - (j -i));
  //DEBUG_L("len out %d, ", ainfo.size ());
}

static void
print_acc_info ( FILE *file, acc_info_t *ainfo )
{
  //DEBUG_L("print_acc_info: ainfo %p\n", ainfo);
  fprintf ( file, "%s%s%s%s%s%s\n",
	    ainfo->base_info.a_def_def ? ", deflt_def" : "",
	    ainfo->base_info.a_decl ? ", decl" : "",
	    ainfo->base_info.a_func ? ", a_func" : "",
	    ainfo->base_info.has_induct_var_acc ? ", induct" : "",
	    ainfo->base_info.multi_induct ? ", multi" : "",
	    ainfo->base_info.complicated ? ", complicated" : "");
  fprintf ( file, "   base var ");
  flexible_print ( stderr, ainfo->base_info.acc_base, 0, (dump_flags_t)0);
  if ( ainfo->base_info.has_induct_var_acc )
    {
      fprintf ( file, ", induc var ");
      flexible_print ( stderr, ainfo->base_info.acc_base,
		       0, (dump_flags_t)0);
    }
  fprintf ( file, ", field ");
  flexible_print ( stderr, ainfo->field, 0, (dump_flags_t)0);
  if ( ainfo->reorg )
    {
      fprintf ( file, ", reorg of ");
      flexible_print ( stderr, ainfo->reorg->gcc_type,
		       0, (dump_flags_t)0);
    }
  fprintf ( file, "\n");
}

static void
print_acc_infos ( FILE *file, std::vector <acc_info_t> ainfo )
{
  fprintf ( file, "print_acc_infos:\n");
  unsigned i;
  unsigned len = ainfo.size ();

  for ( i = 0; i < len; i++ )
    {
      fprintf ( file, "[%d] ", i);
      print_acc_info ( file, &ainfo[i]);
    }
}


// decls < default defs < defined by function
static bool
acc_lt_acc_category ( const acc_info_t& a, const acc_info_t& b )
{
  int ord_a = a.base_info.a_decl ? 1 : a.base_info.a_def_def ? 2 : 3;
  int ord_b = b.base_info.a_decl ? 1 : b.base_info.a_def_def ? 2 : 3;
  if ( ord_a < ord_b ) return true;
  if ( ord_a > ord_b ) return false;
  switch ( ord_a ) {
  case 1:
    // The field isn't there for decls, it's the index. Ignoring the
    // index is harmless.  This is becauseif if we take an arbitary
    // small number of iterations of a loop, then all the permutations
    // of the accesses in those iterations touch the same number of
    // cache lines (just in a different order.)
    return false;
  case 2:
    {
      if ( a.access < b.access ) return true;
      if ( a.access > b.access ) return false;
      return a.field < b.field;
    }
  case 3:
    {
      if ( a.base_info.function < b.base_info.function ) return true;
      if ( a.base_info.function > b.base_info.function ) return false;
      return a.field < b.field;
    }
  }
}

// Complicated is less than noncomplicated and null reorgs are less than non
// null reorgs.
static bool
acc_lt ( const acc_info_t& a, const acc_info_t& b )
{
  if ( a.base_info.complicated && !b.base_info.complicated ) return true;
  if ( !a.base_info.complicated && !b.base_info.complicated ) return false;
  if ( a.reorg == NULL )
    {
      if ( b.reorg != NULL ) return true;
      // compare non_reorg_bit
      return acc_lt_acc_category ( a, b);
    }
  else
    {
      if ( b.reorg == NULL ) return false;
      if ( a.reorg < b.reorg ) return true;
      if ( a.reorg > b.reorg ) return false;
      // compare non_reorg_bit
      return acc_lt_acc_category ( a, b);
    }
}

static bool
acc_eq ( const acc_info_t& a, const acc_info_t& b )
{
  // nothing complicated is equal to anything else.  Being complicated
  // is basically saying that little is really know about it and it's
  // difficult if not meaningless to analyze in depth given this
  // framework.
  if ( a.base_info.complicated || b.base_info.complicated ) return false;
  if ( a.reorg != b.reorg ) return false;
  int ord_a = a.base_info.a_decl ? 1 : a.base_info.a_def_def ? 2 : 3;
  int ord_b = b.base_info.a_decl ? 1 : b.base_info.a_def_def ? 2 : 3;
  if ( ord_a != ord_b ) return false;
  switch ( ord_a ) {
  case 1:
  case 2:
    {
      if ( a.access != b.access ) return false;
    }
  case 3:
    {
      if ( a.base_info.function != b.base_info.function ) return false;
    }
  }
  return a.field == b.field;
}

static bool
all_but_field_eq ( const acc_info_t& a, const acc_info_t& b )
{
  // nothing complicated is equal to anything else.  Being complicated
  // is basically saying that little is really know about it and it's
  // difficult if not meaningless to analyze in depth given this
  // framework.
  if ( a.base_info.complicated || b.base_info.complicated ) return false;
  if ( a.reorg != b.reorg ) return false;
  int ord_a = a.base_info.a_decl ? 1 : a.base_info.a_def_def ? 2 : 3;
  int ord_b = b.base_info.a_decl ? 1 : b.base_info.a_def_def ? 2 : 3;
  if ( ord_a != ord_b ) return false;
  switch ( ord_a ) {
  case 1:
  case 2:
    {
      return a.access == b.access;
    }
  case 3:
    {
      return a.base_info.function == b.base_info.function;
    }
  }
}

#define SINGLE_POOL_SLOPE					\
  ((SINGLE_POOL_RAW_DO_IT_ALWAYS - SINGLE_POOL_RAW_SKIP_IT)	\
  /								\
   (SINGLE_POOL_ABS_DO_IT_ALWAYS - SINGLE_POOL_ABS_SKIP_IT))

#define SINGLE_POOL_INTERSECT		        \
  (SINGLE_POOL_RAW_SKIP_IT                      \
   -						\
   SINGLE_POOL_SLOPE * SINGLE_POOL_ABS_SKIP_IT)

static double
cut_off_eq_single_pool( double x)
{
  return SINGLE_POOL_SLOPE * x + SINGLE_POOL_INTERSECT;
}

static double
alignment_effect( unsigned HOST_WIDE_INT width )
{
  unsigned HOST_WIDE_INT times = param_l1_cache_line_size / width; // ??
  unsigned HOST_WIDE_INT rem   = param_l1_cache_line_size % width;
  if( rem == 0 ) {
    return 1.0;
  }
  unsigned HOST_WIDE_INT m, n, g;
  g = gcd( param_l1_cache_line_size, width);
  m = param_l1_cache_line_size / g;
  n = width / g;
  return 1.0 + (n - 1.0)/m;
}

static void
header ( bool initialize )
{
  static bool emit_header;
  if ( initialize )
    {
      emit_header = true;
    }
  else
    {
      if ( emit_header )
	{
	  emit_header = false;
	  fprintf( stderr, "SANITY CHECKING FAILURE:\n");
	}
    }
}

// TBD I have doubts that this what is really needed
bool
is_array_access( tree acc)
{
  tree type = TREE_TYPE ( acc);
  if( TREE_CODE( type) == ARRAY_TYPE )
    return true;
  while( POINTER_TYPE_P( type) ) {
    type = TREE_TYPE( type);
    if( TREE_CODE( type) == ARRAY_TYPE )
      return true;
  }
  return false;
}

static void
account_for_access ( tree access, tree field, std::vector <acc_info_t> *acc_info, Info_t *info)
{
  //DEBUG_A("account_for_use var: ");
  //DEBUG_F(flexible_print, stderr, access, 0, (dump_flags_t)0);
  //DEBUG(", field: ");
  //DEBUG_F(flexible_print, stderr, field, 1, (dump_flags_t)0);
  
  // assert might eventually make sense but not yet
  //gcc_assert ( TREE_CODE ( ssa_var) == SSA_NAME);
  acc_info_t ai;
  //ai.v = SSA_NAME_VAR ( ssa_var);
  ai.access = access; // TBD We need to see if we can find the decl
  ai.field = field;
  ai.reorg = tree_contains_a_reorgtype ( access, info);
  analyze_access ( access, &ai);
  // don't count this acces if there is no associated induction variable
  if ( !ai.base_info.has_induct_var_acc ) return;
  // Otherwise add the access
  acc_info->push_back( ai);
}

static void
tmasn_helper ( tree t, int indent, std::set<tree> *already )
{
  //DEBUG_A("");
  fprintf( stderr, "%*s", indent, " ");
  indent += 4;
  flexible_print ( stderr, t, 0, (dump_flags_t)0);
  if ( already->find (t) != already->end () )
    {
      fprintf( stderr, " <Induction>\n");
      return;
    }
  else
    {
      fprintf( stderr, "\n");
    }
  //DEBUG_L("code: %s\n", code_str(TREE_CODE (t)));
  if ( TREE_CODE (t) == SSA_NAME )
    {
      already->insert (t);
      gimple *stmt = SSA_NAME_DEF_STMT (t);
      fprintf( stderr, "%*sSSA_NAME defined in: ", indent - 4, " ");
      print_gimple_stmt( stderr, stmt, TDF_DETAILS);
      if ( gimple_code ( stmt) == GIMPLE_PHI )
	{
	  gphi *phi_stmt = dyn_cast <gphi *> ( stmt);
	  for (int i = 0; i < gimple_phi_num_args (phi_stmt); i++)
	    {
	      tree *arg = gimple_phi_arg_def_ptr (phi_stmt, i);
	      tmasn_helper ( *arg, indent, already);
	    }
	}
      else
	{
	  bool a_ass = gimple_code ( stmt) == GIMPLE_ASSIGN;
	  bool a_call = gimple_code ( stmt) == GIMPLE_CALL;
	  // This was being triggered an add: op = op + op
	  //gcc_assert ( a_ass || a_call );

	  if ( a_call )
	    {
	      for ( int i = 0; i < gimple_call_num_args ( stmt); i++ )
		{
		  tmasn_helper ( gimple_call_arg  ( stmt, i) , indent, already);
		}
	    }
	  else
	    {
	      // Note, start with one to skip lhs op. 
	      for ( int i = 1; i < gimple_num_ops ( stmt); i++ )
		{
		  tmasn_helper ( gimple_op ( stmt, i) , indent, already);
		}
	    }
	}
      return;
    }
  if ( DECL_P ( t) )
    {
      return;
    }
  if ( TREE_CODE ( t) == MEM_REF )
    {
      tree t_0 = TREE_OPERAND ( t, 0);
      fprintf( stderr, "%*sMEM_REF t_0: ", indent - 4, " ");
      flexible_print ( stderr, t_0, 1, (dump_flags_t)0);
      tmasn_helper ( t_0 , indent, already);
      return;
    }
  if ( TREE_CODE ( t) == INTEGER_CST ) return;
  fprintf ( stderr, "unanticipated TREE_CODE\n");
  gcc_assert ( 0);
}

static void
tell_me_about_ssa_name ( tree ssa_name, int indent)
{
  fprintf(stderr,"about:\n");
  std::set<tree> already;
  tmasn_helper ( ssa_name, indent, &already);
}

static unsigned insane_helper;

static void
an_ac_helper ( tree t, int indent, std::set<tree> *already, acc_info_t *ainfo )
{
  gcc_assert ( insane_helper < 100 );
  insane_helper++;
  acc_base_info_t *binfo = &ainfo->base_info;
  //DEBUG_A("%*s", indent, " ");
  indent += 4;
  //DEBUG_F( flexible_print, stderr, t, 0, (dump_flags_t)0);
  if ( already->find (t) != already->end () )
    {
      //DEBUG(" <Induction>\n");
      binfo->multi_induct =
	binfo->multi_induct || binfo->has_induct_var_acc;
      binfo->induct_base = t;
      binfo->has_induct_var_acc = true;
      return;
    }
  else
    {
      //DEBUG("\n");
      
    }
  //DEBUG_A("%*scode: %s\n", indent, " ", code_str(TREE_CODE (t)));
  if ( TREE_CODE (t) == SSA_NAME )
    {
      already->insert (t);
      gimple *stmt = SSA_NAME_DEF_STMT (t);
      //DEBUG_A("%*sSSA_NAME defined in: ", indent, " ");
      //DEBUG_F(print_gimple_stmt, stderr, stmt, TDF_DETAILS);
      if ( SSA_NAME_IS_DEFAULT_DEF ( t ) )
        {
	  binfo->acc_base = t;
	  binfo->complicated =
	    binfo->complicated || binfo->a_def_def || binfo->a_decl || binfo->a_func;
	  binfo->a_def_def = true;
	}
      if ( gimple_code ( stmt) == GIMPLE_PHI )
	{
	  gphi *phi_stmt = dyn_cast <gphi *> ( stmt);
	  for (int i = 0; i < gimple_phi_num_args (phi_stmt); i++)
	    {
	      tree *arg = gimple_phi_arg_def_ptr (phi_stmt, i);
	      an_ac_helper ( *arg, indent, already, ainfo);
	    }
	}
      else
	{
	  bool a_ass = gimple_code ( stmt) == GIMPLE_ASSIGN;
	  bool a_call = gimple_code ( stmt) == GIMPLE_CALL;
	  // This was being triggered an add: op = op + op
	  //gcc_assert ( a_ass || a_call );

	  if ( a_call )
	    {
	      binfo->acc_base = t;
	      binfo->complicated =
		binfo->complicated || binfo->a_def_def || binfo->a_decl || binfo->a_func;
	      binfo->a_func = true;
	      binfo->function = stmt;
	      // Question, do we want to walk the call arguements???
	      // Because how do the arguments effect the return value?
	      // It's basically unknow so we shouldn't walk the
	      // arguemets.
	      // 
	      for ( int i = 0; i < gimple_call_num_args ( stmt); i++ )
		{
		  an_ac_helper ( gimple_call_arg  ( stmt, i), indent, already, ainfo);
		}
	    }
	  else
	    {
	      // Note, start with one to skip lhs op. 
	      for ( int i = 1; i < gimple_num_ops ( stmt); i++ )
		{
		  an_ac_helper ( gimple_op ( stmt, i), indent, already, ainfo);
		}
	    }
	}
      return;
    }
  if ( DECL_P ( t) )
    {
      binfo->acc_base = t;
      binfo->complicated =
		binfo->complicated || binfo->a_def_def || binfo->a_decl || binfo->a_func;
      binfo->a_decl = true;
      
      //DEBUG_A("field (index) for a_decl: ");
      //DEBUG_F(flexible_print, stderr, ainfo->field, 1, (dump_flags_t)0);

      if ( TREE_CODE ( t) != FIELD_DECL )
	{
	  an_ac_helper ( ainfo->field, indent, already, ainfo);
	}
      return;
    }
  if ( TREE_CODE ( t) == MEM_REF )
    {
      tree t_0 = TREE_OPERAND ( t, 0);
      //DEBUG_A("%*sMEM_REF t_0: ", indent, " ");
      //DEBUG_F(flexible_print, stderr, t_0, 1, (dump_flags_t)0);
      an_ac_helper ( t_0 , indent, already, ainfo);
      return;
    }
  if ( TREE_CODE ( t) == COMPONENT_REF )
    {
      tree t_0 = TREE_OPERAND ( t, 0);
      //DEBUG_A("%*sCOMPONENT_REF t_0: ", indent, " ");
      //DEBUG_F(flexible_print, stderr, t_0, 1, (dump_flags_t)0);
      an_ac_helper ( t_0 , indent, already, ainfo);
      return;
    }
  if ( TREE_CODE ( t) == INTEGER_CST ) return;
  if ( TREE_CODE ( t) == ADDR_EXPR ) return;
  fprintf ( stderr, "Unanticipated TREE_CODE\n");
  gcc_assert ( 0);
}

static void
analyze_access ( tree access, acc_info_t *acc_info)
{
  insane_helper = 0;
  acc_base_info_t *base_info = &acc_info->base_info;
  //DEBUG_A("analyze_access:\n");
  base_info->a_def_def = false;
  base_info->a_decl = false;
  base_info->a_func = false;
  base_info->has_induct_var_acc = false;
  base_info->multi_induct = false;
  base_info->complicated = false;
  base_info->acc_base = NULL;
  base_info->induct_base = NULL;
  std::set<tree> already;
  an_ac_helper ( access, 4, &already, acc_info);
}


// create_new_types has to crawl "all" the
// types, create new types and transform
// other types that must be changed.
// A type will change when it's a
// a pointer to a ReorgType or it contains
// an interior pointer to one. 
static void
create_new_types ( Info_t *info)
{
  std::map < tree, BoolPair_t>::iterator tmi;
  for( tmi = info->struct_types->begin ();
       tmi != info->struct_types->end ();
       tmi++ ) {
    if ( !tmi->second.processed ) create_a_new_type ( info, tmi->first);
  }    
}

static void
create_a_new_type ( Info_t *info, tree type)
{
  bool layout_changed = false;
  // skip if already processed  		   
  if ( ( *( info->struct_types))[type].processed ) return;

  // Implementation note: Check this for infinite recursion.
  // I don't think it's possible in a sane universe but
  // pointers to reorganized types can occur, so does that
  // an issue (not necessarily here.)
  // Also, is this even necessary? Singletons don't expand
  // and static arrays are not allowed "yet."
  tree field;
  tree new_fields = NULL;
  for ( field = TYPE_FIELDS ( type); // ??? I speced reorg_type_prime here???
        field; 
        field = DECL_CHAIN ( field))
    {
      // make sure all the interior types are processed
      // before processing this type
      if ( TREE_CODE ( field) == RECORD_TYPE )
	{
	  create_a_new_type ( info, field);
	}
    }
	 
  ReorgType_t *ri = get_reorgtype_info ( type, info);
  if ( ri != NULL ) {
    // Create the new record type of the reorg type
    tree reorg_type_prime = lang_hooks.types.make_type (RECORD_TYPE);

    ri->reorg_ver_type = reorg_type_prime;
    //DEBUG_L("TYPE_SIZE(reorg_type_prime): %p, ", TYPE_SIZE(reorg_type_prime));
    //DEBUG_F( print_generic_expr, stderr, TYPE_SIZE(reorg_type_prime), (dump_flags_t)-1);
    //DEBUG("\n");
    
    /* Multi-pool only
    // Create pointer_rep
    // this will be a long and a pointer to the 
    // reorg_type_prime
    tree pointer_rep = 
    lang_hooks.types.make_type( RECORD_TYPE);
    
    tree index_name = get_identifier("index");
    tree index_field = build_decl( BUILTINS_LOCATION, 
    FIELD_DECL, 
    index_name, 
    long_integer_type_node);
    tree base_name = get_identifier("base");
    tree base_field = build_decl( BUILTINS_LOCATION, 
    FIELD_DECL, 
    base_name, 
    reorg_type_prime);
    insert_field_into_struct( pointer_rep, index_field);
    insert_field_into_struct( pointer_rep, base);
    
    reorg_type->pointer_rep = pointer_rep;
    */

    tree pointer_rep = make_signed_type ( TYPE_PRECISION ( pointer_sized_int_node));
    TYPE_MAIN_VARIANT ( pointer_rep) = TYPE_MAIN_VARIANT ( pointer_sized_int_node);
    //DEBUG("Issue with gcc_ of reorg\n");
    //DEBUG_F(print_reorg, stderr, 2, ri);
    const char *gcc_name =
      identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( ri->gcc_type)));
    size_t len =
      strlen ( REORG_SP_PTR_PREFIX) + strlen ( gcc_name);
    char *name = ( char *)alloca(len + 1);
    strcpy ( name, REORG_SP_PTR_PREFIX);
    strcat ( name, gcc_name);
    TYPE_NAME ( pointer_rep) = get_identifier ( name);
    ri->pointer_rep = pointer_rep;
    //DEBUG_L("pointer_rep = ");
    //DEBUG_F( print_generic_expr, stderr, pointer_rep, (dump_flags_t)-1);
    //DEBUG("\n");
    //DEBUG_A("TYPE_MAIN_VARIANT ( pointer_rep) = ");
    //DEBUG_F( print_generic_expr, stderr, TYPE_MAIN_VARIANT ( pointer_rep), (dump_flags_t)-1);
    //DEBUG("\n");

    // Note, we also declare a base type variable (globally.)
    // This variable also belong in the ReorgType.
    
    // Set name of reorg_type_prime
    const char *base_type_name =
      identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( ri->gcc_type)));
    len = strlen ( REORG_SP_PREFIX) + strlen ( base_type_name);
    char *rec_name = ( char*)alloca ( len + 1);
    strcpy ( rec_name, REORG_SP_PREFIX);
    strcat ( rec_name, base_type_name);
    
    //DEBUG_L("TYPE_SIZE(reorg_type_prime): %p\n", TYPE_SIZE(reorg_type_prime));
    
    // Build the new pointer type fields
    TYPE_NAME ( reorg_type_prime) = get_identifier ( rec_name);
    tree field;
    tree new_fields = NULL;
    for ( field = TYPE_FIELDS ( type); field; field = DECL_CHAIN ( field))
      {
	//DEBUG_F( print_generic_decl, stderr, field, TDF_DETAILS); // example
	tree tree_type = TREE_TYPE ( field);
	tree new_fld_type = build_pointer_type ( tree_type);
	tree new_decl =
	  build_decl ( DECL_SOURCE_LOCATION (field),
		       FIELD_DECL, DECL_NAME (field), new_fld_type);
	DECL_CONTEXT ( new_decl) = reorg_type_prime;
	layout_decl ( new_decl, 0);
	
	// We might be missing a bunch of attributes (see
	// tree-nested.c:899) But we seem without without them!
	
	DECL_CHAIN ( new_decl) = new_fields; // <- bug: need decl, not type
	new_fields = new_decl;
	//DEBUG( "built new pointer type field:");
	//DEBUG_F( print_generic_decl, stderr, new_decl, TDF_DETAILS);
	//DEBUG( "\n");
      }

    //DEBUG_L("TYPE_SIZE(reorg_type_prime): %p\n", TYPE_SIZE(reorg_type_prime));
    
    // store reversed fields into reorg_type_prime
    TYPE_FIELDS ( reorg_type_prime) = NULL;
    tree next_fld;
    for ( field = new_fields;
	  field; 
	  field = next_fld    )
      {
	next_fld = DECL_CHAIN ( field);
	DECL_CHAIN ( field) = TYPE_FIELDS ( reorg_type_prime);
	TYPE_FIELDS ( reorg_type_prime) = field;
      }
    //DEBUG_L("TYPE_SIZE(reorg_type_prime): %p\n", TYPE_SIZE(reorg_type_prime));
    // Fix-up the layout
    layout_type ( reorg_type_prime);

    // HERE
    // Create the base element for a reorg type. This is for the single
    // pool case only.
    tree base_var =
      build_decl ( UNKNOWN_LOCATION, VAR_DECL, NULL_TREE, ri->reorg_ver_type);
    // We don't want to manually set DECL_INITIAL here!
    
    const char *type_name =
      identifier_to_locale ( IDENTIFIER_POINTER ( TYPE_NAME ( ri->gcc_type)));
    size_t tlen = strlen ( REORG_SP_BASE_PREFIX) + strlen ( type_name);
    char *base_name = ( char*)alloca ( tlen + 1);
    strcpy ( base_name, REORG_SP_BASE_PREFIX);
    //DECL_NAME ( base_var) = get_identifier ( base_name);
    
    strcat ( base_name, type_name);
    
    DECL_NAME ( base_var) = get_identifier ( base_name); // wrong spot above???
    
    TREE_STATIC ( base_var) = 1;
    TREE_ADDRESSABLE  ( base_var) = 1;
    DECL_NONALIASED ( base_var) = 1;
    SET_DECL_ALIGN ( base_var, TYPE_ALIGN ( ri->reorg_ver_type));
    
    varpool_node::finalize_decl ( base_var);

    relayout_decl ( base_var);
    
    ri->instance_interleave.base = base_var;
  }

  // Mess with the original type too because it might
  // have base_type_fldinterior elements that are modified.
  for ( field = TYPE_FIELDS ( type); 
       field; 
       field = DECL_CHAIN ( field))
    {
      if ( TREE_CODE ( field) == RECORD_TYPE )
	{
	  layout_changed =
	    layout_changed || ( *( info->struct_types)) [ field].layout_changed;
	}
      else
	{
	  // process pointers to reorg types
	  if ( POINTER_TYPE_P ( field) )
	    {
	      tree field_type = TREE_TYPE ( field);
	      if ( is_reorg_type ( field_type, info) )
		{
		  // Change field type.
		  
		  // If multi-pool then set layout_changed to true.
		  
		  // The type pointed to changes for single-pool.
		  ReorgType_t *ri =
		    get_reorgtype_info ( field_type, info);
		  gcc_assert ( ri->pointer_rep);
		  TREE_TYPE ( field) = ri->pointer_rep;
		}
	      tree base = base_type_of ( field);
	      if ( is_reorg_type ( base, info) )
		{
		  // strip off a layer of pointers
		  gcc_assert ( TREE_TYPE ( TREE_TYPE( field)));
		  TREE_TYPE ( field) = TREE_TYPE ( TREE_TYPE( field));
		}
	    }
	}
    }

  // Mark the type as processed
  ( *( info->struct_types)) [ type] = { true, layout_changed};
}

static tree
find_coresponding_field ( tree base_decl, tree field)
{
  tree reorg_field;
  for ( reorg_field = TYPE_FIELDS ( TREE_TYPE ( base_decl)); 
	 reorg_field; 
	 reorg_field = DECL_CHAIN ( reorg_field))
      {
	const char *reorg_field_name =
	  lang_hooks.decl_printable_name ( reorg_field, 2);
	const char *field_name =
	  lang_hooks.decl_printable_name ( field, 2);
	//DEBUG_L("LOOK %s, %s\n", reorg_field_name, field_name);
	
	if ( strcmp ( reorg_field_name, field_name) == 0 )
	  {
	    gcc_assert ( TREE_TYPE( field) == TREE_TYPE( TREE_TYPE(reorg_field)));
	    return reorg_field;
	  }
      }
  internal_error ( "find_coresponding_field: found no field");
}

static void
remove_default_def ( tree default_def, struct function *func)
{
  size_t i;
  tree ssa_name;
  FOR_EACH_SSA_NAME ( i, ssa_name, func)
    {
      if ( default_def == ssa_name )
	{
	  SSANAMES ( func)->unordered_remove ( i);
	  return;
	}
    }
}

static basic_block
make_bb ( char *msg, basic_block prev_bb )
{
  basic_block ret = create_empty_bb ( prev_bb);
  //DEBUG_A( "make_bb ( %s, <bb %d>/%p  ): <bb %d>/%p, prev: <bb %d>/%p, next: <bb %d>/%p\n",
  //	   msg, prev_bb->index, prev_bb,
  //	   ret->index, ret,
  //	   ret->prev_bb->index, ret->prev_bb,
  //	   ret->next_bb->index, ret->next_bb);
  return ret;
}
