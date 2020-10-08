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
#include "expr-rewriter.hpp"
#include "type-stringifier.hpp"
#include <string>
#include <map>

void
ExprTypeRewriter::_walk_PARM_DECL_post(const_tree t)
{
  tree temp = (tree)(t);
  tree ttemp = TREE_TYPE(temp);
  const bool is_interesting = is_interesting_type(ttemp);
  if (!is_interesting) return;
  relayout_decl(temp);
}

void
ExprTypeRewriter::_walk_FUNCTION_DECL_post(const_tree t)
{
  tree fn_type = TREE_TYPE(t);
  gcc_assert(t);
  tree ret_type = TREE_TYPE(fn_type);
  if (!ret_type) return;

  TypeStringifier stringifier;
  std::string name = stringifier.stringify(ret_type);
  // WARNING: You cannot use is interesting here because you haven't
  // changed the return type
  // This is because the return type is not an expression.
  // Therefore it is awkward to do this in the expr-walker...
  // const bool is_interesting = is_interesting_type(ret_type);
  const bool is_interesting = _map.find(ret_type) != _map.end();
  if (!is_interesting) return;

  tree r_t = _map[ret_type];
  TREE_TYPE(fn_type) = r_t;
}

void
ExprTypeRewriter::_walk_MEM_REF_post(const_tree e)
{
  // The second operand is a pointer constant. 
  // Its type specifying the type used for type based alias analysis
  tree op1 = TREE_OPERAND(e, 1);
  gcc_assert(TREE_CODE(op1) == INTEGER_CST);

  tree t = TREE_TYPE(op1);
  const bool already_rewritten = is_interesting_type(t);

  // This is where we do the transformation
  if (!already_rewritten) return;

  const_tree old_type = _imap[t];
  assert_is_type(old_type, POINTER_TYPE);
  const_tree old_base_type = TREE_TYPE(old_type);
  tree old_type_size_tree = TYPE_SIZE_UNIT(old_base_type);
  int old_type_size_int = tree_to_shwi(old_type_size_tree);

  tree reorg_type = t;
  assert_is_type(reorg_type, POINTER_TYPE);
  tree reorg_base_type = TREE_TYPE(reorg_type);
  tree reorg_type_size_tree = TYPE_SIZE_UNIT(reorg_base_type);
  int reorg_type_size_int = tree_to_shwi(reorg_type_size_tree);

  // Let's find out what is the previous offset
  int old_offset = tree_to_uhwi(op1);
  int remainder = old_offset % old_type_size_int;

  int new_offset = old_offset / old_type_size_int * reorg_type_size_int + remainder;

  tree new_offset_tree = build_int_cst(TREE_TYPE(op1), new_offset);
  TREE_OPERAND(e, 1) = new_offset_tree;
}

void
ExprTypeRewriter::_walk_SSA_NAME_post(const_tree t)
{
  // Here, we need to find out 
  log("we are in expr-rewriter SSA_NAME_post\n");
  TypeStringifier stringifier;
  std::string name = stringifier.stringify(TREE_TYPE(t));
  log("%s\n", name.c_str());
}

//TODO:
//Change name of this method...
bool
ExprTypeRewriter::is_interesting_type(tree t)
{
  const bool in_imap = _imap.find(t) != _imap.end();
  bool interesting = in_imap;
  if (!interesting) return false;

  tree possibly_copy = (tree)_imap[t];
  const bool is_copy = possibly_copy == t;
  interesting = !is_copy;
  if (!interesting) return false;

  // Let's just do a quick sanity check
  tree interesting_type = t;
  const bool has_valid_suffix = strstr(TypeStringifier::get_type_identifier(interesting_type).c_str(), ".reorg");
  gcc_assert(has_valid_suffix);
  return true;
}

void
ExprTypeRewriter::handle_pointer_arithmetic_diff(gimple *s, tree op_0, tree op_1)
{
  
  // lhs = op0 - op1 // <-- we are here
  // ... SNIP ...
  // var = lhs / [ex] old_struct_size // <-- we want to be here
  //
  // Let's explore the uses of lhs
  tree lhs = gimple_assign_lhs(s);

  tree reorg_type = TREE_TYPE(op_0);
  const enum tree_code code = TREE_CODE(reorg_type);
  const bool is_pointer = POINTER_TYPE == code;
  const bool is_array = ARRAY_TYPE == code;
  const bool is_valid_input = is_pointer != is_array;
  gcc_assert(is_valid_input);

  tree inner_reorg_type = TREE_TYPE(reorg_type);
  gcc_assert(inner_reorg_type);
  tree reorg_type_size_tree = TYPE_SIZE_UNIT(inner_reorg_type);
  int reorg_type_size_int = tree_to_shwi(reorg_type_size_tree);

  tree old_type = (tree)_imap[reorg_type];
  tree inner_old_type = TREE_TYPE(old_type);
  gcc_assert(old_type);
  tree old_type_size_tree = TYPE_SIZE_UNIT(inner_old_type);
  int old_type_size_int = tree_to_shwi(old_type_size_tree);


  gimple *stmt;
  imm_use_iterator iterator;
  FOR_EACH_IMM_USE_STMT(stmt, iterator, lhs)
  {
      // stmt is a use of lhs
      // gimple_expr_code is only valid for non-debug statements
      bool is_debug = is_gimple_debug (stmt);
      if (is_debug)
	continue;

      enum tree_code code = gimple_expr_code (stmt);
      bool is_exact_div = code == EXACT_DIV_EXPR;
      if (!is_exact_div)
	continue;

      tree divisor = gimple_op (stmt, 2);
      enum tree_code divisor_code = TREE_CODE (divisor);
      bool is_constant = divisor_code == INTEGER_CST;
      if (!is_constant)
	continue;

      int divisor_int = tree_to_shwi (divisor);
      bool is_same_size = divisor_int == old_type_size_int;
      if (!is_same_size)
	continue;

      tree new_integer_cst_tree = build_int_cst(TREE_TYPE(divisor), reorg_type_size_int);
      gimple_set_op (stmt, 2, new_integer_cst_tree);
  }
}

void
ExprTypeRewriter::handle_pointer_arithmetic_nonconstant(gimple *s, tree op_0, tree op_1, bool is_pointer_plus)
{
  if (!is_pointer_plus)
  {
    handle_pointer_arithmetic_diff(s, op_0, op_1);
    return;
  }
  //   _1 = _0 * 72
  //   ... SNIP ...
  //   _2 = _1 + CONSTANT;
  //   ... SNIP ...
  //   _3 = &array + _2;  < -- this is where we are
  //enum tree_code code = TREE_CODE(op_1);
  //assert_is_type(op_1, SSA_NAME);
  tree new_type = TREE_TYPE(gimple_assign_lhs(s));


  gimple *def_for_variable = SSA_NAME_DEF_STMT(op_1);
  // It is possible that we are in a negation statement...
  // Example:
  //   _2 = _1 * 72;
  //   ... SNIP ...
  //   _3 = -_2;  < -- def_for_variable **might** be this stmt.
  //   ... SNIP ...
  //   _4 = &array + _3;
  // Let's find out how many operands we have
  unsigned num_operands = gimple_num_ops(def_for_variable);
  // Here operands is kind of a minomer.
  // operand 0 is the lhs
  // operand 1 is the rhs
  // I.e. lhs = (unary_operator) rhs;
  bool get_another_definition = num_operands == 2;
  tree possibly_not_needed = get_another_definition ? gimple_op (def_for_variable, 1) : NULL;
  def_for_variable = get_another_definition ? SSA_NAME_DEF_STMT(possibly_not_needed) : def_for_variable;

  // Example:
  //   _2 = _1 * 72; <-- Now we are here...
  //   ... SNIP ...
  //   _3 = -_2; 
  //   ... SNIP ...
  //   _4 = &array + _3;

  enum gimple_code gcode = gimple_code(def_for_variable);
  switch (gcode)
  {
  //TODO: FIXME:
  //This is unsafe, waiting for the sizeof solution
	  case GIMPLE_COND:
	  case GIMPLE_CALL:
	  case GIMPLE_ASSIGN:
          break;
	  default:
	  return;
	  break;
  }
  enum tree_code code = gimple_expr_code (def_for_variable);
  const bool is_plus_expr = PLUS_EXPR == code;

  // op_0 is the variable
  // That means that the reorg_type is
  // The truth is that op_0 might not have the correct type
  tree reorg_type_tree = new_type;
  tree reorg_inner_type = TREE_TYPE(reorg_type_tree);
  tree reorg_type_size_tree = TYPE_SIZE_UNIT(reorg_inner_type);
  int reorg_type_size_int = tree_to_shwi(reorg_type_size_tree);
  // That means that the old type is 
  tree old_type_tree = (tree)_imap[reorg_type_tree];
  tree old_inner_type = TREE_TYPE(old_type_tree);
  tree old_type_size_tree = TYPE_SIZE_UNIT(old_inner_type);
  int old_type_size_int = tree_to_shwi(old_type_size_tree);

  if (is_plus_expr)
  {
      // If we are here it is because we are adding an offset.
      // It is usually whenever we do somehting like
      //   _2 = _1 + CONSTANT; <-- to change
      //   _3 = &array + _2;
      tree constant_plus = gimple_op (def_for_variable, 2);
      assert_is_type(constant_plus, INTEGER_CST);

      int old_integer_cst_int = tree_to_uhwi(constant_plus);
      int modulo = old_integer_cst_int % old_type_size_int;
      int new_integer_cst_int = old_integer_cst_int / old_type_size_int * reorg_type_size_int + modulo;
      
      tree new_integer_cst_tree = build_int_cst(TREE_TYPE(constant_plus), new_integer_cst_int);
      gimple_set_op(def_for_variable, 2, new_integer_cst_tree);

      tree variable = gimple_op (def_for_variable, 1);
      def_for_variable = SSA_NAME_DEF_STMT(variable);
      num_operands = gimple_num_ops (def_for_variable);
      get_another_definition = num_operands == 2;
      def_for_variable = get_another_definition ? SSA_NAME_DEF_STMT(gimple_op(def_for_variable, 1)) : def_for_variable;
      code = gimple_expr_code(def_for_variable);


  }

  if (code == MULT_EXPR) {

  tree op_0_earlier = gimple_assign_rhs1(def_for_variable);
  tree op_1_earlier = gimple_assign_rhs2(def_for_variable);

  // We should be able to just call the constant implementation
  //handle_pointer_arithmetic_constants(def_for_variable, op_0, op_1);
  //However...
  //these variables no longer hold the type needed for them to change correctly
  //so, let's do it from here...

  assert_is_type(op_1_earlier, INTEGER_CST);


  tree old_integer_cst_tree = op_1_earlier;
  int old_integer_cst_int = tree_to_uhwi(old_integer_cst_tree);

  int offset = old_integer_cst_int % old_type_size_int ;
  int new_integer_cst_int = old_integer_cst_int / old_type_size_int * reorg_type_size_int + offset;
  log("%d  = %d / %d * %d + %d\n", new_integer_cst_int, old_integer_cst_int, old_type_size_int, reorg_type_size_int, offset);

  tree new_integer_cst_tree = build_int_cst(TREE_TYPE(old_integer_cst_tree), new_integer_cst_int);
  gimple_set_op(def_for_variable, 2, new_integer_cst_tree);
  }
}

void
ExprTypeRewriter::handle_pointer_arithmetic_constants(gimple *s, tree p, tree i, bool is_pointer_plus)
{
  // So, because we have already changed the type
  // tree p will either be the original type
  // if we do not need to modify this expression
  // How do we know if we have an original type?
  // It is when we don't have a type in our map
  tree possibly_reorged_type = TREE_TYPE(p);
  bool is_interesting_case = is_interesting_type(possibly_reorged_type);
  if (!is_interesting_case) return;

  tree reorg_type = possibly_reorged_type;       // this is the type of the variable
  const_tree original_type = _imap[reorg_type];
  // If we are here, that means that our type has the ".reorg" suffix
  const bool has_suffix = strstr(TypeStringifier::get_type_identifier(reorg_type).c_str(), ".reorg");
  bool is_valid_input = has_suffix;
  gcc_assert(is_valid_input);

  // We need to know what size is the previous original type
  tree inner_reorg_type = TREE_TYPE(reorg_type);
  tree inner_orig_type = TREE_TYPE(original_type);
  tree old_size_tree = TYPE_SIZE_UNIT(inner_orig_type);
  int old_size_int = tree_to_shwi(old_size_tree);
  tree new_size_tree = TYPE_SIZE_UNIT(inner_reorg_type);
  int new_size_int = tree_to_shwi(new_size_tree);
  tree old_integer_cst_tree = i;
  int old_integer_cst_int = tree_to_uhwi(old_integer_cst_tree);

  int offset = old_integer_cst_int % old_size_int;
  const bool is_modulo = offset == 0;
  is_valid_input = is_modulo;
  gcc_assert(is_valid_input);

  int new_integer_cst_int = old_integer_cst_int / old_size_int * new_size_int + offset;
  log("%d  = %d / %d * %d\n", new_integer_cst_int, old_integer_cst_int, old_size_int, new_size_int);

  tree new_integer_cst_tree = build_int_cst(TREE_TYPE(old_integer_cst_tree), new_integer_cst_int);
  gimple_set_op(s, 2, new_integer_cst_tree);


}

void
ExprTypeRewriter::_walk_post(const_tree e)
{
  gcc_assert(e);
  tree t = TREE_TYPE(e);
  const bool in_map = _map.find(t) != _map.end();
  if (!in_map) return;

  const enum tree_code code = TREE_CODE(e);
  tree r_t = _map[t];
  TREE_TYPE((tree)e) = r_t;

  return;
  if (code != MEM_REF) return;

  TypeStringifier stringifier;
  std::string name = stringifier.stringify(r_t);
  tree m = TYPE_MAIN_VARIANT(r_t);
  std::string name_m = stringifier.stringify(m);
  const bool main_variant = TYPE_MAIN_VARIANT(r_t) == r_t;
  log("main: %s\n", name_m.c_str());
  log("we are in memref: %s is_main_variant %s\n", name.c_str(), main_variant ? "t" : "f");
  tree type_size = TYPE_SIZE(r_t);
  log("type size 1 %s?", type_size ? "t" : "f");
  if (!type_size) TYPE_SIZE(r_t) = TYPE_SIZE(m);
  log("type size 2 %s?", TYPE_SIZE(r_t) ? "t" : "f");
  if (!TYPE_SIZE(r_t)) layout_type(r_t);
  log("type size 3 %s?", TYPE_SIZE(r_t) ? "t" : "f");
  // still no type_size
  gcc_assert(TYPE_SIZE(r_t));
  const enum tree_code cc = TREE_CODE(TYPE_SIZE(r_t));
  log("%s\n", get_tree_code_name(cc));

  //tree type_main_variant = TYPE_MAIN_VARIANT(TREE_TYPE(e));
  //const bool do_we_have_mv_in_map = _map.find(type_main_variant) != _map.end();

  // TODO: Fix this hack
  // We need to make sure that the type main variant is already good here...
  //TYPE_MAIN_VARIANT(TREE_TYPE(e)) = do_we_have_mv_in_map ? _map[type_main_variant] : TYPE_MAIN_VARIANT(TREE_TYPE(e));

}

void
ExprTypeRewriter::_walk_COMPONENT_REF_post(const_tree e)
{

  const_tree r = TREE_OPERAND(e, 0);
  tree record_type = TREE_TYPE(r);
  const bool in_map1 = _map.find(record_type) != _map.end();

  const_tree f = TREE_OPERAND(e, 1);
  // So, what we need is a map between this field and the new field
  const bool in_map = _map2.find(f) != _map2.end();
  if (!in_map) return;

  auto p = _map2[f];
  tree n_f = p.first;
  bool is_deleted = p.second;

  unsigned f_byte_offset = tree_to_uhwi(DECL_FIELD_OFFSET(f));
  unsigned f_bit_offset = tree_to_uhwi(DECL_FIELD_BIT_OFFSET(f));
  unsigned f_offset = 8 * f_byte_offset + f_bit_offset;

  unsigned nf_byte_offset = tree_to_uhwi(DECL_FIELD_OFFSET(n_f));
  unsigned nf_bit_offset = tree_to_uhwi(DECL_FIELD_BIT_OFFSET(n_f));
  unsigned nf_offset = 8 * nf_byte_offset + nf_bit_offset;
  TREE_OPERAND(e, 1) = n_f;

  if (!is_deleted) return;

  _delete = true;

}


