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
#include "gimple-rewriter.hpp"
#include "type-stringifier.hpp"

void
GimpleTypeRewriter::_walk_pre(const_tree e)
{
  // This is for local variables
  // and other declarations
  exprTypeRewriter.walk(e);
  bool _delete = exprTypeRewriter._delete;
  exprTypeRewriter._delete = false;
  // I don't think it is possible here (local variable delcarations and such);
  gcc_assert(!_delete);
  const bool is_interesting = exprTypeRewriter.is_interesting_type(TREE_TYPE(e));

  /*
  const bool is_ssa_name = TREE_CODE(e) == SSA_NAME;
  if (is_ssa_name)
  {
    tree l = (tree) e;
    if (SSA_NAME_VAR(l) == NULL_TREE) return;
    if (TREE_TYPE(l) != TREE_TYPE(SSA_NAME_VAR(l))) { log("unequal\n"); }

    TREE_TYPE(l) = TREE_TYPE(SSA_NAME_VAR(l));
    TypeStringifier stringifier;
    std::string name = stringifier.stringify(TREE_TYPE(l));
    log("new name %s\n", name.c_str());
  }
  */

  const bool is_var_decl = TREE_CODE(e) == VAR_DECL;
  const bool is_valid = is_interesting && is_var_decl;
  if (!is_valid) return;
  relayout_decl((tree)e);
}

void
GimpleTypeRewriter::_walk_pre(gimple *s)
{
}

void
GimpleTypeRewriter::_walk_pre(gcall *s)
{
}

void
GimpleTypeRewriter::_walk_pre(greturn *s)
{
  const_tree val = gimple_return_retval(s);
  if (!val) return;
  log("rewriting a return value\n");
  exprTypeRewriter.walk(val);
  bool _delete = exprTypeRewriter._delete;
  exprTypeRewriter._delete = false;
  // We can't probably have a write in a return statement.
  gcc_assert(!_delete);
}

void
GimpleTypeRewriter::handle_pointer_arithmetic(gimple *s)
{
  const enum tree_code p = POINTER_PLUS_EXPR;
  const enum tree_code d = POINTER_DIFF_EXPR;
  const enum tree_code e = gimple_expr_code(s);
  const bool is_pointer_plus = p == e;
  const bool is_pointer_diff = d == e;
  bool is_valid_input = is_pointer_plus != is_pointer_diff;
  gcc_assert(is_valid_input);
  // TODO: Implement pointer diff
  
  const enum gimple_rhs_class rhs_class = gimple_assign_rhs_class(s);
  is_valid_input = GIMPLE_BINARY_RHS == rhs_class;
  gcc_assert(is_valid_input);

  tree op_0 = gimple_assign_rhs1(s);
  tree op_1 = gimple_assign_rhs2(s);
  tree lhs = gimple_assign_lhs(s);
  tree op_0_t = TREE_TYPE(op_0);
  tree op_1_t = TREE_TYPE(op_1);
  tree lhs_t = TREE_TYPE(lhs);
  const bool is_op_0_t_interesting = exprTypeRewriter.is_interesting_type(op_0_t);
  const bool is_op_1_t_interesting = exprTypeRewriter.is_interesting_type(op_1_t);
  const bool is_lhs_t_interesting = exprTypeRewriter.is_interesting_type(lhs_t);
  bool is_interesting_case = is_op_0_t_interesting || is_op_1_t_interesting || is_lhs_t_interesting;
  TypeStringifier stringifier;
  std::string name_0 = stringifier.stringify(op_0_t);
  std::string name_1 = stringifier.stringify(op_1_t);
  std::string name_l = stringifier.stringify(lhs_t);
  log("is interesting case %s\n", is_interesting_case ? "t" : "f");
  log("op_0 %s\n", name_0.c_str());
  log("op_1 %s\n", name_1.c_str());
  log("lhs_t%s\n", name_l.c_str());
  if (!is_interesting_case) return;

  const enum tree_code op_1_code = TREE_CODE(op_1);
  const enum tree_code op_0_code = TREE_CODE(op_0);
  const bool is_op_0_icst = INTEGER_CST == op_0_code;
  const bool is_op_1_icst = INTEGER_CST == op_1_code;
  const bool is_constant_case = is_op_0_icst != is_op_1_icst;
  if (!is_constant_case)
  {
    exprTypeRewriter.handle_pointer_arithmetic_nonconstant(s, op_0, op_1, is_pointer_plus);
    bool _delete = exprTypeRewriter._delete;
    exprTypeRewriter._delete = false;
    // probably no deletion in pointer arithmetic...
    gcc_assert(!_delete);
    return;
  }

  tree integer_constant = is_op_0_icst ? op_0 : op_1;
  tree maybe_pointer = is_op_0_icst ? op_1 : op_0;
  const_tree maybe_pointer_t = TREE_TYPE(maybe_pointer);
  assert_is_type(maybe_pointer_t, POINTER_TYPE);
  tree pointer_variable = maybe_pointer;

  exprTypeRewriter.handle_pointer_arithmetic_constants(s, pointer_variable, integer_constant, is_pointer_plus);
  bool _delete = exprTypeRewriter._delete;
  exprTypeRewriter._delete = false;
  // probably no deletion in pointer arithmetic
  gcc_assert(!_delete);
}


void
GimpleTypeRewriter::_walk_pre(gassign *s)
{
  const enum gimple_rhs_class code = gimple_assign_rhs_class(s);

  switch (code)
  {
    case GIMPLE_TERNARY_RHS:
    {
      const_tree rhs3 = gimple_assign_rhs3(s);
      exprTypeRewriter.walk(rhs3);
    }
    /* fall-through */
    case GIMPLE_BINARY_RHS:
    {
      const_tree rhs2 = gimple_assign_rhs2(s);
      exprTypeRewriter.walk(rhs2);
    }
    /* fall-through */
    case GIMPLE_UNARY_RHS:
    case GIMPLE_SINGLE_RHS:
    {
      const_tree rhs1 = gimple_assign_rhs1(s);
      exprTypeRewriter.walk(rhs1);
      const_tree lhs = gimple_assign_lhs(s);
      if (!lhs) break;
      // Here is the only place where we likely can delete a statement.
      exprTypeRewriter.walk(lhs);
      bool _delete = exprTypeRewriter._delete;
      exprTypeRewriter._delete = false;
      if (_delete)
      {
	 _deleted = true;
      }
    }
    break;
    default:
    gcc_unreachable();
    break;
  }


  const enum tree_code e_code = gimple_expr_code(s);
  log("is this the statment i'm looking for? %s\n", get_tree_code_name(e_code));
  print_gimple_stmt(dump_file, s, 0);
  log("\n");
  switch (e_code)
  {
    case POINTER_PLUS_EXPR:
    case POINTER_DIFF_EXPR:
	    handle_pointer_arithmetic(s);
    break;
    case COMPONENT_REF:
    {
      log("i am missing a component ref\n");
      print_gimple_stmt(dump_file, s, 0);
      log("\n");
      
      TypeStringifier stringifier;
      tree e = gimple_assign_rhs1(s);
      const_tree type = TREE_TYPE(e);
      std::string name = stringifier.stringify(type);
      log("%s\n", name.c_str());
    }
    break;
    case MULT_EXPR:
    {
      TypeStringifier stringifier;
      tree op1 = gimple_assign_rhs2(s);
      tree op2 = gimple_assign_rhs1(s);
      tree op1_t = TREE_TYPE(op1);
      tree op2_t = TREE_TYPE(op2);
      std::string op1_s = stringifier.stringify(op1_t);
      std::string op2_s = stringifier.stringify(op2_t);
      log("multiplication\n");
      log("%s * %s\n", op1_s.c_str(), op2_s.c_str());
    }
    break;
    default:
    {
      log("missing %s\n", get_tree_code_name(e_code));
    }
    break;
  }

}

void
GimpleTypeRewriter::_walk_pre(gcond *s)
{
}

void
GimpleTypeRewriter::_rewrite_function_decl()
{
  // NOTE: It seems we only need to rewrite the return type
  // for now...
  cgraph_node *node = NULL;
  FOR_EACH_FUNCTION_WITH_GIMPLE_BODY(node)
  {
    node->get_untransformed_body();
    tree fndecl = node->decl;
    gcc_assert(fndecl);
    exprTypeRewriter.walk(fndecl);
    tree decl = DECL_RESULT(fndecl);
    if (decl) exprTypeRewriter.walk(decl);
  }
}

void
GimpleTypeRewriter::_walk_pre(gphi *s)
{
  unsigned n = gimple_phi_num_args (s);
  for (unsigned i = 0; i < n; i++)
  {
    tree a = gimple_phi_arg_def(s, i);
    exprTypeRewriter.walk(a);
  }
}
