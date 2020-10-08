#include "gimple-caster.hpp"
#include "gimple-pretty-print.h"

#include "type-incomplete-equality.hpp"
#include "type-stringifier.hpp"


void
GimpleCaster::_walk_pre(gassign *s)
{
  const enum gimple_rhs_class code = gimple_assign_rhs_class(s);
  const bool valid_input = GIMPLE_SINGLE_RHS == code;
  if (!valid_input) return;

  // I originally was using gimple_assign_cast_p
  // but that proved to be insufficient...
  // So we have to use our equality comparison...
  TypeIncompleteEquality equality;
  const_tree lhs = gimple_assign_lhs(s);
  const_tree rhs = gimple_assign_rhs1(s);
  gcc_assert(lhs && rhs);
  Reason reason {};
  const_tree t_lhs = TREE_TYPE(lhs);
  const_tree t_rhs = TREE_TYPE(rhs);
  gcc_assert(t_lhs && t_rhs);
  bool is_cast = !equality.equal(t_lhs, t_rhs);
  TypeStringifier stringifier;
  const std::string name_l = stringifier.stringify(t_lhs);
  const std::string name_r = stringifier.stringify(t_rhs);
  // If it is cast, we might need to look at the definition of rhs
  // If the definition comes from a known function... then we are good...
  bool is_ssa = TREE_CODE(rhs) == SSA_NAME;
  while (is_ssa) {
    gimple *def_for_rhs = SSA_NAME_DEF_STMT(rhs);
    gcall *is_call = dyn_cast<gcall*>(def_for_rhs);
    // poor man's goto
    if (!is_call) break;

    const_tree fn = gimple_call_fndecl(is_call);
    // poor man's goto
    if (!fn) break;

    bool known_function = GimpleEscaper::filter_known_function(fn);
    is_cast = !known_function;

    is_ssa = false;
  }
  reason.type_is_casted = is_cast;
  exprEscaper.update(lhs, reason);
  exprEscaper.update(rhs, reason);
  // TODO: 
  // I think this will re-do the work... But it might be necessary?
  GimpleEscaper::_walk_pre(s);
}

void
GimpleCaster::_walk_pre(gcall *s)
{
  GimpleEscaper::_walk_pre(s);

  const_tree fn = gimple_call_fndecl(s);
  // If there's no function declaration, how do we
  // know the argument types?
  if (!fn) return;

  cgraph_node *node = cgraph_node::get(fn);
  const bool known_function = GimpleEscaper::filter_known_function(node) || GimpleEscaper::filter_known_function(fn);
  if (known_function) return;

  const_tree f_t = TREE_TYPE(fn);
  TypeIncompleteEquality equality;
  TypeStringifier stringifier;

  unsigned i = 0;
  unsigned n = gimple_call_num_args(s);
  for (tree a = TYPE_ARG_TYPES(f_t); NULL_TREE != a; a = TREE_CHAIN(a))
  {
    const_tree formal_t = TREE_VALUE(a);
    // There seems to be a final VOID_TYPE at the end of some functions?
    const enum tree_code code = TREE_CODE(formal_t);
    const bool is_void = VOID_TYPE == code;
    if (is_void) continue;

    const_tree real = gimple_call_arg(s, i);
    const_tree real_t = TREE_TYPE(real);
    const bool is_casted = !equality.equal(formal_t, real_t);
    const std::string name_r = stringifier.stringify(real_t);
    const std::string name_f = stringifier.stringify(formal_t);
    Reason arg_reason;
    arg_reason.type_is_casted = is_casted;
    exprEscaper.update(real, arg_reason);
    i++;
  }

  /*
  unsigned n = gimple_call_num_args(s);
  for (unsigned i = 0; i < n; i++)
  {
    const_tree a = gimple_call_arg(s, i);
    gcc_assert(a);
    exprEscaper.update(a, reason);
  }
  */

  const_tree lhs = gimple_call_lhs(s);
  if (!lhs) return;

  const_tree r_t = TREE_TYPE(f_t);
  const_tree l_t TREE_TYPE(lhs);
  const bool is_casted = !equality.equal(r_t, l_t);
  const std::string name_r_t = stringifier.stringify(r_t);
  const std::string name_l_t = stringifier.stringify(r_t);
  Reason ret_reason;
  ret_reason.type_is_casted = is_casted;
  exprEscaper.update(lhs, ret_reason);
}
