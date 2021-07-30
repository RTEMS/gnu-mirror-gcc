/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-dce2 -w" } */

/* The arithmetic using usecount should be gone.  */
/* { dg-final { scan-tree-dump-times "usecount_\[0-9\]+ = usecount_\[0-9\]+ . 1;" 0 "dce2"} } */

typedef union tree_node *tree;
typedef union gimple_statement_d *gimple;
extern const int tree_code_type[];
union tree_node
{
  int code:16;
};
typedef struct immediate_use_iterator_d
{
}
imm_use_iterator;
void
insert_debug_temp_for_var_def (gimple stmt)
{
  gimple def_stmt = ((void *) 0);
  int usecount = 0;
  tree value = ((void *) 0);
  for (; arf ();)
    {
      if (!gimple_debug_bind_p (stmt))
        continue;
      if (usecount++)
        break;
      unsigned char no_value = 0;
      if (!gimple_bb (def_stmt))
        no_value = 1;
      if (!no_value)
#ifdef __GCC_ARM_CAPABILITY_ANY
        value = (__intcap_t)gimple_assign_rhs_to_tree ();
#else
        value = gimple_assign_rhs_to_tree ();
#endif
    }
  if (value)
    {
      if ((tree_code_type[(int) (((value)->code))] == 42)
          || (usecount == 1 && (is_gimple_min_invariant (value))))
#ifdef __GCC_ARM_CAPABILITY_ANY
        value = (__intcap_t)unshare_expr (value);
#else
        value = unshare_expr (value);
#endif
    }
}


