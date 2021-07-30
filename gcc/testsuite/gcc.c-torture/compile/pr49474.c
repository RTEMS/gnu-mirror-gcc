typedef struct gfc_formal_arglist
{
  int next;
}
gfc_actual_arglist;
update_arglist_pass (gfc_actual_arglist* lst, int po, unsigned argpos,
       const char *name)
{
  ((void)(__builtin_expect(!(argpos > 0), 0) ? __builtin_unreachable(), 0 : 0));
  if (argpos == 1)
      return 0;
  if (lst)
#ifdef __GCC_ARM_CAPABILITY_ANY
    lst->next = update_arglist_pass ((__intcap_t)lst->next, po, argpos - 1, name);
#else
    lst->next = update_arglist_pass (lst->next, po, argpos - 1, name);
#endif
  else
#ifdef __GCC_ARM_CAPABILITY_ANY
    lst = (__intcap_t)update_arglist_pass (((void *)0), po, argpos - 1, name);
#else
    lst = update_arglist_pass (((void *)0), po, argpos - 1, name);
#endif
}
