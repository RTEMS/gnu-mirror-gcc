/* { dg-do compile } */
/* This would previously trigger an ICE in dwarf2out.c due to the earlier
   the creation of a paradoxical CADImode SUBREG of a DImode value.  This
   should now be supported.  */
int d (void) {
  volatile int x = 1;
  return x;
}

int main () {
  __uintcap_t b = (__uintcap_t) __builtin_return_address(0);
 volatile int i;
 for (i = 0; i < 4 ; i++)
   for ( ;; b && d())
    ;
  return 0;
}