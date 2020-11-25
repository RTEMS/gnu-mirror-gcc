/* { dg-do run } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis -Wno-dfa " } */

#include <assert.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>

int
main (int argc, char **argv)
{
  struct astruct_s
  {
    _Bool a;
    _Bool delete_me;
    _Bool c;
  };
  struct astruct_s astruct;
  printf ("%d %d", astruct.a, astruct.c);
  astruct.delete_me = false;
  return 0;
}

/// { dg-final { scan-ipa-dump "deleting field delete_me 8" "type-escape-analysis" } } */
