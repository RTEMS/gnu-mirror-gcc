/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <stdint.h>
union a
{
  int16_t b
} c ()
{
  union a d;
  -d.b;
}
main () {}
