/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

#include <stdint.h>
union a
{
  int8_t b
} c () { union a d = {4073709551608}; }
main () {}
