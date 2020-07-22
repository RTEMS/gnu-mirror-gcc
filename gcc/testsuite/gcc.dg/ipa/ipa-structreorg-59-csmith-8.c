/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

struct a
{
  signed b
};
struct
{
  struct a b
} volatile c;
main () { c.b.b; }

// we will do nothing because volatile
