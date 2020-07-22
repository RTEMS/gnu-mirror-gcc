/* { dg-do run } */
/* { dg-options  "-w -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis " } */

struct a
{
};
b (struct a c) {}
main () {}
