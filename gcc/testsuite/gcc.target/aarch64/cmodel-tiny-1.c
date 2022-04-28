/* { dg-do assemble } */
/* { dg-options "-O2 -mcmodel=tiny" } */

int x[5];
void f() { x[3] = 1; }
