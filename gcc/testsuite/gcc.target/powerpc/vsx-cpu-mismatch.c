/* { dg-do compile } */
/* { dg-require-effective-target powerpc_vsx } */
/* { dg-options "-O2 -mdejagnu-cpu=power5 -mvsx" } */
/* { dg-error "‘vsx’ needs at least ‘-mcpu=power7’" } */

/* Make sure -mcpu=power5 -mvsx gives an error if the cpu is not capable of VSX
   support.  */
int x;
