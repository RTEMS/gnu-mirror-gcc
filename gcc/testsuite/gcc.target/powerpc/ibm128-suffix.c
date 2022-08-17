/* { dg-do compile } */
/* { dg-require-effective-target longdouble128 } */
/* { dg-options "-O2" } */

/* See if the 'w' suffix is accepted for __ibm128.  */
#ifndef NUMBER
#define NUMBER  123456789012345678901234567890123456789E-10
#endif

#define GLUE2(X,Y)      X ## Y
#define GLUE(X,Y)       GLUE2(X,Y)

__ibm128 x = GLUE (NUMBER, w);
