/* { dg-do compile } */
/* { dg-require-effective-target cheri_capability_hybrid } */
/* { dg-additional-options "-fdump-tree-original" } */

typedef int T;
struct S1 { int a; };
struct S2 { T a; };
struct S3 { char a; };

int * __capability f(int x1, struct S1 * __capability p1, struct S2 * __capability q1)
{
    return x1 ? &p1->a : &q1->a;
}
/* { dg-final { scan-tree-dump-times {return x1 != 0 \? &p1->a : \(int \* __capability\) &q1->a;} 1 "original" } } */

int * __capability warn1(int x2, struct S1 * __capability p2, struct S2 * q2)
{
  return x2 ? &p2->a : &q2->a; /* { dg-warning {converting non-capability type 'T \*' {aka 'int \*'} to capability type 'int \* __capability' without an explicit cast} } */
}
/* { dg-final { scan-tree-dump-times {return x2 != 0 \? &p2->a : \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) &q2->a\);} 1 "original" } } */

int * __capability warn2(int x3, struct S1 *p3, struct S2 * __capability q3)
{
  return x3 ? &p3->a : &q3->a; /* { dg-warning {converting non-capability type 'int \*' to capability type 'int \* __capability' without an explicit cast} } */
}
/* { dg-final { scan-tree-dump-times {return x3 != 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) &p3->a\) : \(int \* __capability\) } 1 "original" } } */

void * __capability mismatch1(int x4, int * __capability p4, char * __capability q4)
{
  return x4 ? p4 : q4; /* { dg-warning {pointer type mismatch in conditional expression} } */
}
/* { dg-final { scan-tree-dump-times {return x4 != 0 \? \(void \* __capability\) p4 : \(void \* __capability\) q4;} 1 "original" } } */

void * __capability mismatch2(int x5, int * __capability p5, char *q5)
{
  return x5 ? p5 : q5 ; /* { dg-warning {pointer type mismatch in conditional expression} } */
  /* { dg-warning {converting non-capability type 'char \*' to capability type * 'void \* __capability' without an explicit cast} "" { target *-*-*} .-1 } */
}
/* { dg-final { scan-tree-dump-times {return x5 == 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) q5\) : \(void \* __capability\) p5;} 1 "original" } } */

void * __capability mismatch3(int x6, int *p6, char * __capability q6)
{
  return x6 ? p6 : q6; /* { dg-warning {pointer type mismatch in conditional expression} } */
  /* { dg-warning {converting non-capability type 'int \*' to capability type * 'void \* __capability' without an explicit cast} "" { target *-*-*} .-1 } */
}
/* { dg-final { scan-tree-dump-times {return x6 != 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) p6\) : \(void \* __capability\) q6;} 1 "original" } } */

void * __capability mismatch4(int x7, struct S1 * __capability p7, struct S3 * __capability q7)
{
  return x7 ? &p7->a : &q7->a; /* { dg-warning {pointer type mismatch in conditional expression} } */
}
/* { dg-final { scan-tree-dump-times {return x7 != 0 \? \(void \* __capability\) &p7->a : \(void \* __capability\) &q7->a;} 1 "original" } } */

void * __capability mismatch5(int x8, struct S1 * __capability p8, struct S3 *q8)
{
  return x8 ? &p8->a : &q8->a; /* { dg-warning {pointer type mismatch in conditional expression} } */
  /* { dg-warning {converting non-capability type 'char \*' to capability type * 'void \* __capability' without an explicit cast} "" { target *-*-*} .-1 } */
}
/* { dg-final { scan-tree-dump-times {return x8 != 0 \? \(void \* __capability\) &p8->a : \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) &q8->a\);} 1 "original" } } */

void * __capability mismatch6(int x9, struct S1 *p9, struct S3 * __capability q9)
{
  return x9 ? &p9->a : &q9->a; /* { dg-warning {pointer type mismatch in conditional expression} } */
  /* { dg-warning {converting non-capability type 'int \*' to capability type * 'void \* __capability' without an explicit cast} "" { target *-*-*} .-1 } */
}
/* { dg-final { scan-tree-dump-times {return x9 != 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) &p9->a\) : \(void \* __capability\) &q9->a;} 1 "original" } } */

void * __capability mismatch7(int x10, struct S2 * __capability p10, struct S3 * __capability q10)
{
  return x10 ? &p10->a : &q10->a; /* { dg-warning {pointer type mismatch in conditional expression} } */
}
/* { dg-final { scan-tree-dump-times {return x10 != 0 \? \(void \* __capability\) &p10->a : \(void \* __capability\) &q10->a;} 1 "original" } } */

void * __capability mismatch8(int x11, struct S2 * __capability p11, struct S3 *q11)
{
  return x11 ? &p11->a : &q11->a; /* { dg-warning {pointer type mismatch in conditional expression} } */
  /* { dg-warning {converting non-capability type 'char \*' to capability type * 'void \* __capability' without an explicit cast} "" { target *-*-*} .-1 } */
}
/* { dg-final { scan-tree-dump-times {return x11 != 0 \? \(void \* __capability\) &p11->a : \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) &q11->a\);} 1 "original" } } */

void * __capability mismatch9(int x12, struct S2 *p12, struct S3 * __capability q12)
{
  return x12 ? &p12->a : &q12->a; /* { dg-warning {pointer type mismatch in conditional expression} } */
  /* { dg-warning {converting non-capability type 'T \*' {aka 'int \*'} to capability type 'void \* __capability' without an explicit cast} "" { target *-*-* } .-1 } */
}
/* { dg-final { scan-tree-dump-times {return x12 != 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) &p12->a\) : \(void \* __capability\) &q12->a;} 1 "original" } } */

void * __capability void1 (int x13, void * __capability p13, int * __capability q13)
{
  return x13 ? p13 : q13;
}
/* { dg-final { scan-tree-dump-times {return x13 != 0 \? p13 : \(void \* __capability\) q13;} 1 "original" } } */

void * __capability void2 (int x14, int * __capability p14, void * __capability q14)
{
  return x14 ? p14 : q14;
}
/* { dg-final { scan-tree-dump-times {return x14 != 0 \? \(void \* __capability\) p14 : q14;} 1 "original" } } */

void * __capability void3 (int x15, void * __capability p15, int *q15)
{
  return x15 ? p15 : q15; /* { dg-warning {converting non-capability type 'int \*' to capability type 'void \* __capability' without an explicit cast} } */
}
/* { dg-final { scan-tree-dump-times {return x15 == 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) q15\) : p15;} 1 "original" } } */

void * __capability void4 (int x16, void *p16, int * __capability q16)
{
  return x16 ? p16 : q16; /* { dg-warning {converting non-capability type 'void \*' to capability type 'void \* __capability' without an explicit cast} } */
}
/* { dg-final { scan-tree-dump-times {return x16 != 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) p16\) : \(void \* __capability\) q16;} 1 "original" } } */

void * __capability void5 (int x17, int * __capability p17, void *q17)
{
  return x17 ? p17 : q17; /* { dg-warning {converting non-capability type 'void \*' to capability type 'void \* __capability' without an explicit cast} } */
}
/* { dg-final { scan-tree-dump-times {return x17 == 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) q17\) : \(void \* __capability\) p17;} 1 "original" } } */

void * __capability void6 (int x18, int *p18, void * __capability q18)
{
  return x18 ? p18 : q18; /* { dg-warning {converting non-capability type 'int \*' to capability type 'void \* __capability' without an explicit cast} } */
}
/* { dg-final { scan-tree-dump-times {return x18 != 0 \? \.REPLACE_ADDRESS_VALUE \(\.CAP_GLOBAL_DATA_GET \(\), \(long unsigned int\) p18\) : q18;} 1 "original" } } */

void * __capability void7 (int x19, void * __capability p19, struct S2 * __capability q19)
{
  return x19 ? p19 : &q19->a;
}
/* { dg-final { scan-tree-dump-times {return x19 == 0 \? \(void \* __capability\) &q19->a : p19;} 1 "original" } } */
