/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zbb" } */
/* { dg-skip-if "" { *-*-* } { "-O0" } } */

void
foo(short *d, short *tmp) {
    int x = d[0] + d[1];
    int y = d[2] + d[3];
    tmp[0] = x + y;
    tmp[1] = x - y;
}

/* { dg-final { scan-assembler-not {\mzext\.h\M} } } */
