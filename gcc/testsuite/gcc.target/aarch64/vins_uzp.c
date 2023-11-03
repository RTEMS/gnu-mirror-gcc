/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

typedef int v4si __attribute__ ((vector_size (4 * sizeof (int))));

v4si case1(v4si a, v4si b) {
    return  __builtin_shufflevector (a, b, 0, 5, 0, 5);
}

v4si case2(v4si a, v4si b) {
    return  __builtin_shufflevector (a, b, 1, 5, 1, 5);
}

v4si case3(v4si a, v4si b) {
    return  __builtin_shufflevector (a, b, 0, 6, 0, 6);
}

v4si case4(v4si a, v4si b) {
    return  __builtin_shufflevector (a, b, 1, 7, 1, 7);
}

v4si case5(v4si a, v4si b) {
    return  __builtin_shufflevector (a, b, 2, 7, 2, 7);
}

v4si case6(v4si a, v4si b) {
    return  __builtin_shufflevector (b, a, 2, 7, 2, 7);
}

v4si case7(v4si a, v4si b) {
    return  __builtin_shufflevector (a, b, 7, 2, 7, 2);
}

/* { dg-final { scan-assembler-not {\ttbl\t} } } */
/* { dg-final { scan-assembler-not {\tldr\t} } } */
