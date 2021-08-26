/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-additional-options "-O3" } */

void e(char *x) { __builtin_memcpy(x, x + 8, 8); }

void f(char *x) { __builtin_memcpy(x, x + 16, 16); }

void g(char *x) { __builtin_memcpy(x, x + 32, 32); }

void h(char *x) { __builtin_memcpy(x, x + 36, 36); }

int fun(int x)
{
    void *a[] = {&&label, &&label2};
    label:
        goto *a[x++];
    label2:
        return x;
}
