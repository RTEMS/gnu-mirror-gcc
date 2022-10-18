/* { dg-do compile } */

union U {
    double val;
    union U *ptr;
};

union U *d;
double a;
int b;
int c;

static void fn1(union U *p1, int p2, _Bool p3)
{
    union U *e;

    if (p2 == 0)
	a = ((union U*)((__SIZE_TYPE__)p1 & ~1))->val;
       /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-1 } */

    if (b) {
	e = p1;
    } else if (c) {
	e = ((union U*)((__SIZE_TYPE__)p1 & ~1))->ptr;
       /* { dg-warning "cast from provenance-free integer type to pointer type will give pointer that can not be dereferenced" "" { target { cheri_pointers_are_caps } } .-1 } */
	d = e;
    } else {
	e = 0;
	d = ((union U*)0)->ptr;
    }

    fn1 (e, 0, 0);
    fn1 (0, 0, p3);
}

void fn2 (void)
{
  fn1 (0, 0, 0);
}
