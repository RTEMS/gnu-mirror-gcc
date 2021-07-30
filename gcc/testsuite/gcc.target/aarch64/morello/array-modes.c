/* { dg-do compile } */

/* This simply checks that we don't ICE on this code: we were getting
   the wrong mode for the array (using an integer mode which is wrong
   given that the elements are unions in CADImode).  */

union U {
  char *p;
};
void f(void) {
  union U a[] = {0, 0};
}
