/* { dg-do compile } */
struct s {
  int x;
} __capability *p; /* { dg-warning "use of '__capability' before the pointer type is deprecated" } */
