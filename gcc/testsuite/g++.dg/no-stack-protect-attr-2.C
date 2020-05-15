/* PR c/94722 */
/* { dg-do compile } */

int __attribute__((no_stack_protect, stack_protect)) c() /* { dg-warning "ignoring attribute 'stack_protect' because it conflicts with attribute 'no_stack_protect'" } */
{
  return 0;
}
