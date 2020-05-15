/* PR c/94722 */
/* { dg-do compile } */

int __attribute__((no_stack_protect, stack_protect)) c() /* { dg-warning "'stack_protect' attribute ignored due to conflict with 'no_stack_protect' attribute" } */
{
  return 0;
}
