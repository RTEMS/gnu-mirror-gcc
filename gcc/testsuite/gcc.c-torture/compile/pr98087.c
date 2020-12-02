/* PR c/98087 */

struct S {};
void foo (int n)
{
  struct S a[n][0];
  __builtin_clear_padding (a);
}
