struct S { char s[0]; } *__capability a;

void
foo (void)
{
  char *b = (char *) a->s;
  int c = 0;
  b[0] = 0;
  while (++c < 9)
    b[c] = 255;
}
