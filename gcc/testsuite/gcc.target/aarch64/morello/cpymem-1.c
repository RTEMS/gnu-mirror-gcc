struct s { __uintcap_t arr[4]; };

void
f1 (struct s *__capability dst, struct s *__capability src)
{
  *dst = *src;
}

void
f2 (struct s *dst, struct s *__capability src)
{
  *dst = *src;
}

void
f3 (struct s *__capability dst, struct s *src)
{
  *dst = *src;
}
