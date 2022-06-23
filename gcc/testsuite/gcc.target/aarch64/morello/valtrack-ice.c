/* { dg-do compile } */
/* { dg-additional-options "-O2 -g" } */
void fmtinfo(char *fmt) {
  int c = 0;
  do {
    switch (c) {
    default:
      if ((c = fmt[1]) && c != 'l')
      case '.':
        if (fmt)
          --fmt;
    }
  }
  while (c = *++fmt);
}
