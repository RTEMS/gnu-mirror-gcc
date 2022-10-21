/* { dg-do compile } */

int
foo () {
  return __builtin_align_down (120, 16);
}

int
bar () {
  return __builtin_align_down (127, 32);
}

int
baz () {
  return __builtin_align_down (1023, 512);
}

/* { dg-final { scan-assembler-times {mov\t[wx][0-9]+, 112} 1 } } */
/* { dg-final { scan-assembler-times {mov\t[wx][0-9]+, 96} 1 } } */
/* { dg-final { scan-assembler-times {mov\t[wx][0-9]+, 512} 1 } } */
