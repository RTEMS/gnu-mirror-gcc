/* { dg-do compile } */
/* { dg-options "-O2 -march=morello+c64 -mabi=purecap" } */

int
foo () {
  return __builtin_align_up (120, 16);
}

int
bar () {
  return __builtin_align_up (1, 256);
}

int
baz () {
  return __builtin_align_up (1023, 512);
}

/* { dg-final { scan-assembler-times {mov\t[wx][0-9]+, 128} 1 } } */
/* { dg-final { scan-assembler-times {mov\t[wx][0-9]+, 256} 1 } } */
/* { dg-final { scan-assembler-times {mov\t[wx][0-9]+, 1024} 1 } } */
