/* { dg-do compile }  */
/* { dg-additional-options "-fdump-tree-lower" } */
extern __int128 __attribute__((aligned(16))) outside[20];
extern __int128 __attribute__((aligned(16))) otheroutside[20];
void* other()
{
  return __builtin_memcpy(&outside, &otheroutside, 320);
}
/* Memcpy call is inlined in the tree but not the resulting assembly.  */
/* { dg-final { scan-tree-dump-not "memcpy" "lower" } } */
/* { dg-final { scan-assembler "bl\tmemcpy" } } */
