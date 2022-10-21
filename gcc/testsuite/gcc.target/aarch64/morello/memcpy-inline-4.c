/* { dg-do compile }  */
/* { dg-additional-options "-fdump-tree-lower" } */
extern __int128 __attribute__((aligned(16))) outside[2];
extern __int128 __attribute__((aligned(16))) otheroutside[2];
void* other()
{
  return __builtin_memcpy(&outside, &otheroutside, 32);
}
/* Memcpy call is inlined in both the tree and the resulting assembly.  */
/* { dg-final { scan-tree-dump-not "memcpy" "lower" } } */
/* { dg-final { scan-assembler-not "bl\tmemcpy" } } */
