/* Similar to vect-peel-stack-object.c this test only fails on Morello.
   The over-read only happens within alignment boundaries which means it can't
   trigger any problem by reading over a page boundary.
   This is testing a different mechanism to vect-peel-stack-object.c, this is
   testing reducing a vector load to a half-vector while that is testing
   peeling for gaps.  */
/* { dg-do run } */
/* { dg-additional-sources "vect-peel-stack-object-runner.c" } */
#define MB_BLOCK_SIZE 16
#define VERT_PRED_16 0
#define HOR_PRED_16 1
#define DC_PRED_16 2
extern unsigned short mprr_2[5][16][16];
extern void initialise_s(int *);

void foo() {
  int s[62];
  int i,j;
  initialise_s(&s[0]);
  for (j=0; j < MB_BLOCK_SIZE; j++)
    for (i=0; i < MB_BLOCK_SIZE; i++)
      {
	mprr_2[VERT_PRED_16 ][j][i]=s[j*4 + 1];
	mprr_2[HOR_PRED_16 ][j][i]=s[j*4];
      }
}
