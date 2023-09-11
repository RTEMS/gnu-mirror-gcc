/* When vectorising an object on the stack the vectorizor may determine that
   the object can have vector-size alignment.  In that case accesses outside of
   the object are known not to trigger a page fault and hence we're fine
   (we only read the data and do not use it).  For CHERI we need to worry about
   such accesses, hence we add that test here.  */
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
      mprr_2[HOR_PRED_16][j][i]=s[j*4];
}
