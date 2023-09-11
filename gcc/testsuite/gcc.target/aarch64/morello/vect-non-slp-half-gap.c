/* This is testing we do not decide that an overrun is fine when vectorising a
   known-alignment vector *in the non-SLP branch of get_group_load_store_type*.  */
/* { dg-do run } */
/* { dg-additional-sources "vect-peel-stack-object-runner.c" } */
/* Is hard to trigger this code path, turn of cost modelling in order to help.  */
/* { dg-additional-options "-fvect-cost-model=unlimited" } */
#define MB_BLOCK_SIZE 16
#define VERT_PRED_16 0
#define HOR_PRED_16 1
#define DC_PRED_16 2
extern unsigned short mprr_2[5][16][16];
extern void initialise_s(int *);

void foo() {
  int s[62];
  int j;
  initialise_s(&s[0]);
  for (j=0; j < MB_BLOCK_SIZE; j++)
    {
      mprr_2[HOR_PRED_16][j][0]=s[j*4];
      mprr_2[VERT_PRED_16][j][0]=s[j*4 + 1];
    }
}
