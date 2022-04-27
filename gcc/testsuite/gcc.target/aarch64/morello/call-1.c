/* { dg-do compile } */

#if !__CHERI_PURE_CAPABILITY__
#pragma GCC target "arch=morello"
#endif

int (*__capability ret)(int);
int ret_sibcall() { return ret(1); }
int ret_call() { return ret(2) + 1; }

void (*__capability noret)(int);
int noret_sibcall() { noret(1); }
int noret_call() { noret(2); return 1; }

/* { dg-final { scan-assembler-times {\tbl?r\tc[0-9]+} 4 } } */
