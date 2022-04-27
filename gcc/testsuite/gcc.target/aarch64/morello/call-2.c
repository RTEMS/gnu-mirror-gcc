/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[23s]} } } } */

int ret(int);

/*
** ret_sibcall:
**	mov	w0, #?1
**	b	ret
*/
int __GIMPLE ret_sibcall() {
  int (*__capability ptr)(int);
  int res;

  ptr = __CAP_ADDR ret;
  res = ptr(1);
  return res;
}

/*
** ret_call:
**	...
**	bl	ret
**	...
*/
int __GIMPLE ret_call() {
  int (*__capability ptr)(int);
  int res;
  int sum;

  ptr = __CAP_ADDR ret;
  res = ptr(2);
  sum = res + 1;
  return sum;
}

void noret(int);

/*
** noret_sibcall:
**	mov	w0, #?1
**	b	noret
*/
void __GIMPLE noret_sibcall() {
  void (*__capability ptr)(int);

  ptr = __CAP_ADDR noret;
  ptr(1);
}

/*
** noret_call:
**	...
**	bl	noret
**	...
*/
int __GIMPLE noret_call() {
  void (*__capability ptr)(int);

  ptr = __CAP_ADDR noret;
  ptr(2);
  return 1;
}
