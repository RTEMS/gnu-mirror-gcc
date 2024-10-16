/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2 -mbranch-protection=gcs" } */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {\.L[0-9]+\:} } } */

void run(void (*)());

/*
** bar.0:
**	...
**	hint	40 // chkfeat x16
**	tbnz	w16, 0, (\.L[0-9]+)
**	...
**	mrs	x1, s3_3_c2_c5_1 // gcspr_el0
**	subs	x1, x3, x1
**	bne	(\.L[0-9]+)\n\1\:
**	...
**	br	x[0-9]+\n\2\:
**	...
**	sysl	xzr, #3, c7, c7, #1 // gcspopm
**	...
**	b	\1
*/
int
foo (int *ptr)
{
  __label__ failure;

  void bar () { *ptr += 1; goto failure; }
  run (bar);
  return 1;

failure:
  return 0;
}
