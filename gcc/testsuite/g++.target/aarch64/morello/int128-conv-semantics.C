/* Check that __int128 conversion semantics are implemented
   correctly in the front and middle-end.  */
/* { dg-do assemble } */
/* { dg-additional-options "-save-temps=obj -fdump-tree-original -fpermissive" } */
/* { dg-final { check-function-bodies "**" ""  { {-O[123s]} } } } */
/* { dg-require-effective-target cheri_capability_pure } */
/* { dg-skip-if "" { *-*-* } { -fuse-linker-plugin } } */

/*
** _Z18uintcap_to_uint128u11__uintcap_t:
** 	mov	x1, 0
** 	ret
*/
unsigned __int128 uintcap_to_uint128(unsigned __intcap a1)
{
  return a1;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(__int128 unsigned\) \(long unsigned int\) a1;} 1 "original"  } } */

/*
** _Z17intcap_to_uint128u10__intcap_t:
** 	asr	x1, x0, 63
** 	ret
*/
unsigned __int128 intcap_to_uint128(__intcap a2)
{
  return a2;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(__int128 unsigned\) \(long int\) a2;} 1 "original"  } } */

/*
** _Z14ptr_to_uint128Pi:
** 	asr	x1, x0, 63
** 	ret
*/
unsigned __int128 ptr_to_uint128(int *a3)
{
  return a3; /* { dg-warning \[-fpermissive\] } */
}
/* { dg-final { scan-tree-dump-times {<retval> = \(__int128 unsigned\) \(long int\) a3;} 1 "original"  } } */

/*
** _Z17uintcap_to_int128u11__uintcap_t:
** 	mov	x1, 0
** 	ret
*/
 __int128 uintcap_to_int128(unsigned __intcap a4)
 {
   return a4;
 }
/* { dg-final { scan-tree-dump-times {<retval> = \(__int128\) \(long unsigned int\) a4;} 1 "original"  } } */

/*
** _Z16intcap_to_int128u10__intcap_t:
** 	asr	x1, x0, 63
** 	ret
*/
 __int128 intcap_to_int128(__intcap a5)
 {
   return a5;
 }
/* { dg-final { scan-tree-dump-times {<retval> = \(__int128\) \(long int\) a5;} 1 "original"  } } */

/*
** _Z13ptr_to_int128Pi:
** 	asr	x1, x0, 63
** 	ret
*/
 __int128 ptr_to_int128(int *a6)
 {
   return a6; /* { dg-warning \[-fpermissive\] } */
 }
/* { dg-final { scan-tree-dump-times {<retval> = \(__int128\) \(long int\) a6;} 1 "original"  } } */
