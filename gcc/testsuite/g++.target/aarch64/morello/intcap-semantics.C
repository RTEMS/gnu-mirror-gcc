/* Check that intcap semantics are implemented correctly in the
   front-end.  */
/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

__intcap f1(__intcap x1, long y1)
{
  return x1 + y1;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x1>, \(long int\) SAVE_EXPR <x1> \+ y1\)} 1 "original"  } } */

__intcap f2(__intcap x2, int y2)
{
  return x2 + y2;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x2>, \(long int\) SAVE_EXPR <x2> \+ \(long int\) y2\)} 1 "original"  } } */

__intcap f3(__intcap x3, short y3)
{
  return x3 + y3;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x3>, \(long int\) SAVE_EXPR <x3> \+ \(long int\) y3\)} 1 "original"  } } */

__intcap f4(__intcap x4, unsigned int y4)
{
  return x4 + y4;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x4>, \(long int\) SAVE_EXPR <x4> \+ \(long int\) y4\)} 1 "original"  } } */

unsigned __intcap f5(__intcap x5, unsigned long y5)
{
  return x5 + y5;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(\(unsigned __intcap\) SAVE_EXPR <x5>, \(long unsigned int\) SAVE_EXPR <x5> \+ y5\)} 1 "original"  } } */

unsigned __intcap f6(unsigned __intcap x6, unsigned long y6)
{
  return x6 + y6;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x6>, \(long unsigned int\) SAVE_EXPR <x6> \+ y6\)} 1 "original"  } } */

unsigned __intcap f7(unsigned __intcap x7, long y7)
{
  return x7 + y7;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x7>, \(long unsigned int\) SAVE_EXPR <x7> \+ \(long unsigned int\) y7\)} 1 "original"  } } */

bool f8(__intcap x8, __intcap y8)
{
  return x8 > y8;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long int\) x8 > \(long int\) y8;} 1 "original"  } } */

bool f9(unsigned __intcap x9, __intcap y9)
{
  return x9 > y9;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long unsigned int\) x9 > \(long unsigned int\) y9;} 1 "original"  } } */

__intcap f10 (long x10)
{
  return (__intcap)x10;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(0, x10\);} 1 "original"  } } */

char * __capability f11 (char * __capability x11, __intcap y11)
{
  return x11 + y11;
}
/* { dg-final { scan-tree-dump-times {<retval> = x11 \+ \(sizetype\) y11;} 1 "original"  } } */

float f12(__intcap x12, float y12)
{
  return x12 + y12;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(float\) \(long int\) x12 \+ y12;} 1 "original"  } } */

__intcap f13(__intcap x13)
{
  return +x13;
}
/* { dg-final { scan-tree-dump-times {<retval> = NON_LVALUE_EXPR <x13>;} 1 "original"  } } */

__intcap f14(__intcap x14)
{
  return -x14;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x14>, -\(long int\) SAVE_EXPR <x14>\)} 1 "original"  } } */

__intcap f15(__intcap x15)
{
  return ~x15;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x15>, ~\(long int\) SAVE_EXPR <x15>\)} 1 "original"  } } */

bool f16(__intcap x16)
{
  return !x16;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long int\) x16 == 0;} 1 "original"  } } */

__intcap gc;
__intcap *f17 ()
{
  return &gc;
}
/* { dg-final { scan-tree-dump-times {<retval> = &gc;} 1 "original"  } } */

__intcap f18 (__intcap x18 __attribute__((cheri_no_provenance)), __intcap y18)
{
  return x18 + y18;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <y18>, \(long int\) x18 \+ \(long int\) SAVE_EXPR <y18>\)} 1 "original"  } } */

__intcap f19 (__intcap x19,
	      __intcap y19 __attribute__((cheri_no_provenance)))
{
  return x19 + y19;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x19>, \(long int\) SAVE_EXPR <x19> \+ \(long int\) y19\)} 1 "original"  } } */

__intcap f20 (__intcap x20 __attribute__((cheri_no_provenance)),
	      __intcap y20 __attribute__((cheri_no_provenance)))
{
  return x20 + y20;
}

/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(0, \(long int\) x20 \+ \(long int\) y20\)} 1 "original"  } } */

unsigned __intcap f21 (unsigned __intcap __attribute__((cheri_no_provenance))
		       x21,
		       __intcap __attribute__((cheri_no_provenance)) y21)
{
  return x21 + y21;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(0, \(long unsigned int\) x21 \+ \(long unsigned int\) y21\)} 1 "original"  } } */

unsigned __intcap f22 (__intcap __attribute__((cheri_no_provenance)) x22,
		       unsigned __intcap __attribute__((cheri_no_provenance))
		       y22)
{
  return x22 + y22;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(0, \(long unsigned int\) x22 \+ \(long unsigned int\) y22\)} 1 "original"  } } */

unsigned __intcap f23 (unsigned __intcap
		       __attribute__ ((cheri_no_provenance)) x23,
		       unsigned __intcap
		       __attribute__ ((cheri_no_provenance)) y23)
{
  return x23 + y23;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(0, \(long unsigned int\) x23 \+ \(long unsigned int\) y23\)} 1 "original"  } } */

__intcap f24 (__intcap __attribute__((cheri_no_provenance)) x24)
{
  return x24 + 1;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(0, \(long int\) x24 \+ 1\);} 1 "original"  } } */

__intcap f25 (__intcap __attribute__ ((cheri_no_provenance)) x25)
{
  return -x25;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(0, -\(long int\) x25\);} 1 "original"  } } */

__intcap x26;
__intcap f26() {
    return x26++;
}
/* { dg-final { scan-tree-dump-times {<retval> = SAVE_EXPR <x26>;, x26 = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x26>, \(long int\) SAVE_EXPR <x26> \+ 1\);, SAVE_EXPR <x26>;} 1 "original"  } } */

__intcap x27;
__intcap f27() {
    return ++x27;
}
/* { dg-final { scan-tree-dump-times {<retval> = x27 = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x27>, \(long int\) SAVE_EXPR <x27> \+ 1\)} 1 "original"  } } */

__intcap x28 __attribute__((cheri_no_provenance));
__intcap f28() {
    return --x28;
}
/* { dg-final { scan-tree-dump-times {<retval> = x28 = \.REPLACE_ADDRESS_VALUE \(0, \(long int\) x28 \+ -1\)} 1 "original"  } } */

__intcap x29 __attribute__((cheri_no_provenance));
__intcap f29()
{
  return x29++;
}
/* { dg-final { scan-tree-dump-times {<retval> = SAVE_EXPR <x29>;, x29 = \.REPLACE_ADDRESS_VALUE \(0, \(long int\) x29 \+ 1\);, SAVE_EXPR <x29>;} 1 "original"  } } */

__intcap f30(__intcap x30,
	     __intcap y30)
{
  return x30 + y30; /* { dg-warning "it is not clear which should be used as the source of provenance" } */
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x30>, \(long int\) SAVE_EXPR <x30> \+ \(long int\) y30\)} 1 "original"  } } */

__intcap x31, y31;
void f31()
{
  x31 += y31;
}
/* { dg-final { scan-tree-dump-times {x31 = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x31>, \(long int\) SAVE_EXPR <x31> \+ \(long int\) y31\)} 1 "original"  } } */

__intcap f32 (__intcap x32, __intcap y32)
{
  return x32 << y32;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x32>, \(long int\) SAVE_EXPR <x32> << \(long int\) y32\)} 1 "original"  } } */

bool f33 (__intcap x33, __intcap y33)
{
  return x33 || y33;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long int\) x33 != 0 \|\| \(long int\) y33 != 0;} 1 "original"  } } */

__intcap f34 (__intcap x34, long long y34)
{
  return x34 + y34;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x34>, \(long int\) SAVE_EXPR <x34> \+ \(long int\) y34\)} 1 "original"  } } */

unsigned __intcap f35 (unsigned __intcap x35, unsigned long long y35)
{
  return x35 + y35;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x35>, \(long unsigned int\) SAVE_EXPR <x35> \+ \(long unsigned int\) y35\)} 1 "original"  } } */

unsigned __intcap f36 (__intcap x36, unsigned long long y36)
{
  return x36 + y36;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(\(unsigned __intcap\) SAVE_EXPR <x36>, \(long unsigned int\) SAVE_EXPR <x36> \+ \(long unsigned int\) y36\)} 1 "original"  } } */

unsigned __intcap f37 (unsigned __intcap x37, long long y37)
{
  return x37 + y37;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x37>, \(long unsigned int\) SAVE_EXPR <x37> \+ \(long unsigned int\) y37\)} 1 "original"  } } */

__intcap f38 (__intcap x38, __int128 y38)
{
  return x38 + y38;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x38>, \(long int\) SAVE_EXPR <x38> \+ \(long int\) y38\)} 1 "original"  } } */

unsigned __intcap f39 (unsigned __intcap x39, unsigned __int128 y39)
{
  return x39 + y39;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x39>, \(long unsigned int\) SAVE_EXPR <x39> \+ \(long unsigned int\) y39\)} 1 "original"  } } */

unsigned __intcap f40 (__intcap x40, unsigned __int128 y40)
{
  return x40 + y40;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(\(unsigned __intcap\) SAVE_EXPR <x40>, \(long unsigned int\) SAVE_EXPR <x40> \+ \(long unsigned int\) y40\)} 1 "original"  } } */

unsigned __intcap f41 (unsigned __intcap x41, __int128 y41)
{
  return x41 + y41;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x41>, \(long unsigned int\) SAVE_EXPR <x41> \+ \(long unsigned int\) y41\)} 1 "original"  } } */

bool f42 (__intcap x42, __intcap y42)
{
  return x42 == y42;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long int\) x42 == \(long int\) y42;} 1 "original" } } */

bool f43 (__intcap x43, unsigned __intcap y43)
{
  return x43 == y43;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long unsigned int\) x43 == \(long unsigned int\) y43;} 1 "original" } } */

bool f44 (unsigned __intcap x44, long y44)
{
  return x44 == y44;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long unsigned int\) x44 == \(long unsigned int\) y44;} 1 "original" } } */

bool f45 (__intcap x45, long long y45)
{
  return x45 == y45;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long int\) x45 == \(long int\) y45;} 1 "original" } } */

bool f46 (__intcap x46, unsigned long y46)
{
  return x46 == y46;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long unsigned int\) x46 == y46;} 1 "original" } } */

bool f47 (__intcap x47, __int128 y47)
{
  return x47 == y47;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long int\) x47 == \(long int\) y47;} 1 "original" } } */

bool f48 (__intcap x48, unsigned __int128 y48)
{
  return x48 == y48;
}
/* { dg-final { scan-tree-dump-times {<retval> = \(long unsigned int\) x48 == \(long unsigned int\) y48;} 1 "original" } } */

__intcap f49 (__intcap x49, __int128 y49)
{
  return x49 << y49;
}
/* { dg-final { scan-tree-dump-times {<retval> = \.REPLACE_ADDRESS_VALUE \(SAVE_EXPR <x49>, \(long int\) SAVE_EXPR <x49> << y49\)} 1 "original"  } } */

__int128 f50 (__int128 x50, __intcap y50)
{
  return x50 << y50;
}
/* { dg-final { scan-tree-dump-times {<retval> = x50 << \(long int\) y50;} 1 "original" } } */
