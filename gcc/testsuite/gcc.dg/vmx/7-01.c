/* { dg-do compile } */
#include <altivec.h>
extern vector signed short image[];
extern vector signed short band[];

#define load(a,b) (a[b])
#define store(v,a,b) (a[b]) = (v)

void
haar (vector signed char a, vector signed char b, vector signed char c,
      vector signed char d, unsigned int N, int XX)
{
  unsigned int i;
  vector unsigned char high, low;
  vector signed int zero = ((vector signed int){0,0,0,0});

  for (i = 0; i < N; i++) {
    high = (vector unsigned char) (vec_mergeh (load(image, i+XX),
					       load(image, i)));
    low = (vector unsigned char) (vec_mergel (load(image, i+XX),
					      load(image, i)));

    store (vec_packs (vec_msum (a, high, zero),
		      vec_msum (a, low, zero)),
	   band, i);
    store (vec_packs (vec_msum (b, high, zero),
		      vec_msum (b, low, zero)),
	   band, i+1);
    store(vec_packs (vec_msum (c, high, zero),
		     vec_msum (c, low, zero)),
	  band, i+2);
    store(vec_packs (vec_msum (d, high, zero),
		     vec_msum (d, low, zero)),
	  band, i+3);
  }
}
