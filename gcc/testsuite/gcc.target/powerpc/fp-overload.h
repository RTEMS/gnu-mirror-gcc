/* Common code to test the floating point overload functions.  */

#define TEST(TYPE, SCALAR, TYPE_STR, SIZE)				\
									\
void									\
do_add_ ## TYPE_STR ## _f ## SIZE (TYPE *p, TYPE *q, TYPE *r)		\
{									\
  *p = __builtin_add_f ## SIZE (*q, *r);				\
}									\
									\
void									\
do_sub_ ## TYPE_STR ## _f ## SIZE (TYPE *p, TYPE *q, TYPE *r)		\
{									\
  *p = __builtin_sub_f ## SIZE (*q, *r);				\
}									\
									\
void									\
do_mult_ ## TYPE_STR ## _f ## SIZE (TYPE *p, TYPE *q, TYPE *r)		\
{									\
  *p = __builtin_mult_f ## SIZE (*q, *r);				\
}									\
									\
void									\
do_neg_ ## TYPE_STR ## _f ## SIZE (TYPE *p, TYPE *q)			\
{									\
  *p = __builtin_neg_f ## SIZE (*q);					\
}									\
									\
void									\
do_abs_ ## TYPE_STR ## _f ## SIZE (TYPE *p, TYPE *q)			\
{									\
  *p = __builtin_abs_f ## SIZE (*q);					\
}									\
									\
void									\
do_nabs_ ## TYPE_STR ## _f ## SIZE (TYPE *p, TYPE *q)			\
{									\
  *p = __builtin_neg_f ## SIZE (__builtin_abs_f ## SIZE (*q));		\
}									\
									\
void									\
do_fma_ ## TYPE_STR ## _f ## SIZE (TYPE *p,				\
				   TYPE *q,				\
				   TYPE *r,				\
				   TYPE *s)				\
{									\
  *p = __builtin_fma_f ## SIZE (*q, *r, *s);				\
}									\
									\
void									\
do_fms_ ## TYPE_STR ## _f ## SIZE (TYPE *p,				\
				   TYPE *q,				\
				   TYPE *r,				\
				   TYPE *s)				\
{									\
  TYPE neg_s = __builtin_neg_f ## SIZE (*s);				\
  *p = __builtin_fma_f ## SIZE (*q, *r, neg_s);				\
}									\
									\
void									\
do_nfma_ ## TYPE_STR ## _f ## SIZE (TYPE *p,				\
				    TYPE *q,				\
				    TYPE *r,				\
				    TYPE *s)				\
{									\
  TYPE f = __builtin_fma_f ## SIZE (*q, *r, *s);			\
  *p = __builtin_neg_f ## SIZE (f);					\
}									\
									\
void									\
do_nfms_ ## TYPE_STR ## _f ## SIZE (TYPE *p,				\
				    TYPE *q,				\
				    TYPE *r,				\
				    TYPE *s)				\
{									\
  TYPE neg_s = __builtin_neg_f ## SIZE (*s);				\
  TYPE f = __builtin_fma_f ## SIZE (*q, *r, neg_s);			\
  *p = __builtin_neg_f ## SIZE (f);					\
}									\
									\
void									\
do_reduce_ ## TYPE_STR ## _f ## SIZE (SCALAR *p, TYPE *q)		\
{									\
  *p = __builtin_reduce_f ## SIZE (*q);					\
}
