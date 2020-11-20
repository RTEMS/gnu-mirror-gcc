void add0 (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
	   TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = a[i] + b[i];
}

void add90snd (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
	       TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = a[i] + (b[i] * 1.0i);
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */

void add180snd (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
	        TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = a[i] + (b[i] * 1.0i * 1.0i);
}

void add270snd (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
	        TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = a[i] + b[i];
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" } } */

void add90fst (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
	       TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = (a[i] * 1.0i) + b[i];
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT90" 1 "vect" } } */

void add180fst (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
	        TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = (a[i] * 1.0i * 1.0i) + b[i];
}

void add270fst (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
	        TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = (a[i] * 1.0i * 1.0i * 1.0i) + b[i];
}

/* { dg-final { scan-tree-dump-times "stmt.*COMPLEX_ADD_ROT270" 1 "vect" } } */

void addconjfst (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
		 TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = ~a[i] + b[i];
}

void addconjsnd (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
		 TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = a[i] + ~b[i];
}

void addconjboth (TYPE _Complex a[restrict N], TYPE _Complex b[restrict N],
		  TYPE _Complex c[restrict N])
{
  for (int i=0; i < N; i++)
    c[i] = ~a[i] + ~b[i];
}
