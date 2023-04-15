/* { dg-do compile } */
/* { dg-options "-fsanitize=vla-bound" } */

// make sure we do not ICE on any of these

const char* name = "hallo";


typedef void (*ht)(int n, int m, char x[n][m]);
void e(ht) { }

int n, m;
static void f0(char a[n][m]) { }
static void f1(int u, int v, char a[u][v]) { }
static void f2(int u, int v, char a[u][v]) { }

void f(void)
{
	int x = 1;
	int (*m)[x] = 0;
	m = ({ long* d2; (int (*)[d2[0]])(0); });

	/* function pointer assignments */

	void (*gp)(char x[4][3]) = f0;
	void (*hp)(int n, int m, char x[n][m]) = f1;
	ht hp2 = f1;
	e(f1);

	/* composite type */

	int u = 3; int v = 4;
	char a[u][v];
	(1 ? f1 : f2)(u, v, a);
}

/* size expression in parameter */

extern void a(long N, char (*a)[N]);

static void b(void)
{
	a(1, ({ int d = 0; (char (*)[d])0; }) );
}

/* composite type */

int c(int u, char (*a)[u]);
int c(int u, char (*a)[u]) { }

int d(void)
{
	char a[3];
	c(3, &a);
}

