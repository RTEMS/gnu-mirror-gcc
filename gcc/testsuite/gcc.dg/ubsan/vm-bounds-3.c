/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound" } */

int m, n;

static void a0(char (*a)[4]) { }
static void b0(char (*a)[n]) { }
static void c0(char (*a)[n][m]) { }
static void d0(char (*a)[4][m]) { }
static void e0(char (*a)[n][3]) { }
static void f0(char a[n][m]) { }

static void b1(int u, char (*a)[u]) { }
static void c1(int u, int v, char (*a)[u][v]) { }
static void d1(int v, char (*a)[4][v]) { }
static void e1(int u, char (*a)[u][3]) { }
static void f1(int u, int v, char a[u][v]) { }



int main()
{
	m = 3, n = 4;

	int u = 4;
	int v = 3;

	/* function arguments */

	char A0[4];
	char A1[u];
	char B0[3];
	char B1[v];

	a0(&A0);
	a0(&A1);
	a0(&B0);	/* { dg-warning "incompatible pointer type" } */
	a0(&B1);
	/* { dg-output "bound 4 of type 'char \\\[4\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	b0(&A0);
	b0(&A1);
	b0(&B0);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	b0(&B1);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	b1(4, &A0);
	b1(4, &B0);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char C0[4][3];
	char C1[u][3];
	char C2[4][v];
	char C3[u][v];
	char D0[3][4];
	char D1[v][4];
	char D2[3][u];
	char D3[v][u];

	c0(&C0);
	c0(&C1);
	c0(&C2);
	c0(&C3);
	c0(&D0);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]\\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	c0(&D1);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]\\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	c0(&D2);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	c0(&D3);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	d0(&C0);
	d0(&D0);	/* { dg-warning "incompatible pointer type" } */
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	d1(3, &C0);
	d1(3, &D0);	/* { dg-warning "incompatible pointer type" } */
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	e0(&C0);
	e0(&D0);	/* { dg-warning "incompatible pointer type" } */
	e1(4, &C0);
	e1(4, &D0);	/* { dg-warning "incompatible pointer type" } */

	f0(C0);
	f0(D0);
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	f1(4, 3, C0);
	f1(4, 3, D0);
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'" } */
}

