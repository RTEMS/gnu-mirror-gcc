/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound" } */


/* test return types */

int m, n;

static char (*z0(void))[5][5] { char (*p)[m][n] = 0; return p; }
static char (*z1(void))[5][5] { char (*p)[5][n] = 0; return p; }
static char (*z2(void))[5][5] { char (*p)[m][5] = 0; return p; }


int main()
{
	m = 4, n = 3;

	int u = 4;
	int v = 3;

	/* initialization */

	char a[4];
	char (*pa)[u] = &a;
	char (*qa)[v] = &a;
	/* { dg-output "bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char b[u];
	const char (*pb)[u] = &b;
	char (*qb)[v] = &b;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char c[4][3];
	char (*pc0)[u][v] = &c;
	char (*qc0)[v][u] = &c;	
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]\\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char (*pc1)[u][3] = &c;
	char (*qc1)[v][3] = &c;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[3\\\]' does not match bound 4 of type 'char \\\[4\\\]\\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char (*pc2)[4][v] = &c;
	char (*qc2)[4][u] = &c;
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char (*pc3)[][v] = &c;	
	char (*qc3)[][u] = &c;
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char d[u][v];
	char (*pd0)[4][3] = &d;
	char (*qd0)[3][4] = &d;	
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[3\\\]\\\[4\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[4\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	char (*pd1)[u][3] = &d;	
	char (*qd1)[v][4] = &d;	
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[4\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[4\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	char (*pd2)[4][v] = &d;
	char (*qd2)[3][u] = &d;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[3\\\]\\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	char (*pd3)[u][v] = &d;
	char (*qd3)[v][u] = &d;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	char e[4][v];
	char (*pe0)[4][3] = &e;
	char (*qe0)[4][4] = &e;	
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[4\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char f[u][3];
	char (*pf)[4][3] = &f;
	char (*qf)[3][3] = &f;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[3\\\]\\\[3\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char (*g[u])[v];
	char (*(*pg)[u])[v] = &g;
	char (*(*qg)[v])[u] = &g;	
	/* { dg-output "\[^\n\r]*bound 3 of type '\[^\]]*\\\[\\\*\\\]' does not match bound 4 of type '\[^\]]*\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	/* assignment */

	pa = &a;
	qa = &a;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	pb = &b;
	qb = &b;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	pc0 = &c;
	qc0 = &c;	
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 4 of type 'char \\\[4\\\]\\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	pc1 = &c;
	qc1 = &c;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[3\\\]' does not match bound 4 of type 'char \\\[4\\\]\\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	pc2 = &c;
	qc2 = &c;
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	pd0 = &d;
	qd0 = &d;	
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[3\\\]\\\[4\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[4\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	pd1 = &d;
	qd1 = &d;	
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[4\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[4\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	pd2 = &d;
	qd2 = &d;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[3\\\]\\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	pd3 = &d;
	qd3 = &d;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	
	pe0 = &e;
	qe0 = &e;
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[4\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	pf = &f;
	qf = &f;
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[3\\\]\\\[3\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	/* return */
	z0();		
	/* { dg-output "\[^\n\r]*bound 5 of type 'char \\\[5\\\]\\\[5\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 5 of type 'char \\\[5\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	z1();		
	/* { dg-output "\[^\n\r]*bound 5 of type 'char \\\[5\\\]' does not match bound 3 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	z2();	
	/* { dg-output "\[^\n\r]*bound 5 of type 'char \\\[5\\\]\\\[5\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]\\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */

	char (*(*p)(void))[u][v] = &z0;
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]\\\[\\\*\\\]' does not match bound 5 of type 'char \\\[5\\\]\\\[5\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	/* { dg-output "\[^\n\r]*bound 3 of type 'char \\\[\\\*\\\]' does not match bound 5 of type 'char \\\[5\\\]'" } */
}

