/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound" } */


void foo1(void (*p)(int n, char (*a)[n]))
{
	char A0[3];
	(*p)(3, &A0);
	(*p)(4, &A0);	/* */
	/* { dg-output "bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
}

void b0(int n, char (*a)[n]) { }


int n;

void foo2(void (*p)(int n, char (*a)[n]))
{
	n = 4;
	char A0[3];
	(*p)(3, &A0);
	(*p)(4, &A0);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
}

void foo3(void (*p)(int n0, char (*a)[n]))
{
	n = 4;
	char A0[3];
	(*p)(3, &A0);	/* */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	(*p)(4, &A0);	/* */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
}

void foo4(void (*p)(int n, char (*a)[n]))
{
	n = 3;
	char A0[3];
	(*p)(3, &A0);
	(*p)(4, &A0);	/* */
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'" } */
}


void foo5(void (*p)(int n0, char (*a)[n]))
{
	n = 3;
	char A0[3];
	(*p)(3, &A0);
	(*p)(4, &A0);
}


void b1(int n0, char (*a)[n]) { }



int main()
{
	foo1(&b0);

	foo2(&b1);
	foo3(&b1); // we should diagnose mismatch and run-time discrepancies

	foo4(&b1);
	foo5(&b1); // we should diagnose mismatch and run-time discrepancies
}



