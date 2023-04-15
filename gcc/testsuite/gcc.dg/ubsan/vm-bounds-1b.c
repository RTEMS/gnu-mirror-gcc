/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound" } */
/* { dg-require-effective-target trampolines } */


static char bb[4][4];
static char (*g())[4][4] { return &bb; }

int main()
{
	int n = 3;
	char b[4];
	char (*f())[++n] { return &b; }

	if (4 != sizeof(*f()))
		__builtin_abort();

	char (*(*p)())[++n] = &f;	/* { dg-output "bound 5 of type 'char \\\[\\\*\\\]' does not match bound 4 of type 'char \\\[\\\*\\\]'\[^\n\r]*(\n|\r\n|\r)" } */


	if (5 != sizeof(*(*p)()))
		__builtin_abort();

	char (*(*q)())[++n][4] = &g;	/* { dg-output "\[^\n\r]*bound 6 of type 'char \\\[\\\*\\\]\\\[4\\\]' does not match bound 4 of type 'char \\\[4\\\]\\\[4\\\]'\[^\n\r]*(\n|\r\n|\r)" } */


	if (6 * 4 != sizeof(*(*q)()))
		__builtin_abort();

	return 0;
}

