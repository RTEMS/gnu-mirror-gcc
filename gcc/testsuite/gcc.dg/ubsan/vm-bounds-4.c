/* { dg-do run } */
/* { dg-options "-fsanitize=vla-bound" } */



void b0(int n, char (*a)[n]) { }
void b1(int m, char (*a)[m]);
void b2(int m; char (*a)[m], int m) { }
void b3(int m; char (*a)[m], int m);
int n;
void b4(char (*a)[n]) { }
void b5(char (*a)[n]);

int main()
{
	char A0[3];
	b1(4, &A0);
	/* { dg-output "bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	b0(4, &A0);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	b2(&A0, 4);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	b3(&A0, 4);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	n = 4;
	b4(&A0);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'\[^\n\r]*(\n|\r\n|\r)" } */
	b5(&A0);
	/* { dg-output "\[^\n\r]*bound 4 of type 'char \\\[\\\*\\\]' does not match bound 3 of type 'char \\\[3\\\]'" } */
}


void b1(int n, char (*a)[n]) { }
void b3(int m; char (*a)[m], int m) { }
void b5(char (*a)[n]) { }


