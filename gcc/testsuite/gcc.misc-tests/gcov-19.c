/* { dg-options "-fprofile-conditions -ftest-coverage" } */
/* { dg-do run { target native } } */

/* some side effect to stop branches from being pruned */
int x = 0;

/* || works */
void
mcdc001a (int a, int b)
{
    if (a || b) /* conditions(1/4) true(0) false(0 1) */
		/* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc001b (int a, int b)
{
    if (a || b) /* conditions(3/4) true(0) */
		/* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc001c (int a, int b)
{
    if (a || b) /* conditions(4/4) */
	x = 1;
    else
	x = 2;
}

void
mcdc001d (int a, int b, int c)
{
    if (a || b || c) /* conditions(2/6) false(0 1 2) true(2) */
		     /* conditions(end) */
	x = 1;
}

/* && works */
void
mcdc002a (int a, int b)
{
    if (a && b) /* conditions(1/4) true(0 1) false(0) */
		/* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc002b (int a, int b)
{
    if (a && b) /* conditions(3/4) false(0) */
		/* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc002c (int a, int b)
{
    if (a && b) /* conditions(4/4) */
	x = 1;
    else
	x = 2;
}

void
mcdc002d (int a, int b, int c)
{
    if (a && b && c) /* conditions(4/6) false(0 2) */
		     /* conditions(end) */
	x = 1;
}

/* negation works */
void
mcdc003a (int a, int b)
{
    if (!a || !b) /* conditions(2/4) false(0 1) */
		  /* conditions(end) */
	x = 1;
    else
	x = 2;
}

/* single conditionals with and without else */
void
mcdc004a (int a)
{
    if (a) /* conditions(1/2) true(0) */
	   /* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc004b (int a)
{
    if (a) /* conditions(2/2) */
	x = 1;
    else
	x = 2;
}

void
mcdc004c (int a)
{
    if (a) /* conditions(1/2) false(0) */
	   /* conditions(end) */
	x = 1;
}

void
mcdc004d (int a, int b, int c)
{
    /* With no else this is interpreted as (a && (b || c)) */
    if (a)  /* conditions(3/6) true(2) false(1 2)*/
    {
	if (b || c)
	    x = a + b + c;
    }
}

void
mcdc004e (int a, int b, int c)
{
    /* With the else, this is interpreted as 2 expressions */
    if (a)  /* conditions(2/2) */
    {
	if (b || c) /* conditions(1/4) true(1) false(0 1) */
	    x = a + b + c;
    }
    else
    {
	x = c;
    }
}

/* mixing && and || works */
void
mcdc005a (int a, int b, int c)
{
    if ((a && b) || c) /* conditions(1/6) true(0 1) false(0 1 2) */
		       /* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc005b (int a, int b, int c, int d)
{
    /* This is where masking MC/DC gets unintuitive:

       1 1 0 0 => covers 1 (d = 0) as && 0 masks everything to the left
       1 0 0 0 => covers 2 (b = 0, c = 0) as (a && 0) masks a and d is never
       evaluated. */
    if ((a && (b || c)) && d) /* conditions(3/8) true(0 1 2 3) false(0) */
			      /* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc005c (int a, int b, int c, int d)
{
    if (a || (b && c) || d) /* conditions(2/8) true(0 3) false(0 1 2 3) */
			    /* conditions(end) */
        x = a + b + c + d;
}

void
mcdc005d (int a, int b, int c, int d)
{
    /* This test is quite significant - it has a single input
       (1, 0, 0, 0) and tests specifically for when a multi-term left operand
       is masked. d = 0 should mask a || b and for the input there are no other
       sources for masking a (since b = 0). */
    if ((a || b) && (c || d)) /* conditions(2/8) true(0 1 2 3) false(0 1) */
			      /* conditions(end) */
	x = a + b;
    else
	x = c + d;
}

/* nested conditionals */
void
mcdc006a (int a, int b, int c, int d, int e)
{
    if (a) /* conditions(2/2) */
    {
	if (b && c) /* conditions(3/4) false(1) */
		    /* conditions(end) */
	    x = 1;
	else
	    x = 2;
    }
    else
    {
	if (c || d) /* conditions(2/4) true(0 1) */
		    /* conditions(end) */
	    x = 3;
	else
	    x = 4;
    }
}

void
mcdc006b (int a, int b, int c)
{
    if (a) /* conditions(6/6) */
	if (b)
	    if (c)
		x = a + b + c;
}

void
mcdc006c (int a, int b, int c)
{
    if (a) /* conditions(2/2) */
    {
	if (b) /*conditions(2/2) */
	{
	    if (c) /* conditions(2/2) */
	    {
		x = a + b + c;
	    }
	}
	else
	{
	    x = b;
	}
    }
    else
    {
	x = a;
    }
}

/* else/if */
void
mcdc007a (int a, int b, int c, int d)
{
    if (a) /* conditions(2/2) */
    {
	if (b) /* conditions(1/2) true(0) */
	       /* conditions(end) */
	    x = 1;
	else
	    x = 2;
    }
    else if (c) /* conditions(2/2) */
    {
	if (d) /* conditions(1/2) true(0) */
	       /* conditions(end) */
	    x = 3;
	else
	    x = 4;
    }
}

void
mcdc007b (int a, int b, int c)
{
    goto begin;
then:
    x = 1;
    return;
begin:
    /* similar to if (a || b || c) x = 1 */
    if (a) /* conditions(2/2) */
	goto then;
    else if (b) /* conditions(2/2) */
	goto then;
    else if (c) /* conditions(1/2) true(0) */
		/* conditions(end) */
	goto then;
}

/* while loop */
void
mcdc008a (int a)
{
    while (a < 10) /* conditions(2/2) */
	x = a++;
}

void
mcdc008b (int a)
{
    while (a > 10) /* conditions(1/2) true(0) */
		   /* conditions(end) */
	x = a--;
}

void
mcdc008c (int a)
{
    // should work, even with no body
    while (a) /* conditions(2/2) */
	break;
}

void
mcdc008d (int a, int b, int c, int d)
{
    /* multi-term loop conditional */
    while ((a && (b || c)) && d) /* conditions(8/8) */
	a = b = c = d = 0;
}

void
mcdc009a (int a, int b)
{
    while (a > 0 && b > 0) /* conditions(3/4) false(1) */
			   /* conditions(end) */
	x = a--;
}

/* for loop */
void
mcdc010a(int a, int b)
{
    for (int i = 0; i < b; i++) /* conditions(2/2) */
    {
	if (a < b) /* conditions(2/2) */
	    x = 1;
	else
	    x = a += 2;
    }
}

void
mcdc010b ()
{
    for (int a = 0; a <= 1; ++a) /* conditions(2/2) */
    {
	x = a;
    }
}

int always (int) { return 1; }

/* no-condition infinite loops */
void
mcdc010c (int a)
{
    for (;;)
    {
	if (always(a)) /* conditions(1/2) false(0) */
		       /* conditions(end) */
	{
	    x = a;
	    break;
	}
	x += a + 1;
    }
}

/* conditionals without control flow constructs work */
void
mcdc011a (int a, int b, int c)
{
    x = (a && b) || c; /* conditions(5/6) false(1) */
		       /* conditions(end) */
}

/* sequential expressions are handled independently */
void
mcdc012a (int a, int b, int c)
{
    if (a || b) /* conditions(3/4) true(0) */
		/* conditions(end) */
	x = 1;
    else
	x = 2;

    if (c) /* conditions(2/2) */
	x = 1;
}

/*
 * cannot ever satisfy MC/DC, even with all input combinations, because not all
 * variables independently affect the decision
 */
void
mcdc013a (int a, int /* b */, int c)
{
    /*
     * Specification: (a && b) || c
     *
     * But the expression was implemented wrong. This has branch coverage, but
     * not MC/DC
     */
    if ((a && !c) || c) /* conditions(5/6) false(1) */
	/* conditions(end) */
	x = 1;
    else
	x = 2;
}

void
mcdc014a ()
{
    int conds[64] = { 0 };
    /* conditions(64/128) true(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63) */
    x = conds[ 0] || conds[ 1] || conds[ 2] || conds[ 3] || conds[ 4] ||
	conds[ 5] || conds[ 6] || conds[ 7] || conds[ 8] || conds[ 9] ||
	conds[10] || conds[11] || conds[12] || conds[13] || conds[14] ||
	conds[15] || conds[16] || conds[17] || conds[18] || conds[19] ||
	conds[20] || conds[21] || conds[22] || conds[23] || conds[24] ||
	conds[25] || conds[26] || conds[27] || conds[28] || conds[29] ||
	conds[30] || conds[31] || conds[32] || conds[33] || conds[34] ||
	conds[35] || conds[36] || conds[37] || conds[38] || conds[39] ||
	conds[40] || conds[41] || conds[42] || conds[43] || conds[44] ||
	conds[45] || conds[46] || conds[47] || conds[48] || conds[49] ||
	conds[50] || conds[51] || conds[52] || conds[53] || conds[54] ||
	conds[55] || conds[56] || conds[57] || conds[58] || conds[59] ||
	conds[60] || conds[61] || conds[62] || conds[63]
	;  /* conditions(end) */
}

/* early returns */
void
mcdc015a (int a, int b)
{
    if (a) /* conditions(2/2) */
	return;

    if (b) /* conditions(1/2) true(0) */
	   /* conditions(end) */
	x = 1;
}

void
mcdc015b (int a, int b)
{
    for (int i = 5; i > a; i--) /* conditions(2/2) */
    {
	if (i == b) /* conditions(2/2) */
	    return;
	x = i;
    }
}

void
mcdc015c (int a, int b)
{
    for (int i = 5; i > a; i--) /* conditions(2/2) */
    {
	if (i == b) /* conditions(2/2) */
	{
	    x = 0;
	    return;
	}
	else
	{
	    x = 1;
	    return;
	}

	x = i;
    }
}


/* check nested loops */
void
mcdc016a (int a, int b)
{
    for (int i = 0; i < a; i++) /* conditions(2/2) */
	for (int k = 0; k < b; k++) /* conditions(2/2) */
	    x = i + k;
}

void
mcdc016b (int a, int b)
{
    for (int i = 0; i < a; i++) /* conditions(2/2) */
    {
	if (a > 5) /* conditions(2/2) */
	    break;

	for (int k = 0; k < b; k++) /* conditions(2/2) */
	    x = i + k;
    }
}

void
mcdc016c (int a, int b)
{
    for (int i = 0; i < a; i++) /* conditions(2/2) */
    {
	if (a > 5) /* conditions(1/2) true(0) */
		   /* conditions(end) */
	    return;

	for (int k = 0; k < b; k++) /* conditions(2/2) */
	    x = i + k;
    }
}

void
mcdc016d (int a, int b)
{
    for (int i = 0; i < a; i++) /* conditions(2/2) */
    {
	for (int k = 0; k < 5; k++) /* conditions(2/2) */
	{
	    if (b > 5) /* conditions(1/2) true(0) */
		       /* conditions(end) */
		return;
	    x = i + k;
	}

    }
}

/* do-while loops */
void
mcdc017a (int a)
{
    do
    {
	a--;
    } while (a > 0); /* conditions(2/2) */
}

void
noop () {}

void
mcdc017b (int a, int b)
{
    do
    {
	/*
	 * This call is important; it can add more nodes to the body in the
	 * CFG, which has changes how close exits and breaks are to the loop
	 * conditional.
	 */
	noop ();
	a--;
	if (b) /* conditions(2/2) */
	    break;

    } while (a > 0); /* conditions(2/2) */
}

void
mcdc017c (int a, int b)
{
    int left = 0;
    int right = 0;
    int n = a + b;
    do
    {
	if (a) /* conditions(1/2) false(0) */
	       /* conditions(end) */
	{
	    left = a > left ? b : left; /* conditions(2/2) */
	}
	if (b) /* conditions(1/2) false(0) */
	{
	    right = b > right ? a : right; /* conditions(2/2) */
	}
    } while (n-- > 0); /* conditions(2/2) */
}

int id  (int x) { return  x; }
int inv (int x) { return !x; }

/* collection of odd cases lifted-and-adapted from real-world code */
int mcdc018a (int a, int b, int c, int d, int e, int f, int g, int len)
{
    int n;
    /* adapted from zlib/gz_read */
    do
    {
	n = -1;
	if (n > len) /* conditions(2/2) */
	    n = len;

	if (b) /* conditions(2/2) */
	{
	    if (b < 5) /* conditions(2/2) */
		x = 1;
	    noop();
	}
	else if (c && d) /* conditions(3/4) false(1) */
	{
	    x = 2;
	    break;
	}
	else if (e || f) /* conditions(2/4) false(0 1) */
			 /* conditions(end) */
	{
	    if (id(g)) /* conditions(2/2) */
		return 0;
	    continue;
	}
    } while (a-- > 0); /* conditions(2/2) */

    return 1;
}

void
mcdc018b (int a, int b, int c)
{
    int n;
    while (a) /* conditions(2/2) */
    {
	/* else block does not make a difference for the problem, but ensures
	   loop termination. */
	if (b) /* conditions(2/2) */
	    n = c ? 0 : 0; // does not show up in CFG (embedded in the block)
	else
	    n = 0;
	a = n;
    }
}

/* Adapted from zlib/compress2 */
void
mcdc018c (int a, int b)
{
    int err;
    do
    {
	a = inv (a);
	err = a;
    } while (err); /* conditions(1/2) true(0) */
		   /* conditions(end) */

    a = id (a);
    if (a) /* conditions(1/2) true(0) */
	x *= a + 1;
}

/* too many conditions, coverage gives up */
void
mcdc019a ()
{
    int conds[65] = { 0 };
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wcoverage-too-many-conditions"
    x = conds[ 0] || conds[ 1] || conds[ 2] || conds[ 3] || conds[ 4] ||
	conds[ 5] || conds[ 6] || conds[ 7] || conds[ 8] || conds[ 9] ||
	conds[10] || conds[11] || conds[12] || conds[13] || conds[14] ||
	conds[15] || conds[16] || conds[17] || conds[18] || conds[19] ||
	conds[20] || conds[21] || conds[22] || conds[23] || conds[24] ||
	conds[25] || conds[26] || conds[27] || conds[28] || conds[29] ||
	conds[30] || conds[31] || conds[32] || conds[33] || conds[34] ||
	conds[35] || conds[36] || conds[37] || conds[38] || conds[39] ||
	conds[40] || conds[41] || conds[42] || conds[43] || conds[44] ||
	conds[45] || conds[46] || conds[47] || conds[48] || conds[49] ||
	conds[50] || conds[51] || conds[52] || conds[53] || conds[54] ||
	conds[55] || conds[56] || conds[57] || conds[58] || conds[59] ||
	conds[60] || conds[61] || conds[62] || conds[63] || conds[64]
	;
    #pragma GCC diagnostic pop
}

/* ternary */
void
mcdc020a (int a)
{
    // special case, this can be reduced to:
    // _1 = argc != 0;
    // e = (int) _1;
    x = a ? 1 : 0;

    // changing to different int makes branch
    x = a ? 2 : 1; /* conditions(2/2) */
}

void
mcdc020b (int a, int b)
{
    x = (a || b) ? 1 : 0; /* conditions(3/4) true(1) */
}

void
mcdc020c (int a, int b)
{
    x = a ? 0
	: b ? 1 /* conditions(2/2) */
	: 2;    /* conditions(1/2) false(0) */
		/* conditions(end) */
}

/* Infinite loop (no exit-edge), this should not be called, but it should
   compile fine */
void
mcdc021a ()
{
    while (1)
	;
}

/* Computed goto can give all sorts of problems, including difficult path
   contractions. */
void
mcdc021b ()
{
  void *op = &&dest;
dest:
  if (op) /* conditions(0/2) true(0) false(0) */
	  /* conditions(end) */
    goto * 0;
}

int __sigsetjmp ();

/* This should compile, but not called. */
void
mcdc021c ()
{
  while (x) /* conditions(0/2) true(0) false(0)*/
	    /* conditions(end) */
     __sigsetjmp ();
}

/* If edges are not properly contracted the a && id (b) will be interpreted as
   two independent expressions. */
void
mcdc021d (int a, int b, int c, int d)
{
    if (a && id (b)) /* conditions(1/4) true(0 1) false(0) */
		     /* conditions(end) */
	x = 1;
    else if (c && id (d)) /* conditions(1/4) true(0 1) false(0) */
			  /* conditions(end) */
	x = 2;
    else
	x = 3;
}

/* Adapted from linux arch/x86/tools/relocs.c
   With poor edge contracting this became an infinite loop. */
void
mcdc022a (int a, int b)
{
    for (int i = 0; i < 5; i++) /* conditions(2/2) */
    {
	x = i;
	for (int j = i; j < 5; j++) /* conditions(2/2) */
	{
	    if (id (id (a)) || id (b)) /* conditions(3/4) true(0) */
				       /* conditions(end) */
		continue;
	    b = inv(b);
	}
    }
}

int
mcdc022b (int a)
{
    int devt;
    if (a) /* conditions(2/2) */
    {
	x = a * 2;
	if (x != a / 10 || x != a % 10) /* conditions(1/4) true(1) false(0 1) */
					/* conditions(end) */
	    return 0;
    } else {
	devt = id (a);
	if (devt) /* conditions(1/2) true(0) */
		  /* conditions(end) */
	    return 0;
    }

    return devt;
}

/* Adapted from linux arch/x86/events/intel/ds.c

   It broken sorting so that the entry block was not the first node after
   sorting. */
void
mcdc022c (int a)
{
    if (!a) /* conditions(2/2) */
	return;

    for (int i = 0; i < 5; i++) /* conditions(2/2) */
    {
	if (id (a + i) || inv (a - 1)) /* conditions(1/4) false(0 1) true(1) */
				       /* conditions(end) */
	    x = a + i;
	if (inv (a)) /* conditions(1/2) true(0) */
		     /* conditions(end) */
	    break;
    }
}

void
mcdc022d (int a)
{
    int i;
    for (i = 0; i < id (a); i++) /* conditions(1/2) false(0) */
    {
	if (!inv (a)) /* conditions(1/2) false(0)*/
		      /* conditions(end) */
	    break;
    }

    if (i < a) /* conditions(1/2) false(0) */
	       /* conditions(end) */
	x = a + 1;
}

/* 023 specifically tests that masking works correctly, which gets complicated
   fast with a mix of operators and deep subexpressions.  These tests violates
   the style guide slightly to emphasize the nesting.  They all share the same
   implementation and only one input is given to each function to obtain clean
   coverage results. */
void
mcdc023a (int a, int b, int c, int d, int e, int f, int g, int h, int i, int k,
	  int l, int m, int n)
{
    // [a m n] = 0, [b, ...] = 1
    // a is masked by b and the remaining terms should be short circuited
    if (/* conditions(1/24) true(0 2 3 4 5 6 7 8 9 10 11) false(0 1 2 3 4 5 6 7 8 9 10 11) */
	/* conditions(end) */
	   (a || b)
	|| (   ((c && d) || (e && (f || g) && h))
	    && (k || l)
	    && (m || n)))
	x = a + b;
    else
	x = b + c;
}

void
mcdc023b (int a, int b, int c, int d, int e, int f, int g, int h, int i, int k,
	  int l, int m, int n)
{
    // [a b d h] = 0, [c, ...] = 1
    // h = 0 => false but does not mask (a || b) or (c && d). d = 0 masks c.
    if (/* conditions(4/24) true(0 1 2 3 4 5 6 7 8 9 10 11) false(2 4 5 6 8 9 10 11) */
	/* conditions(end) */
	   (a || b)
	|| (   ((c && d) || (e && (f || g) && h))
	    && (k || l)
	    && (m || n)))
	x = a + b;
    else
	x = b + c;
}

void
mcdc023c (int a, int b, int c, int d, int e, int f, int g, int h, int i, int k,
	  int l, int m, int n)
{
    /* [m n a b] = 0, [...] = 1
       n,m = 0 should mask all other terms than a, b */
    if (/* conditions(4/24) true(0 1 2 3 4 5 6 7 8 9 10 11) false(2 3 4 5 6 7 8 9) */
	/* conditions(end) */
	   (a || b)
	|| (   ((c && d) || (e && (f || g) && h))
	    && (k || l)
	    && (m || n)))
	x = a + b;
    else
	x = b + c;
}

void
mcdc023d (int a, int b, int c, int d, int e, int f, int g, int h, int i, int k,
	  int l, int m, int n)
{
    /* [a b] = 0, [h, ...] = 1
       n,m = 0 should mask all other terms than a, b */
    if (/* conditions(4/24) true(0 1 2 3 4 5 6 7 8 9 10 11) false(2 3 4 5 6 7 10 11) */
	/* conditions(end) */
	   (a || b)
	|| (   ((c && d) || (e && (f || g) && h))
	    && (k || l)
	    && (m || n)))
	x = a + b;
    else
	x = b + c;
}

void
mcdc023e (int a, int b, int c, int d, int e, int f, int g, int h, int i, int k,
	  int l, int m, int n)
{
    /* [a b d] = 0, [c h, ...] = 1
       h = 1 should mask c, d, leave other terms intact.
       If [k l m n] were false then h itself would be masked.
       [a b] are masked as collateral by [m n]. */
    if (/* conditions(5/24) true(0 1 2 3 6 9 11) false(0 1 2 3 4 5 6 7 8 9 10 11) */
	/* conditions(end) */
	   (a || b)
	|| (   ((c && d) || (e && (f || g) && h))
	    && (k || l)
	    && (m || n)))
	x = a + b;
    else
	x = b + c;
}

void
mcdc023f (int a, int b, int c, int d, int e, int f, int g, int h, int i, int k,
	  int l, int m, int n)
{
    /* [a b c f g] = 0, [e, ...] = 1
       [f g] = 0 should mask e, leave [c d] intact. */
    if (/* conditions(5/24) true(0 1 2 3 4 5 6 7 8 9 10 11) false(3 4 7 8 9 10 11) */
	/* conditions(end) */
	   (a || b)
	|| (   ((c && d) || (e && (f || g) && h))
	    && (k || l)
	    && (m || n)))
	x = a + b;
    else
	x = b + c;
}

void
mcdc023g (int a, int b, int c, int d, int e, int f, int g, int h, int i, int k,
	  int l, int m, int n)
{
    /* [a b d f g] = 0, [e c, ...] = 1
       Same as 023f but with [c d] flipped so d masks c rather than c
       short-circuits.  This should not be lost. */
    if (/* conditions(5/24) true(0 1 2 3 4 5 6 7 8 9 10 11) false(2 4 7 8 9 10 11) */
	/* conditions(end) */
	   (a || b)
	|| (   ((c && d) || (e && (f || g) && h))
	    && (k || l)
	    && (m || n)))
	x = a + b;
    else
	x = b + c;
}

int main ()
{
    mcdc001a (0, 1);

    mcdc001b (0, 1);
    mcdc001b (0, 0);

    mcdc001c (0, 1);
    mcdc001c (0, 0);
    mcdc001c (1, 1);

    mcdc001d (1, 1, 1);
    mcdc001d (0, 1, 0);

    mcdc002a (1, 0);

    mcdc002b (1, 0);
    mcdc002b (1, 1);

    mcdc002c (0, 0);
    mcdc002c (1, 1);
    mcdc002c (1, 0);

    mcdc002d (1, 1, 1);
    mcdc002d (1, 0, 0);

    mcdc003a (0, 0);
    mcdc003a (1, 0);

    mcdc004a (0);
    mcdc004b (0);
    mcdc004b (1);
    mcdc004c (1);

    mcdc004d (0, 0, 0);
    mcdc004d (1, 1, 1);

    mcdc004e (0, 0, 0);
    mcdc004e (1, 1, 1);

    mcdc005a (1, 0, 1);

    mcdc005b (1, 1, 0, 0);
    mcdc005b (1, 0, 0, 0);

    mcdc005c (0, 1, 1, 0);

    mcdc005d (1, 0, 0, 0);

    mcdc006a (0, 0, 0, 0, 0);
    mcdc006a (1, 0, 0, 0, 0);
    mcdc006a (1, 1, 1, 0, 0);

    mcdc006b (0, 0, 0);
    mcdc006b (1, 0, 0);
    mcdc006b (1, 1, 0);
    mcdc006b (1, 1, 1);

    mcdc006c (0, 0, 0);
    mcdc006c (1, 0, 0);
    mcdc006c (1, 1, 0);
    mcdc006c (1, 1, 1);

    mcdc007a (0, 0, 0, 0);
    mcdc007a (1, 0, 0, 0);
    mcdc007a (0, 0, 1, 0);

    mcdc007b (0, 0, 0);
    mcdc007b (0, 1, 1);
    mcdc007b (1, 0, 1);

    mcdc008a (0);

    mcdc008b (0);

    mcdc008c (0);
    mcdc008c (1);

    mcdc008d (0, 0, 0, 0);
    mcdc008d (1, 0, 0, 0);
    mcdc008d (1, 0, 1, 0);
    mcdc008d (1, 0, 1, 1);
    mcdc008d (1, 1, 1, 1);

    mcdc009a (0, 0);
    mcdc009a (1, 1);

    mcdc010a (0, 0);
    mcdc010a (0, 9);
    mcdc010a (2, 1);

    mcdc010b ();

    mcdc010c (1);

    mcdc011a (0, 0, 0);
    mcdc011a (1, 1, 0);
    mcdc011a (1, 0, 1);

    mcdc012a (0, 0, 0);
    mcdc012a (0, 1, 1);

    mcdc013a (0, 0, 0);
    mcdc013a (0, 0, 1);
    mcdc013a (0, 1, 0);
    mcdc013a (0, 1, 1);
    mcdc013a (1, 0, 0);
    mcdc013a (1, 0, 1);
    mcdc013a (1, 1, 0);
    mcdc013a (1, 1, 1);

    mcdc014a ();

    mcdc015a (0, 0);
    mcdc015a (1, 0);

    mcdc015b (0, 0);
    mcdc015b (0, 1);
    mcdc015b (6, 1);

    mcdc015c (0, 0);
    mcdc015c (0, 5);
    mcdc015c (6, 1);

    mcdc016a (5, 5);

    mcdc016b (5, 5);
    mcdc016b (6, 5);

    mcdc016c (5, 5);

    mcdc016d (1, 0);

    mcdc017a (0);
    mcdc017a (2);

    mcdc017b (2, 0);
    mcdc017b (0, 1);

    mcdc017c (1, 1);

    mcdc018a (0, 0, 1, 1, 0, 0, 0, 0);
    mcdc018a (0, 1, 0, 0, 0, 0, 1, -2);
    mcdc018a (0, 6, 0, 0, 0, 0, 1, -2);
    mcdc018a (0, 6, 0, 0, 0, 0, 1, -2);
    mcdc018a (0, 0, 0, 1, 0, 1, 1, 0);
    mcdc018a (1, 0, 0, 0, 1, 1, 0, 0);

    mcdc018b (1, 0, 0);
    mcdc018b (1, 1, 0);

    mcdc018c (1, 1);

    mcdc019a ();

    mcdc020a (0);
    mcdc020a (1);

    mcdc020b (0, 0);
    mcdc020b (1, 0);

    mcdc020c (0, 1);
    mcdc020c (1, 1);

    mcdc021d (1, 0, 1, 0);

    mcdc022a (0, 0);

    mcdc022b (0);
    mcdc022b (1);

    mcdc022c (0);
    mcdc022c (1);

    mcdc022d (1);

    mcdc023a (0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    mcdc023b (0, 0, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1);
    mcdc023c (0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0);
    mcdc023d (0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 1, 1);
    mcdc023e (0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1);
    mcdc023f (0, 0, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1);
    mcdc023g (0, 0, 1, 0, 1, 0, 0, 1, 1, 1, 1, 1, 1);
}

/* { dg-final { run-gcov conditions { --conditions gcov-19.c } } } */
