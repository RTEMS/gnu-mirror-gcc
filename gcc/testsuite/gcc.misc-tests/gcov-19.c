/* { dg-options "-fprofile-arcs -fprofile-conditions -ftest-coverage" } */
/* { dg-do run { target native } } */

/* some side effect to stop branches from being pruned */
int x = 0;

/* || works */
void mcdc001a (int a, int b)
{
    if (a || b) /* conditions(1/4) true(0) false(0 1) */
                /* conditions(end) */
        x = 1;
    else
        x = 2;
}

void mcdc001b (int a, int b)
{
    if (a || b) /* conditions(3/4) true(0) false() */
                /* conditions(end) */
        x = 1;
    else
        x = 2;
}

void mcdc001c (int a, int b)
{
    if (a || b) /* conditions(4/4) */
        x = 1;
    else
        x = 2;
}

void mcdc001d (int a, int b, int c)
{
    if (a || b || c) /* conditions(3/6) false(1 2) true(2) */
                     /* conditions(end) */
        x = 1;
}

/* && works */
void mcdc002a (int a, int b)
{
    if (a && b) /* conditions(1/4) true(0 1) false(0) */
                /* conditions(end) */
        x = 1;
    else
        x = 2;
}

void mcdc002b (int a, int b)
{
    if (a && b) /* conditions(3/4) false(0) */
                /* conditions(end) */
      x = 1;
    else
      x = 2;
}

void mcdc002c (int a, int b)
{
    if (a && b) /* conditions(4/4) */
      x = 1;
    else
      x = 2;
}

void mcdc002d (int a, int b, int c)
{
    /*
     * This is an odd case, and falls victim to trying to detect nested ifs.
     *
     * if (a) if (b) if (c) with no else is equivalent to if (a && b && c) and
     * the CFGs are identical *unless* the else nodes are generated too. In the
     * && expression, all false edges should go to the same else, but in the
     * nested-if case they go to different elses.
     *
     * This can be surprising, and bad for MC/DC because non-independent
     * conditionals masked by terms further-right can not be detected. If an
     * else node is generated, this expression becomes a 3-term decision again.
     */
    if (a && b && c) /* conditions(suppress) conditions(4/6) false(0 2) */
                     /* conditions(end) */
        x = 1;
}

/* negation works */
void mcdc003a (int a, int b)
{
    if (!a || !b) /* conditions(2/4) false(0 1) */
                  /* conditions(end) */
      x = 1;
    else
      x = 2;
}

/* single conditionals with and without else */
void mcdc004a (int a)
{
    if (a) /* conditions(1/2) true(0) */
           /* conditions(end) */
      x = 1;
    else
      x = 2;
}

void mcdc004b (int a)
{
    if (a) /* conditions(2/2) */
      x = 1;
    else
      x = 2;
}

void mcdc004c (int a)
{
    if (a) /* conditions(1/2) false(0) */
           /* conditions(end) */
        x = 1;
}

void mcdc004d (int a, int b, int c) {
    if (a)  /* conditions(2/2) */
    {
        if (b || c) /* conditions(1/4) true(1) false(0 1) */
            x = a + b + c;
    }
}

/* mixing && and || works */
void mcdc005a (int a, int b, int c)
{
    if ((a && b) || c) /* conditions(1/6) true(0 1) false(0 1 2) */
                       /* conditions(end) */
        x = 1;
    else
        x = 2;
}

void mcdc005b (int a, int b, int c, int d)
{
    if ((a && (b || c)) && d) /* conditions(4/8) true(1 2 3) false(0) */
                              /* conditions(end) */
        x = 1;
    else
        x = 2;
}

/* nested conditionals */
void mcdc006a (int a, int b, int c, int d, int e)
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

void mcdc006b (int a, int b, int c)
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
    }
}

/* else/if */
void mcdc007a (int a, int b, int c, int d)
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
    }
}

void mcdc007b (int a, int b, int c)
{
    /* similar to if (a || b || c) x = 1 */
    if (a) /* conditions(2/2) */
        goto then;
    else if (b) /* conditions(2/2) */
        goto then;
    else if (c) /* conditions(1/2) true(0) */
                /* conditions(end) */
        goto then;

    return;

then:
        x = 1;
}

/* while loop */
void mcdc008a (int a)
{
    while (a < 10) /* conditions(2/2) */
        x = a++;
}

void mcdc008b (int a)
{
    while (a > 10) /* conditions(1/2) true(0) */
                   /* conditions(end) */
        x = a--;
}

void mcdc008c (int a)
{
    // should work, even with no body
    while (a) /* conditions(2/2) */
        break;
}

void mcdc008d (int a, int b, int c, int d)
{
    /* multi-term loop conditional */
    while ((a && (b || c)) && d) /* conditions(8/8) */
        a = b = c = d = 0;
}

void mcdc009a (int a, int b)
{
    while (a > 0 && b > 0) /* conditions(3/4) false(1) */
                           /* conditions(end) */
        x = a--;
}

/* for loop */
void mcdc010a(int a, int b)
{
    for (int i = 0; i < b; i++) /* conditions(2/2) */
      {
        if (a < b) /* conditions(2/2) */
            x = 1;
        else
            x = a += 2;
      }
}

int always (int) { return 1; }

/* no-condition infinite loops */
void mcdc010b (int a)
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
void mcdc011a (int a, int b, int c)
{
    x = (a && b) || c; /* conditions(5/6) false(1) */
                       /* conditions(end) */
}

/* sequential expressions are handled independently */
void mcdc012a (int a, int b, int c) {
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
void mcdc013a (int a, int /* b */, int c)
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

void mcdc014a ()
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
void mcdc015a (int a, int b)
{
    if (a) /* conditions(2/2) */
        return;

    if (b) /* conditions(1/2) true(0) */
           /* conditions(end) */
        x = 1;
}

void mcdc015b (int a, int b)
{
    for (int i = 5; i > a; i--) /* conditions(2/2) */
    {
        if (i == b) /* conditions(2/2) */
            return;
        x = i;
    }
}

void mcdc015c (int a, int b)
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
void mcdc016a (int a, int b)
{
    for (int i = 0; i < a; i++) /* conditions(2/2) */
        for (int k = 0; k < b; k++) /* conditions(2/2) */
            x = i + k;
}

void mcdc016b (int a, int b)
{
    for (int i = 0; i < a; i++) /* conditions(2/2) */
    {
        if (a > 5) /* conditions(2/2) */
            break;

        for (int k = 0; k < b; k++) /* conditions(2/2) */
            x = i + k;
    }
}

void mcdc016c (int a, int b)
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

void mcdc016d (int a, int b)
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
void mcdc017a (int a)
{
    do {
        a--;
    } while (a > 0); /* conditions(2/2) */
}

void noop () {}

void mcdc017b (int a, int b)
{
    do {
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

void mcdc017c (int a, int b)
{
    int left = 0;
    int right = 0;
    int n = a + b;
    do {
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

void mcdc018b (int a, int b, int c) {
    int n;
    while (a) /* conditions(2/2) */
    {
        /*
         * else block does not make a difference for the problem, but ensures
         * loop termination
         */
        if (b) /* conditions(2/2) */
            n = c ? 0 : 0; // does not show up in CFG (embedded in the block)
        else
            n = 0;
        a = n;
    }
}

/* too many conditions, coverage gives up */
void mcdc019a () {
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
void mcdc020a (int a)
{
    // special case, this can be reduced to:
    // _1 = argc != 0;
    // e = (int) _1;
    x = a ? 1 : 0;

    // changing to different int makes branch
    x = a ? 2 : 1; /* conditions(2/2) */
}

void mcdc020b (int a, int b)
{
    x = (a || b) ? 1 : 0; /* conditions(3/4) true(1) */
}

void mcdc020c (int a, int b)
{
    x = a ? 0
      : b ? 1 /* conditions(2/2) */
      : 2;    /* conditions(1/2) false(0) */
              /* conditions(end) */
}

/* test with functions as conditionals */

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

  mcdc005a (1, 0, 1);

  mcdc005b (1, 1, 0, 0);
  mcdc005b (1, 0, 0, 0);

  mcdc006a (0, 0, 0, 0, 0);
  mcdc006a (1, 0, 0, 0, 0);
  mcdc006a (1, 1, 1, 0, 0);

  mcdc006b (0, 0, 0);
  mcdc006b (1, 0, 0);
  mcdc006b (1, 1, 0);
  mcdc006b (1, 1, 1);

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

  mcdc010b (1);

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

  mcdc019a ();

  mcdc020a (0);
  mcdc020a (1);

  mcdc020b (0, 0);
  mcdc020b (1, 0);

  mcdc020c (0, 1);
  mcdc020c (1, 1);
}

/* { dg-final { run-gcov conditions { --conditions gcov-19.c } } } */
