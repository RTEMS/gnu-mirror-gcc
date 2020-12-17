/* PR tree-optimization/94779 */
/* { dg-options "-O2 -fdump-tree-switchconv" } */
/* { dg-do compile { target nonpic } } */

int global;

int f1(unsigned x)
{
    int v;
    switch (x)
    {
        case 0:
            return 1;
        case 1:
            return 2;
        case 2:
            return 3;
    }
}

/* { dg-final { scan-tree-dump-times "Linear transformation with A = 1 and B = 1" 1 "switchconv" } } */
/* { dg-final { scan-tree-dump-not "if " "switchconv" } } */
