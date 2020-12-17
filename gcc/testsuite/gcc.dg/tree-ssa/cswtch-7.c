/* PR tree-optimization/94779 */
/* { dg-options "-O2 -fdump-tree-switchconv" } */
/* { dg-do compile { target nonpic } } */

int global;

int f1(unsigned x)
{
    switch (x)
    {
        case 0:
            return 1;
        case 1:
            return 2;
    }
}

/* { dg-final { scan-tree-dump-times "Linear transformation with A = 1 and B = 1" 1 "switchconv" } } */
/* { dg-final { scan-tree-dump "Switch converted" "switchconv" } } */
