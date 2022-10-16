/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ifcombine-details-blocks" } */

void sink();

void reversed(unsigned char *a)
{
  if (*a & 0x60)
    if (!(*a & 0x02))
      g();
}

/* { dg-final { scan-tree-dump "optimizing double bit test" } } */

