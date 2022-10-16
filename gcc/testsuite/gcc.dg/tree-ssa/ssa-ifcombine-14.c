/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ifcombine-details-blocks" } */

void sink();

void same(unsigned char *a)
{
  if (*a & 0x80)
    if (*a & 0x40)
      g();
}

/* { dg-final { scan-tree-dump "optimizing double bit test" } } */

