/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch" } */

int IsHTMLWhitespaceNoRange(int aChar)
{
  return aChar == 0x0001 || aChar == 0x000A ||
         aChar == 0x000C || aChar == 0x000E ||
         aChar == 0x0020;
}

/* { dg-final { scan-tree-dump "Condition chain \\(at .*if-to-switch-2.c:7\\) with 5 conditions \\(3 BBs\\) transformed into a switch statement." "iftoswitch" } } */
