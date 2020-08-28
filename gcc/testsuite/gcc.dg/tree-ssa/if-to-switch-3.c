/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-iftoswitch" } */

int IsHTMLWhitespace(int aChar)
{
  return aChar == 0x0009 || aChar == 0x000A ||
         aChar == 0x000C || aChar == 0x000D ||
         aChar == 0x0020 || aChar == 0x0030;
}

/* { dg-final { scan-tree-dump "Condition chain \\(at .*if-to-switch-3.c:8\\) with 5 conditions \\(3 BBs\\) transformed into a switch statement." "iftoswitch" } } */
