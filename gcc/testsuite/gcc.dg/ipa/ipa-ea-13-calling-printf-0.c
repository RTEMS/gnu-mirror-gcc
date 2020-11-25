/* { dg-do link } */
/* { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } */

#include <stddef.h>
#include <stdio.h>

int
main (int argc, char **argv)
{
  char *filename = "helloworld.txt";
  FILE *f = fopen (filename, "r");
  fclose (f);
}

// This is the incomplete type p=0 because it is memoized and it is an
// optimization. However, I will also match a 1
/// { dg-final { scan-wpa-ipa-dump " record _IO_FILE .. reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record _IO_FILE ... reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record _IO_wide_data .. reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record _IO_wide_data ... reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record _IO_codecvt .. reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record _IO_codecvt ... reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump "void_type. reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record _IO_marker .. reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
/// { dg-final { scan-wpa-ipa-dump " record _IO_marker ... reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
// This is the complete type... but I have condensed it to match .*
/// { dg-final { scan-wpa-ipa-dump "  record FILE .* reason: e=1 g=0 p=. r=1 c=0 v=0 u=0 i=." "type-escape-analysis" } } */
