/* { dg-do compile } */
/* { dg-additional-options "-Werror" } */

#include <string.h>

static char commonNameW[] = { 'J','u','a','n',' ','L','a','n','g',0 };

void func(int z)
{
     int a  = {(__intcap_t) 0 * 3};
     int b  = {(__intcap_t) 2 * 3};
     int c  = {(__intcap_t) 0 * sizeof(int)};
     int d  = {(__intcap_t) 2 * sizeof(d)};
     int e  = {(__intcap_t) 0 * z};
     int f  = {(__intcap_t) 2 * z};
     int g  = {(__intcap_t) strlen(commonNameW) * 3};
     int h  = {(__intcap_t) strlen(commonNameW) * 3};
     int i  = {(__intcap_t) strlen(commonNameW) * sizeof(int)};
     int j  = {(__intcap_t) strlen(commonNameW) * sizeof(f)};
     int k  = {(__intcap_t) strlen(commonNameW) * z};
     int l  = {(__intcap_t) strlen(commonNameW) * z};
}