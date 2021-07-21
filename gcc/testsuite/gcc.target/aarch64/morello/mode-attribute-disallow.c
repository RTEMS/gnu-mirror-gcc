/* { dg-do compile } */
/* TODO Ensure only ran with -mfake-capability or with purecap. */

typedef unsigned myval __attribute__((__mode__(__pointer__))); /* { dg-error "mode 'pointer' applied to inappropriate type" "pointer mode attribute disallowed for unsigned" } */
