/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

PRE_MODIFY_OFFSET (double, m33)
PRE_MODIFY_OFFSET (double, m32)
PRE_MODIFY_OFFSET (double, m1)
PRE_MODIFY_OFFSET (double, 1)
PRE_MODIFY_OFFSET (double, 31)
PRE_MODIFY_OFFSET (double, 32)
POST_MODIFY_OFFSET (double, m33)
POST_MODIFY_OFFSET (double, m32)
POST_MODIFY_OFFSET (double, m1)
POST_MODIFY_OFFSET (double, 1)
POST_MODIFY_OFFSET (double, 31)
POST_MODIFY_OFFSET (double, 32)

/* { dg-final { scan-assembler-not {c[0-9]+\][!,]} } } */
