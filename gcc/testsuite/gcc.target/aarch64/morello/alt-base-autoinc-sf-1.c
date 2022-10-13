/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

PRE_MODIFY_OFFSET (float, m65)
PRE_MODIFY_OFFSET (float, m64)
PRE_MODIFY_OFFSET (float, m1)
PRE_MODIFY_OFFSET (float, 1)
PRE_MODIFY_OFFSET (float, 63)
PRE_MODIFY_OFFSET (float, 64)
POST_MODIFY_OFFSET (float, m65)
POST_MODIFY_OFFSET (float, m64)
POST_MODIFY_OFFSET (float, m1)
POST_MODIFY_OFFSET (float, 1)
POST_MODIFY_OFFSET (float, 63)
POST_MODIFY_OFFSET (float, 64)

/* { dg-final { scan-assembler-not {c[0-9]+\][!,]} } } */
