/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

PRE_MODIFY_OFFSET (uint32_t, m65)
PRE_MODIFY_OFFSET (uint32_t, m64)
PRE_MODIFY_OFFSET (uint32_t, m1)
PRE_MODIFY_OFFSET (uint32_t, 1)
PRE_MODIFY_OFFSET (uint32_t, 63)
PRE_MODIFY_OFFSET (uint32_t, 64)
POST_MODIFY_OFFSET (uint32_t, m65)
POST_MODIFY_OFFSET (uint32_t, m64)
POST_MODIFY_OFFSET (uint32_t, m1)
POST_MODIFY_OFFSET (uint32_t, 1)
POST_MODIFY_OFFSET (uint32_t, 63)
POST_MODIFY_OFFSET (uint32_t, 64)

/* { dg-final { scan-assembler-not {\][!,]} } } */
