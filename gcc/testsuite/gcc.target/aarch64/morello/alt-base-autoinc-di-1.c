/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

PRE_MODIFY_OFFSET (uint64_t, m33)
PRE_MODIFY_OFFSET (uint64_t, m32)
PRE_MODIFY_OFFSET (uint64_t, m1)
PRE_MODIFY_OFFSET (uint64_t, 1)
PRE_MODIFY_OFFSET (uint64_t, 31)
PRE_MODIFY_OFFSET (uint64_t, 32)
POST_MODIFY_OFFSET (uint64_t, m33)
POST_MODIFY_OFFSET (uint64_t, m32)
POST_MODIFY_OFFSET (uint64_t, m1)
POST_MODIFY_OFFSET (uint64_t, 1)
POST_MODIFY_OFFSET (uint64_t, 31)
POST_MODIFY_OFFSET (uint64_t, 32)

/* { dg-final { scan-assembler-not {\][!,]} } } */
