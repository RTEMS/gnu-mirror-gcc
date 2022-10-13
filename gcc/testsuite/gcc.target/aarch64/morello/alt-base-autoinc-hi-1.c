/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

PRE_MODIFY_OFFSET (uint16_t, m129)
PRE_MODIFY_OFFSET (uint16_t, m128)
PRE_MODIFY_OFFSET (uint16_t, m1)
PRE_MODIFY_OFFSET (uint16_t, 1)
PRE_MODIFY_OFFSET (uint16_t, 127)
PRE_MODIFY_OFFSET (uint16_t, 128)
POST_MODIFY_OFFSET (uint16_t, m129)
POST_MODIFY_OFFSET (uint16_t, m128)
POST_MODIFY_OFFSET (uint16_t, m1)
POST_MODIFY_OFFSET (uint16_t, 1)
POST_MODIFY_OFFSET (uint16_t, 127)
POST_MODIFY_OFFSET (uint16_t, 128)

/* { dg-final { scan-assembler-not {\][!,]} } } */
