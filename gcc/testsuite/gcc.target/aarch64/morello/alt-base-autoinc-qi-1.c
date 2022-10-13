/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

PRE_MODIFY_OFFSET (uint8_t, m257)
PRE_MODIFY_OFFSET (uint8_t, m256)
PRE_MODIFY_OFFSET (uint8_t, m1)
PRE_MODIFY_OFFSET (uint8_t, 1)
PRE_MODIFY_OFFSET (uint8_t, 255)
PRE_MODIFY_OFFSET (uint8_t, 256)
POST_MODIFY_OFFSET (uint8_t, m257)
POST_MODIFY_OFFSET (uint8_t, m256)
POST_MODIFY_OFFSET (uint8_t, m1)
POST_MODIFY_OFFSET (uint8_t, 1)
POST_MODIFY_OFFSET (uint8_t, 255)
POST_MODIFY_OFFSET (uint8_t, 256)

/* { dg-final { scan-assembler-not {\][!,]} } } */
