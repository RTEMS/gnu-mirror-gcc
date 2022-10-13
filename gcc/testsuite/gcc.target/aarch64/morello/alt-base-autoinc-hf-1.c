/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef __fp16 fp16;

PRE_MODIFY_OFFSET (fp16, m129)
PRE_MODIFY_OFFSET (fp16, m128)
PRE_MODIFY_OFFSET (fp16, m1)
PRE_MODIFY_OFFSET (fp16, 1)
PRE_MODIFY_OFFSET (fp16, 127)
PRE_MODIFY_OFFSET (fp16, 128)
POST_MODIFY_OFFSET (fp16, m129)
POST_MODIFY_OFFSET (fp16, m128)
POST_MODIFY_OFFSET (fp16, m1)
POST_MODIFY_OFFSET (fp16, 1)
POST_MODIFY_OFFSET (fp16, 127)
POST_MODIFY_OFFSET (fp16, 128)

/* { dg-final { scan-assembler-not {c[0-9]+\][!,]} } } */
