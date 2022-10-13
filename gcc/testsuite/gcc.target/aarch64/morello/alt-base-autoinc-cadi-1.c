/* { dg-do assemble } */
/* { dg-additional-options "-fno-ivopts -save-temps" } */
/* { dg-require-effective-target cheri_capability_hybrid } */

#define ALT_BASE
#include "load-store-utils.h"

typedef __uintcap_t uintcap_t;

PRE_MODIFY_OFFSET (uintcap_t, m17)
PRE_MODIFY_OFFSET (uintcap_t, m16)
PRE_MODIFY_OFFSET (uintcap_t, m1)
PRE_MODIFY_OFFSET (uintcap_t, 1)
PRE_MODIFY_OFFSET (uintcap_t, 15)
PRE_MODIFY_OFFSET (uintcap_t, 16)
POST_MODIFY_OFFSET (uintcap_t, m17)
POST_MODIFY_OFFSET (uintcap_t, m16)
POST_MODIFY_OFFSET (uintcap_t, m1)
POST_MODIFY_OFFSET (uintcap_t, 1)
POST_MODIFY_OFFSET (uintcap_t, 15)
POST_MODIFY_OFFSET (uintcap_t, 16)

/* { dg-final { scan-assembler-not {c[0-9]+\][!,]} } } */
