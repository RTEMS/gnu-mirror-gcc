/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */

#define ALT_BASE
#include "load-store-utils.h"

#include <arm_neon.h>

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_OFFSET (int8x16x3_t, m257)
STORE_ZERO_OFFSET (int8x16x3_t, m256)
STORE_ZERO_OFFSET (int8x16x3_t, m255)
STORE_ZERO_OFFSET (int8x16x3_t, m1)
STORE_ZERO_OFFSET (int8x16x3_t, 1)
STORE_ZERO_OFFSET (int8x16x3_t, 223)
STORE_ZERO_OFFSET (int8x16x3_t, 224)
STORE_ZERO_OFFSET (int8x16x3_t, 225)
STORE_ZERO_OFFSET (int8x16x3_t, 256)

STORE_ZERO_INDEX (int8x16x3_t, int32_t, 1)
STORE_ZERO_INDEX (int8x16x3_t, uint32_t, 1)
STORE_ZERO_INDEX (int8x16x3_t, uint64_t, 1)

STORE_ZERO_INDEX (int8x16x3_t, int32_t, 2)
STORE_ZERO_INDEX (int8x16x3_t, uint32_t, 2)
STORE_ZERO_INDEX (int8x16x3_t, uint64_t, 2)

STORE_ZERO_INDEX (int8x16x3_t, int32_t, 4)
STORE_ZERO_INDEX (int8x16x3_t, uint32_t, 4)
STORE_ZERO_INDEX (int8x16x3_t, uint64_t, 4)

STORE_ZERO_INDEX (int8x16x3_t, int32_t, 8)
STORE_ZERO_INDEX (int8x16x3_t, uint32_t, 8)
STORE_ZERO_INDEX (int8x16x3_t, uint64_t, 8)

STORE_ZERO_INDEX (int8x16x3_t, int32_t, 16)
STORE_ZERO_INDEX (int8x16x3_t, uint32_t, 16)
STORE_ZERO_INDEX (int8x16x3_t, uint64_t, 16)
