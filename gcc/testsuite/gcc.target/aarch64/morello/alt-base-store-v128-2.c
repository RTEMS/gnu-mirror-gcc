/* { dg-do assemble } */
/* { dg-additional-options "-save-temps" } */

#include <arm_neon.h>

#define ALT_BASE
#include "load-store-utils.h"

/* Check for valid asm, but don't mandate a particular sequence.  */
STORE_ZERO_OFFSET (int8x16_t, m257)
STORE_ZERO_OFFSET (int16x8_t, m256)
STORE_ZERO_OFFSET (int32x4_t, m255)
STORE_ZERO_OFFSET (int64x2_t, m1)
STORE_ZERO_OFFSET (float16x8_t, 1)
STORE_ZERO_OFFSET (float32x4_t, 247)
STORE_ZERO_OFFSET (float64x2_t, 248)
STORE_ZERO_OFFSET (int8x16_t, 249)
STORE_ZERO_OFFSET (int8x16_t, 256)
STORE_ZERO_OFFSET (int8x16_t, 511)
STORE_ZERO_OFFSET (int8x16_t, 512)

STORE_ZERO_INDEX (int8x16_t, int32_t, 1)
STORE_ZERO_INDEX (int8x16_t, uint32_t, 1)
STORE_ZERO_INDEX (int8x16_t, uint64_t, 1)

STORE_ZERO_INDEX (int8x16_t, int32_t, 2)
STORE_ZERO_INDEX (int8x16_t, uint32_t, 2)
STORE_ZERO_INDEX (int8x16_t, uint64_t, 2)

STORE_ZERO_INDEX (int8x16_t, int32_t, 4)
STORE_ZERO_INDEX (int8x16_t, uint32_t, 4)
STORE_ZERO_INDEX (int8x16_t, uint64_t, 4)

STORE_ZERO_INDEX (int8x16_t, int32_t, 8)
STORE_ZERO_INDEX (int8x16_t, uint32_t, 8)
STORE_ZERO_INDEX (int8x16_t, uint64_t, 8)

STORE_ZERO_INDEX (int8x16_t, int32_t, 16)
STORE_ZERO_INDEX (int8x16_t, uint32_t, 16)
STORE_ZERO_INDEX (int8x16_t, uint64_t, 16)
