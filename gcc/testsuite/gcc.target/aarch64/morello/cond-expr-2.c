#include <stdint.h>

__intcap_t icap;
__uintcap_t ucap;
int8_t i8;
uint8_t u8;
int16_t i16;
uint16_t u16;
int32_t i32;
uint32_t u32;
int64_t i64;
uint64_t u64;
__int128 i128;
unsigned __int128 u128;

void foo(int cond) {
  _Static_assert(_Generic (cond ? ucap : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? ucap : icap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : icap, __intcap_t: 1, default: 0));

  _Static_assert(!_Generic (cond ? ucap : ucap, __intcap_t: 1, default: 0));
  _Static_assert(!_Generic (cond ? ucap : icap, __intcap_t: 1, default: 0));
  _Static_assert(!_Generic (cond ? icap : ucap, __intcap_t: 1, default: 0));
  _Static_assert(!_Generic (cond ? icap : icap, __uintcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? ucap : u8, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? ucap : i8, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : u8, __intcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : i8, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? ucap : u16, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? ucap : i16, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : u16, __intcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : i16, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? ucap : u32, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? ucap : i32, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : u32, __intcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : i32, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? ucap : u64, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? ucap : i64, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : u64, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : i64, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? ucap : u128, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? ucap : i128, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : u128, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? icap : i128, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? u8 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i8 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? u8 : icap, __intcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i8 : icap, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? u16 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i16 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? u16 : icap, __intcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i16 : icap, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? u32 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i32 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? u32 : icap, __intcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i32 : icap, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? u64 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i64 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? u64 : icap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i64 : icap, __intcap_t: 1, default: 0));

  _Static_assert(_Generic (cond ? u128 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i128 : ucap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? u128 : icap, __uintcap_t: 1, default: 0));
  _Static_assert(_Generic (cond ? i128 : icap, __intcap_t: 1, default: 0));
}
