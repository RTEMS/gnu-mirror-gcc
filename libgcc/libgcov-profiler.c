/* Routines required for instrumenting a program.  */
/* Compile this one with gcc.  */
/* Copyright (C) 1989-2023 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#include "libgcov.h"
#if !defined(inhibit_libc)

#ifndef floor_log2

/*
 * If value is less then 8 we increment corresponding counter
 * otherwise we take its logarithm and increment corresponding counter
 */

/* For convenience, define 0 -> word_size.  */
static inline int clz_hwi(gcov_type_unsigned x) {
  if (x == 0)
    return sizeof(gcov_type_unsigned) * 8;
  if (sizeof(gcov_type_unsigned) == sizeof(long))
    return __builtin_clzl(x);
  if (sizeof(gcov_type_unsigned) == sizeof(long long))
    return __builtin_clzll(x);
  else
    return __builtin_clz(x);
}

static inline gcov_type_unsigned floor_log2(gcov_type_unsigned x) {
  return sizeof(gcov_type_unsigned) * 8 - clz_hwi(x) - 1;
}

#endif

#ifdef L_gcov_histogram_profiler
/*
 * If value is less then 8 we increment corresponding counter
 * otherwise we take its logarithm and increment corresponding counter
 */

void __gcov_histogram_profiler(gcov_type *counters, gcov_type value,
                               gcov_type hist_sizes) {
  gcc_assert(hist_sizes >= 0 && value >= 0);
  gcov_type_unsigned hist_size = hist_sizes;
  gcov_type_unsigned u_value = value;
  // uncode section sizes
  gcov_type_unsigned mask = (((gcov_type_unsigned)1) << 10) - 1;
  gcov_type_unsigned lin_size = hist_size & mask;
  gcov_type_unsigned exp_size = (hist_size >> 10) & mask;
  gcov_type_unsigned mod_size = hist_size >> 20;
  gcov_type_unsigned tot_size = exp_size + lin_size;
  // add to the regular histogram
  if (u_value < lin_size) {
    counters[value]++;
  } else {
    gcov_type_unsigned pow2 = floor_log2(u_value);
    gcov_type_unsigned lin_pow2 = floor_log2(lin_size - 1);
    if (lin_size < tot_size && pow2 == lin_pow2) {
      counters[lin_size]++;
    } else {
      if ((lin_pow2 - lin_size) + tot_size > pow2) {
        counters[pow2 + (lin_size - lin_pow2) - 1]++;
      } else {
        counters[tot_size - 1]++;
      }
    }
  }
  // add to the modular histogram
  counters[tot_size + (u_value % mod_size)]++;
}

#endif

#if defined(L_gcov_histogram_profiler_atomic) && GCOV_SUPPORTS_ATOMIC

void __gcov_histogram_profiler_atomic(gcov_type *counters, gcov_type value,
                                      gcov_type hist_sizes) {
  gcc_assert(hist_sizes >= 0 && value >= 0);
  gcov_type_unsigned hist_size = hist_sizes;
  gcov_type_unsigned u_value = value;
  // uncode section sizes
  gcov_type_unsigned mask = (((gcov_type_unsigned)1) << 10) - 1;
  gcov_type_unsigned lin_size = hist_size & mask;
  gcov_type_unsigned exp_size = (hist_size >> 10) & mask;
  gcov_type_unsigned mod_size = hist_size >> 20;
  gcov_type_unsigned tot_size = exp_size + lin_size;
  // add to the regular histogram
  if (u_value < lin_size) {
    __atomic_fetch_add(&counters[value], 1, __ATOMIC_RELAXED);
  } else {
    gcov_type_unsigned pow2 = floor_log2(u_value);
    gcov_type_unsigned lin_pow2 = floor_log2(lin_size - 1);
    if ((lin_pow2 - lin_size) + tot_size > pow2) {
      __atomic_fetch_add(&counters[pow2 + (lin_size - lin_pow2) - 1], 1,
                         __ATOMIC_RELAXED);
    } else {
      __atomic_fetch_add(&counters[tot_size - 1], 1, __ATOMIC_RELAXED);
    }
  }
  // add to the modular histogram
  __atomic_fetch_add(&counters[tot_size + (u_value % mod_size)], 1,
                     __ATOMIC_RELAXED);
}

#endif

#ifdef L_gcov_interval_profiler
/* If VALUE is in interval <START, START + STEPS - 1>, then increases the
   corresponding counter in COUNTERS.  If the VALUE is above or below
   the interval, COUNTERS[STEPS] or COUNTERS[STEPS + 1] is increased
   instead.  */

void
__gcov_interval_profiler (gcov_type *counters, gcov_type value,
                          int start, unsigned steps)
{
  gcov_type delta = value - start;
  if (delta < 0)
    counters[steps + 1]++;
  else if (delta >= steps)
    counters[steps]++;
  else
    counters[delta]++;
}
#endif

#if defined(L_gcov_interval_profiler_atomic) && GCOV_SUPPORTS_ATOMIC
/* If VALUE is in interval <START, START + STEPS - 1>, then increases the
   corresponding counter in COUNTERS.  If the VALUE is above or below
   the interval, COUNTERS[STEPS] or COUNTERS[STEPS + 1] is increased
   instead.  Function is thread-safe.  */

void
__gcov_interval_profiler_atomic (gcov_type *counters, gcov_type value,
				 int start, unsigned steps)
{
  gcov_type delta = value - start;
  if (delta < 0)
    __atomic_fetch_add (&counters[steps + 1], 1, __ATOMIC_RELAXED);
  else if (delta >= steps)
    __atomic_fetch_add (&counters[steps], 1, __ATOMIC_RELAXED);
  else
    __atomic_fetch_add (&counters[delta], 1, __ATOMIC_RELAXED);
}
#endif

#ifdef L_gcov_pow2_profiler
/* If VALUE is a power of two, COUNTERS[1] is incremented.  Otherwise
   COUNTERS[0] is incremented.  */

void
__gcov_pow2_profiler (gcov_type *counters, gcov_type value)
{
  if (value == 0 || (value & (value - 1)))
    counters[0]++;
  else
    counters[1]++;
}
#endif

#if defined(L_gcov_pow2_profiler_atomic) && GCOV_SUPPORTS_ATOMIC
/* If VALUE is a power of two, COUNTERS[1] is incremented.  Otherwise
   COUNTERS[0] is incremented.  Function is thread-safe.  */

void
__gcov_pow2_profiler_atomic (gcov_type *counters, gcov_type value)
{
  if (value == 0 || (value & (value - 1)))
    __atomic_fetch_add (&counters[0], 1, __ATOMIC_RELAXED);
  else
    __atomic_fetch_add (&counters[1], 1, __ATOMIC_RELAXED);
}
#endif

/* Tries to determine N most commons value among its inputs.  */

static inline void
__gcov_topn_values_profiler_body (gcov_type *counters, gcov_type value,
				  int use_atomic)
{
  gcov_topn_add_value (counters, value, 1, use_atomic, 1);
}

#ifdef L_gcov_topn_values_profiler
void
__gcov_topn_values_profiler (gcov_type *counters, gcov_type value)
{
  __gcov_topn_values_profiler_body (counters, value, 0);
}
#endif

#if defined(L_gcov_topn_values_profiler_atomic) && GCOV_SUPPORTS_ATOMIC

/* Update one value profilers (COUNTERS) for a given VALUE.

   CAVEAT: Following function is not thread-safe, only total number
   of executions (COUNTERS[2]) is update with an atomic instruction.
   Problem is that one cannot atomically update two counters
   (COUNTERS[0] and COUNTERS[1]), for more information please read
   following email thread:
   https://gcc.gnu.org/ml/gcc-patches/2016-08/msg00024.html.  */

void
__gcov_topn_values_profiler_atomic (gcov_type *counters, gcov_type value)
{
  __gcov_topn_values_profiler_body (counters, value, 1);
}
#endif

#ifdef L_gcov_indirect_call_profiler_v4

/* These two variables are used to actually track caller and callee.  Keep
   them in TLS memory so races are not common (they are written to often).
   The variables are set directly by GCC instrumented code, so declaration
   here must match one in tree-profile.c  */

#if defined(HAVE_CC_TLS) && !defined (USE_EMUTLS)
__thread
#endif
struct indirect_call_tuple __gcov_indirect_call;

/* By default, the C++ compiler will use function addresses in the
   vtable entries.  Setting TARGET_VTABLE_USES_DESCRIPTORS to nonzero
   tells the compiler to use function descriptors instead.  The value
   of this macro says how many words wide the descriptor is (normally 2).

   It is assumed that the address of a function descriptor may be treated
   as a pointer to a function.  */

/* Tries to determine the most common value among its inputs. */
static inline void
__gcov_indirect_call_profiler_body (gcov_type value, void *cur_func,
				    int use_atomic)
{
  /* If the C++ virtual tables contain function descriptors then one
     function may have multiple descriptors and we need to dereference
     the descriptors to see if they point to the same function.  */
  if (cur_func == __gcov_indirect_call.callee
      || (__LIBGCC_VTABLE_USES_DESCRIPTORS__
	  && *(void **) cur_func == *(void **) __gcov_indirect_call.callee))
    __gcov_topn_values_profiler_body (__gcov_indirect_call.counters, value,
				      use_atomic);

  __gcov_indirect_call.callee = NULL;
}

void
__gcov_indirect_call_profiler_v4 (gcov_type value, void *cur_func)
{
  __gcov_indirect_call_profiler_body (value, cur_func, 0);
}

#if GCOV_SUPPORTS_ATOMIC
void
__gcov_indirect_call_profiler_v4_atomic (gcov_type value, void *cur_func)
{
  __gcov_indirect_call_profiler_body (value, cur_func, 1);
}
#endif

#endif

#ifdef L_gcov_time_profiler

/* Counter for first visit of each function.  */
gcov_type __gcov_time_profiler_counter ATTRIBUTE_HIDDEN;

#endif

#ifdef L_gcov_average_profiler
/* Increase corresponding COUNTER by VALUE.  FIXME: Perhaps we want
   to saturate up.  */

void
__gcov_average_profiler (gcov_type *counters, gcov_type value)
{
  counters[0] += value;
  counters[1] ++;
}
#endif

#if defined(L_gcov_average_profiler_atomic) && GCOV_SUPPORTS_ATOMIC
/* Increase corresponding COUNTER by VALUE.  FIXME: Perhaps we want
   to saturate up.  Function is thread-safe.  */

void
__gcov_average_profiler_atomic (gcov_type *counters, gcov_type value)
{
  __atomic_fetch_add (&counters[0], value, __ATOMIC_RELAXED);
  __atomic_fetch_add (&counters[1], 1, __ATOMIC_RELAXED);
}
#endif

#ifdef L_gcov_ior_profiler
/* Bitwise-OR VALUE into COUNTER.  */

void
__gcov_ior_profiler (gcov_type *counters, gcov_type value)
{
  *counters |= value;
}
#endif

#if defined(L_gcov_ior_profiler_atomic) && GCOV_SUPPORTS_ATOMIC
/* Bitwise-OR VALUE into COUNTER.  Function is thread-safe.  */

void
__gcov_ior_profiler_atomic (gcov_type *counters, gcov_type value)
{
  __atomic_fetch_or (&counters[0], value, __ATOMIC_RELAXED);
}
#endif


#endif /* inhibit_libc */
