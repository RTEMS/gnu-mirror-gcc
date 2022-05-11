/* Define per-register tables for data flow info and register allocation.
   Copyright (C) 2022 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_MODE_ALIGN_H
#define GCC_MODE_ALIGN_H

/* Target-dependent globals.  */
struct target_mode_align {
    bool any_modes_align;
    bool all_modes_align;
    /* Record for each mode whether it's accesses require strict alignment.  */
    bool x_mode_strict_align[NUM_MACHINE_MODES];
};

extern struct target_mode_align default_target_mode_align;
#if SWITCHABLE_TARGET
extern struct target_mode_align *this_target_mode_align;
#else
#define this_target_mode_align (&default_target_mode_align)
#endif

ALWAYS_INLINE bool
all_modes_strict_align ()
{
  return this_target_mode_align->all_modes_align;
}

ALWAYS_INLINE bool
any_modes_strict_align ()
{
  return this_target_mode_align->any_modes_align;
}

ALWAYS_INLINE bool
mode_strict_align (machine_mode x)
{
  gcc_checking_assert (x != BLKmode && x != VOIDmode);
  return this_target_mode_align->x_mode_strict_align[x];
}

ALWAYS_INLINE bool
mode_strict_align_inc_blk_void (machine_mode x)
{
  if (x == BLKmode || x == VOIDmode)
    return false;
  return mode_strict_align (x);
}

#endif /* GCC_MODE_ALIGN_H */

