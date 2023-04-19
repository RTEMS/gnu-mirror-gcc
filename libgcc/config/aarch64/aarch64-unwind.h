/* Copyright (C) 2017-2023 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

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

#if !defined (AARCH64_UNWIND_H) && !defined (__ILP32__)
#define AARCH64_UNWIND_H

#define DWARF_REGNUM_AARCH64_RA_STATE 34

#define MD_DEMANGLE_RETURN_ADDR(context, fs, addr) \
  aarch64_demangle_return_addr (context, fs, addr)

static inline int
aarch64_cie_signed_with_b_key (struct _Unwind_Context *context)
{
  const struct dwarf_fde *fde = _Unwind_Find_FDE (context->bases.func,
						  &context->bases);
  if (fde != NULL)
    {
      const struct dwarf_cie *cie = get_cie (fde);
      if (cie != NULL)
	{
	  char *aug_str = cie->augmentation;
	  return strchr (aug_str, 'B') == NULL ? 0 : 1;
	}
    }
  return 0;
}

/* Do AArch64 private extraction on ADDR_WORD based on context info CONTEXT and
   unwind frame info FS.  If ADDR_WORD is signed, we do address authentication
   on it using CFA of current frame.  */

static inline void *
aarch64_demangle_return_addr (struct _Unwind_Context *context,
			      _Unwind_FrameState *fs,
			      _Unwind_Word addr_word)
{
  void *addr = (void *)addr_word;
  const int reg = DWARF_REGNUM_AARCH64_RA_STATE;

  if (fs->regs.how[reg] == REG_UNSAVED)
    return addr;

  /* Return-address signing state is toggled by DW_CFA_GNU_window_save (where
     REG_UNSAVED/REG_UNSAVED_ARCHEXT means RA signing is disabled/enabled),
     or set by a DW_CFA_expression.  */
  if (fs->regs.how[reg] == REG_UNSAVED_ARCHEXT
      || (_Unwind_GetGR (context, reg) & 0x1) != 0)
    {
      _Unwind_Word salt = (_Unwind_Word) context->cfa;
      if (aarch64_cie_signed_with_b_key (context) != 0)
	return __builtin_aarch64_autib1716 (addr, salt);
      return __builtin_aarch64_autia1716 (addr, salt);
    }

  return addr;
}

/* GCS enable flag for chkfeat instruction.  */
#define CHKFEAT_GCS 1

/* SME runtime function local to libgcc, streaming compatible
   and preserves more registers than the base PCS requires, but
   we don't rely on that here.  */
__attribute__ ((visibility ("hidden")))
void __libgcc_arm_za_disable (void);

/* Disable the SME ZA state in case an unwound frame used the ZA
   lazy saving scheme. And unwind the GCS for EH.  */
#undef _Unwind_Frames_Extra
#define _Unwind_Frames_Extra(x)				\
  do							\
    {							\
      __libgcc_arm_za_disable ();			\
      if (__builtin_aarch64_chkfeat (CHKFEAT_GCS) == 0)	\
	{						\
	  for (_Unwind_Word n = (x); n != 0; n--)	\
	    __builtin_aarch64_gcspopm ();		\
	}						\
    }							\
  while (0)

/* On signal entry the OS places a token on the GCS that can be used to
   verify the integrity of the GCS pointer on signal return.  It also
   places the signal handler return address (the restorer that calls the
   signal return syscall) on the GCS so the handler can return.
   Because of this token, each stack frame visited during unwinding has
   exactly one corresponding entry on the GCS, so the frame count is
   the number of entries that will have to be popped at EH return time.

   Note: This depends on the GCS signal ABI of the OS.

   When unwinding across a stack frame for each frame the corresponding
   entry is checked on the GCS against the computed return address from
   the normal stack.  If they don't match then _URC_FATAL_PHASE2_ERROR
   is returned.  This check is omitted if

   1. GCS is disabled. Note: asynchronous GCS disable is supported here
      if GCSPR and the GCS remains readable.
   2. Non-catchable exception where exception_class == 0.  Note: the
      pthread cancellation implementation in glibc sets exception_class
      to 0 when the unwinder is used for cancellation cleanup handling,
      so this allows the GCS to get out of sync during cancellation.
      This weakens security but avoids an ABI break in glibc.
   3. Zero return address which marks the outermost stack frame.
   4. Signal stack frame, the GCS entry is an OS specific token then
      with the top bit set.
 */
#undef _Unwind_Frames_Increment
#define _Unwind_Frames_Increment(exc, context, frames)	\
  do							\
    {							\
      frames++;						\
      if (__builtin_aarch64_chkfeat (CHKFEAT_GCS) != 0	\
	  || exc->exception_class == 0			\
	  || _Unwind_GetIP (context) == 0)		\
	break;						\
      const _Unwind_Word *gcs = __builtin_aarch64_gcspr (); \
      if (_Unwind_IsSignalFrame (context))		\
	{						\
	  if (gcs[frames] >> 63 == 0)			\
	    return _URC_FATAL_PHASE2_ERROR;		\
	}						\
      else						\
	{						\
	  if (gcs[frames] != _Unwind_GetIP (context))	\
	    return _URC_FATAL_PHASE2_ERROR;		\
	}						\
    }							\
  while (0)

#endif /* defined AARCH64_UNWIND_H && defined __ILP32__ */
