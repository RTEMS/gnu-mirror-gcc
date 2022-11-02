/* DWARF2 frame unwind data structure.
   Copyright (C) 1997-2020 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

typedef unsigned int _Unwind_Reg;
/* The result of interpreting the frame unwind info for a frame.
   This is all symbolic at this point, as none of the values can
   be resolved until the target pc is located.  */
typedef struct
{
  /* Each register save state can be described in terms of a CFA slot,
     another register, or a location expression.  */
  struct frame_state_reg_info
  {
    struct {
      union {
	_Unwind_Reg reg;
	_Unwind_SwordAddr offset;
	const unsigned char *exp;
#ifdef __CHERI_PURE_CAPABILITY__
	struct {
	    /* Only used for REG_SAVED_SETADDRESS.
	       N.b. two SwordAddr's are the same size as a pointer in pure
	       capability, hence this is not increasing the size of the frame
	       state structure.  */
	    _Unwind_SwordAddr offset1;
	    _Unwind_SwordAddr offset2;
	} offs;
#endif
      } loc;
      enum {
	REG_UNSAVED,
	REG_SAVED_OFFSET,
	REG_SAVED_REG,
	REG_SAVED_EXP,
	REG_SAVED_VAL_OFFSET,
	REG_SAVED_VAL_EXP,
#ifdef __CHERI_PURE_CAPABILITY__
	REG_SAVED_SET_ADDRESS,
#endif
	REG_UNDEFINED
      } how;
    } reg[__LIBGCC_DWARF_FRAME_REGISTERS__+1];

    /* Used to implement DW_CFA_remember_state.  */
    struct frame_state_reg_info *prev;

    /* The CFA can be described in terms of a reg+offset or a
       location expression.  */
    _Unwind_SwordAddr cfa_offset;
    _Unwind_Reg cfa_reg;
    const unsigned char *cfa_exp;
    enum {
      CFA_UNSET,
      CFA_REG_OFFSET,
      CFA_EXP
    } cfa_how;
  } regs;

  /* The PC described by the current frame state.
     This is read from the CFA data or FDE pc range.  On capability
     architectures it can be a pointer, but will never be an executable pointer
     (since reading from the DWARF information can only have the permissions of
     the capability we use for a base, and we never use an executable
     capability for such a base).  For non-capability architectures
     _Unwind_Address is the same as a pointer.  */
  _Unwind_Address pc;

  /* The information we care about from the CIE/FDE.  */
  _Unwind_Personality_Fn personality;
  _Unwind_SwordAddr data_align;
  _Unwind_WordAddr code_align;
  _Unwind_Reg retaddr_column;
  unsigned char fde_encoding;
  unsigned char lsda_encoding;
  unsigned char saw_z;
  unsigned char signal_frame;
  void *eh_ptr;
} _Unwind_FrameState;

