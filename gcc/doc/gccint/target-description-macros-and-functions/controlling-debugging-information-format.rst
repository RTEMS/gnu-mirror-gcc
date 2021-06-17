..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _debugging-info:

Controlling Debugging Information Format
****************************************

.. prevent bad page break with this line

This describes how to specify debugging information.

.. toctree::
  :maxdepth: 2


.. _all-debuggers:

Macros Affecting All Debugging Formats
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

These macros affect all debugging formats.

.. c:macro:: DBX_REGISTER_NUMBER (regno)

  A C expression that returns the DBX register number for the compiler
  register number :samp:`{regno}`.  In the default macro provided, the value
  of this expression will be :samp:`{regno}` itself.  But sometimes there are
  some registers that the compiler knows about and DBX does not, or vice
  versa.  In such cases, some register may need to have one number in the
  compiler and another for DBX.

  If two registers have consecutive numbers inside GCC, and they can be
  used as a pair to hold a multiword value, then they *must* have
  consecutive numbers after renumbering with ``DBX_REGISTER_NUMBER``.
  Otherwise, debuggers will be unable to access such a pair, because they
  expect register pairs to be consecutive in their own numbering scheme.

  If you find yourself defining ``DBX_REGISTER_NUMBER`` in way that
  does not preserve register pairs, then what you must do instead is
  redefine the actual register numbering scheme.

.. c:macro:: DEBUGGER_AUTO_OFFSET (x)

  A C expression that returns the integer offset value for an automatic
  variable having address :samp:`{x}` (an RTL expression).  The default
  computation assumes that :samp:`{x}` is based on the frame-pointer and
  gives the offset from the frame-pointer.  This is required for targets
  that produce debugging output for DBX and allow the frame-pointer to be
  eliminated when the :option:`-g` option is used.

.. c:macro:: DEBUGGER_ARG_OFFSET (offset, x)

  A C expression that returns the integer offset value for an argument
  having address :samp:`{x}` (an RTL expression).  The nominal offset is
  :samp:`{offset}`.

.. c:macro:: PREFERRED_DEBUGGING_TYPE

  A C expression that returns the type of debugging output GCC should
  produce when the user specifies just :option:`-g`.  Define
  this if you have arranged for GCC to support more than one format of
  debugging output.  Currently, the allowable values are ``DBX_DEBUG``,
  ``DWARF2_DEBUG``, ``XCOFF_DEBUG``, ``VMS_DEBUG``,
  and ``VMS_AND_DWARF2_DEBUG``.

  When the user specifies :option:`-ggdb`, GCC normally also uses the
  value of this macro to select the debugging output format, but with two
  exceptions.  If ``DWARF2_DEBUGGING_INFO`` is defined, GCC uses the
  value ``DWARF2_DEBUG``.  Otherwise, if ``DBX_DEBUGGING_INFO`` is
  defined, GCC uses ``DBX_DEBUG``.

  The value of this macro only affects the default debugging output; the
  user can always get a specific type of output by using :option:`-gstabs`,
  :option:`-gdwarf-2`, :option:`-gxcoff`, or :option:`-gvms`.

.. _dbx-options:

Specific Options for DBX Output
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

These are specific options for DBX output.

.. c:macro:: DBX_DEBUGGING_INFO

  Define this macro if GCC should produce debugging output for DBX
  in response to the :option:`-g` option.

.. c:macro:: XCOFF_DEBUGGING_INFO

  Define this macro if GCC should produce XCOFF format debugging output
  in response to the :option:`-g` option.  This is a variant of DBX format.

.. c:macro:: DEFAULT_GDB_EXTENSIONS

  Define this macro to control whether GCC should by default generate
  GDB's extended version of DBX debugging information (assuming DBX-format
  debugging information is enabled at all).  If you don't define the
  macro, the default is 1: always generate the extended information
  if there is any occasion to.

.. c:macro:: DEBUG_SYMS_TEXT

  Define this macro if all ``.stabs`` commands should be output while
  in the text section.

.. c:macro:: ASM_STABS_OP

  A C string constant, including spacing, naming the assembler pseudo op to
  use instead of ``"\t.stabs\t"`` to define an ordinary debugging symbol.
  If you don't define this macro, ``"\t.stabs\t"`` is used.  This macro
  applies only to DBX debugging information format.

.. c:macro:: ASM_STABD_OP

  A C string constant, including spacing, naming the assembler pseudo op to
  use instead of ``"\t.stabd\t"`` to define a debugging symbol whose
  value is the current location.  If you don't define this macro,
  ``"\t.stabd\t"`` is used.  This macro applies only to DBX debugging
  information format.

.. c:macro:: ASM_STABN_OP

  A C string constant, including spacing, naming the assembler pseudo op to
  use instead of ``"\t.stabn\t"`` to define a debugging symbol with no
  name.  If you don't define this macro, ``"\t.stabn\t"`` is used.  This
  macro applies only to DBX debugging information format.

.. c:macro:: DBX_NO_XREFS

  Define this macro if DBX on your system does not support the construct
  :samp:`xs{tagname}`.  On some systems, this construct is used to
  describe a forward reference to a structure named :samp:`{tagname}`.
  On other systems, this construct is not supported at all.

.. c:macro:: DBX_CONTIN_LENGTH

  A symbol name in DBX-format debugging information is normally
  continued (split into two separate ``.stabs`` directives) when it
  exceeds a certain length (by default, 80 characters).  On some
  operating systems, DBX requires this splitting; on others, splitting
  must not be done.  You can inhibit splitting by defining this macro
  with the value zero.  You can override the default splitting-length by
  defining this macro as an expression for the length you desire.

.. c:macro:: DBX_CONTIN_CHAR

  Normally continuation is indicated by adding a :samp:`\\` character to
  the end of a ``.stabs`` string when a continuation follows.  To use
  a different character instead, define this macro as a character
  constant for the character you want to use.  Do not define this macro
  if backslash is correct for your system.

.. c:macro:: DBX_STATIC_STAB_DATA_SECTION

  Define this macro if it is necessary to go to the data section before
  outputting the :samp:`.stabs` pseudo-op for a non-global static
  variable.

.. c:macro:: DBX_TYPE_DECL_STABS_CODE

  The value to use in the 'code' field of the ``.stabs`` directive
  for a typedef.  The default is ``N_LSYM``.

.. c:macro:: DBX_STATIC_CONST_VAR_CODE

  The value to use in the 'code' field of the ``.stabs`` directive
  for a static variable located in the text section.  DBX format does not
  provide any 'right' way to do this.  The default is ``N_FUN``.

.. c:macro:: DBX_REGPARM_STABS_CODE

  The value to use in the 'code' field of the ``.stabs`` directive
  for a parameter passed in registers.  DBX format does not provide any
  'right' way to do this.  The default is ``N_RSYM``.

.. c:macro:: DBX_REGPARM_STABS_LETTER

  The letter to use in DBX symbol data to identify a symbol as a parameter
  passed in registers.  DBX format does not customarily provide any way to
  do this.  The default is ``'P'``.

.. c:macro:: DBX_FUNCTION_FIRST

  Define this macro if the DBX information for a function and its
  arguments should precede the assembler code for the function.  Normally,
  in DBX format, the debugging information entirely follows the assembler
  code.

.. c:macro:: DBX_BLOCKS_FUNCTION_RELATIVE

  Define this macro, with value 1, if the value of a symbol describing
  the scope of a block ( ``N_LBRAC`` or ``N_RBRAC`` ) should be
  relative to the start of the enclosing function.  Normally, GCC uses
  an absolute address.

.. c:macro:: DBX_LINES_FUNCTION_RELATIVE

  Define this macro, with value 1, if the value of a symbol indicating
  the current line number ( ``N_SLINE`` ) should be relative to the
  start of the enclosing function.  Normally, GCC uses an absolute address.

.. c:macro:: DBX_USE_BINCL

  Define this macro if GCC should generate ``N_BINCL`` and
  ``N_EINCL`` stabs for included header files, as on Sun systems.  This
  macro also directs GCC to output a type number as a pair of a file
  number and a type number within the file.  Normally, GCC does not
  generate ``N_BINCL`` or ``N_EINCL`` stabs, and it outputs a single
  number for a type number.

.. _dbx-hooks:

Open-Ended Hooks for DBX Format
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

These are hooks for DBX format.

.. c:macro:: DBX_OUTPUT_SOURCE_LINE (stream, line, counter)

  A C statement to output DBX debugging information before code for line
  number :samp:`{line}` of the current source file to the stdio stream
  :samp:`{stream}`.  :samp:`{counter}` is the number of time the macro was
  invoked, including the current invocation; it is intended to generate
  unique labels in the assembly output.

  This macro should not be defined if the default output is correct, or
  if it can be made correct by defining ``DBX_LINES_FUNCTION_RELATIVE``.

.. c:macro:: NO_DBX_FUNCTION_END

  Some stabs encapsulation formats (in particular ECOFF), cannot handle the
  ``.stabs "",N_FUN,,0,0,Lscope-function-1`` gdb dbx extension construct.
  On those machines, define this macro to turn this feature off without
  disturbing the rest of the gdb extensions.

.. c:macro:: NO_DBX_BNSYM_ENSYM

  Some assemblers cannot handle the ``.stabd BNSYM/ENSYM,0,0`` gdb dbx
  extension construct.  On those machines, define this macro to turn this
  feature off without disturbing the rest of the gdb extensions.

.. _file-names-and-dbx:

File Names in DBX Format
^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

This describes file names in DBX format.

.. c:macro:: DBX_OUTPUT_MAIN_SOURCE_FILENAME (stream, name)

  A C statement to output DBX debugging information to the stdio stream
  :samp:`{stream}`, which indicates that file :samp:`{name}` is the main source
  file---the file specified as the input file for compilation.
  This macro is called only once, at the beginning of compilation.

  This macro need not be defined if the standard form of output
  for DBX debugging information is appropriate.

  It may be necessary to refer to a label equal to the beginning of the
  text section.  You can use :samp:`assemble_name (stream, ltext_label_name)`
  to do so.  If you do this, you must also set the variable
  :samp:`{used_ltext_label_name}` to ``true``.

.. c:macro:: NO_DBX_MAIN_SOURCE_DIRECTORY

  Define this macro, with value 1, if GCC should not emit an indication
  of the current directory for compilation and current source language at
  the beginning of the file.

.. c:macro:: NO_DBX_GCC_MARKER

  Define this macro, with value 1, if GCC should not emit an indication
  that this object file was compiled by GCC.  The default is to emit
  an ``N_OPT`` stab at the beginning of every source file, with
  :samp:`gcc2_compiled.` for the string and value 0.

.. c:macro:: DBX_OUTPUT_MAIN_SOURCE_FILE_END (stream, name)

  A C statement to output DBX debugging information at the end of
  compilation of the main source file :samp:`{name}`.  Output should be
  written to the stdio stream :samp:`{stream}`.

  If you don't define this macro, nothing special is output at the end
  of compilation, which is correct for most machines.

.. c:macro:: DBX_OUTPUT_NULL_N_SO_AT_MAIN_SOURCE_FILE_END

  Define this macro *instead of* defining
  ``DBX_OUTPUT_MAIN_SOURCE_FILE_END``, if what needs to be output at
  the end of compilation is an ``N_SO`` stab with an empty string,
  whose value is the highest absolute text address in the file.

.. _dwarf:

Macros for DWARF Output
^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here are macros for DWARF output.

.. c:macro:: DWARF2_DEBUGGING_INFO

  Define this macro if GCC should produce dwarf version 2 format
  debugging output in response to the :option:`-g` option.

  .. function:: int TARGET_DWARF_CALLING_CONVENTION (const_tree function)

    Define this to enable the dwarf attribute ``DW_AT_calling_convention`` to
    be emitted for each function.  Instead of an integer return the enum
    value for the ``DW_CC_`` tag.

  To support optional call frame debugging information, you must also
  define ``INCOMING_RETURN_ADDR_RTX`` and either set
  ``RTX_FRAME_RELATED_P`` on the prologue insns if you use RTL for the
  prologue, or call ``dwarf2out_def_cfa`` and ``dwarf2out_reg_save``
  as appropriate from ``TARGET_ASM_FUNCTION_PROLOGUE`` if you don't.

.. c:macro:: DWARF2_FRAME_INFO

  Define this macro to a nonzero value if GCC should always output
  Dwarf 2 frame information.  If ``TARGET_EXCEPT_UNWIND_INFO``
  (see :ref:`exception-region-output`) returns ``UI_DWARF2``, and
  exceptions are enabled, GCC will output this information not matter
  how you define ``DWARF2_FRAME_INFO``.

.. function:: enum unwind_info_type TARGET_DEBUG_UNWIND_INFO (void)

  This hook defines the mechanism that will be used for describing frame
  unwind information to the debugger.  Normally the hook will return
  ``UI_DWARF2`` if DWARF 2 debug information is enabled, and
  return ``UI_NONE`` otherwise.

  A target may return ``UI_DWARF2`` even when DWARF 2 debug information
  is disabled in order to always output DWARF 2 frame information.

  A target may return ``UI_TARGET`` if it has ABI specified unwind tables.
  This will suppress generation of the normal debug frame unwind information.

.. c:macro:: DWARF2_ASM_LINE_DEBUG_INFO

  Define this macro to be a nonzero value if the assembler can generate Dwarf 2
  line debug info sections.  This will result in much more compact line number
  tables, and hence is desirable if it works.

.. c:macro:: DWARF2_ASM_VIEW_DEBUG_INFO

  Define this macro to be a nonzero value if the assembler supports view
  assignment and verification in ``.loc``.  If it does not, but the
  user enables location views, the compiler may have to fallback to
  internal line number tables.

.. function:: int TARGET_RESET_LOCATION_VIEW (rtx_insn *)

  This hook, if defined, enables -ginternal-reset-location-views, and
  uses its result to override cases in which the estimated min insn
  length might be nonzero even when a PC advance (i.e., a view reset)
  cannot be taken for granted.

  If the hook is defined, it must return a positive value to indicate
  the insn definitely advances the PC, and so the view number can be
  safely assumed to be reset; a negative value to mean the insn
  definitely does not advance the PC, and os the view number must not
  be reset; or zero to decide based on the estimated insn length.

  If insn length is to be regarded as reliable, set the hook to
  ``hook_int_rtx_insn_0``.

.. c:var:: bool TARGET_WANT_DEBUG_PUB_SECTIONS

  True if the ``.debug_pubtypes`` and ``.debug_pubnames`` sections
  should be emitted.  These sections are not used on most platforms, and
  in particular GDB does not use them.

.. c:var:: bool TARGET_DELAY_SCHED2

  True if sched2 is not to be run at its normal place.
  This usually means it will be run as part of machine-specific reorg.

.. c:var:: bool TARGET_DELAY_VARTRACK

  True if vartrack is not to be run at its normal place.
  This usually means it will be run as part of machine-specific reorg.

.. c:var:: bool TARGET_NO_REGISTER_ALLOCATION

  True if register allocation and the passes
  following it should not be run.  Usually true only for virtual assembler
  targets.

.. c:macro:: ASM_OUTPUT_DWARF_DELTA (stream, size, label1, label2)

  A C statement to issue assembly directives that create a difference
  :samp:`{lab1}` minus :samp:`{lab2}`, using an integer of the given :samp:`{size}`.

.. c:macro:: ASM_OUTPUT_DWARF_VMS_DELTA (stream, size, label1, label2)

  A C statement to issue assembly directives that create a difference
  between the two given labels in system defined units, e.g. instruction
  slots on IA64 VMS, using an integer of the given size.

.. c:macro:: ASM_OUTPUT_DWARF_OFFSET (stream, size, label, offset, section)

  A C statement to issue assembly directives that create a
  section-relative reference to the given :samp:`{label}` plus :samp:`{offset}`, using
  an integer of the given :samp:`{size}`.  The label is known to be defined in the
  given :samp:`{section}`.

.. c:macro:: ASM_OUTPUT_DWARF_PCREL (stream, size, label)

  A C statement to issue assembly directives that create a self-relative
  reference to the given :samp:`{label}`, using an integer of the given :samp:`{size}`.

.. c:macro:: ASM_OUTPUT_DWARF_DATAREL (stream, size, label)

  A C statement to issue assembly directives that create a reference to the
  given :samp:`{label}` relative to the dbase, using an integer of the given :samp:`{size}`.

.. c:macro:: ASM_OUTPUT_DWARF_TABLE_REF (label)

  A C statement to issue assembly directives that create a reference to
  the DWARF table identifier :samp:`{label}` from the current section.  This
  is used on some systems to avoid garbage collecting a DWARF table which
  is referenced by a function.

.. function:: void TARGET_ASM_OUTPUT_DWARF_DTPREL (FILE *file, int size, rtx x)

  If defined, this target hook is a function which outputs a DTP-relative
  reference to the given TLS symbol of the specified size.

.. _vms-debug:

Macros for VMS Debug Format
^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. prevent bad page break with this line

Here are macros for VMS debug format.

.. c:macro:: VMS_DEBUGGING_INFO

  Define this macro if GCC should produce debugging output for VMS
  in response to the :option:`-g` option.  The default behavior for VMS
  is to generate minimal debug info for a traceback in the absence of
  :option:`-g` unless explicitly overridden with :option:`-g0`.  This
  behavior is controlled by ``TARGET_OPTION_OPTIMIZATION`` and
  ``TARGET_OPTION_OVERRIDE``.