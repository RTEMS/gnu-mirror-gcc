..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _target-macros:

Target Description Macros and Functions
---------------------------------------

.. index:: machine description macros

.. index:: target description macros

.. index:: macros, target description

.. index:: tm.h macros

In addition to the file :samp:`{machine}.md`, a machine description
includes a C header file conventionally given the name
:samp:`{machine}.h` and a C source file named :samp:`{machine}.c`.
The header file defines numerous macros that convey the information
about the target machine that does not fit into the scheme of the
:samp:`.md` file.  The file :samp:`tm.h` should be a link to
:samp:`{machine}.h`.  The header file :samp:`config.h` includes
:samp:`tm.h` and most compiler source files include :samp:`config.h`.  The
source file defines a variable ``targetm``, which is a structure
containing pointers to functions and data relating to the target
machine.  :samp:`{machine}.c` should also contain their definitions,
if they are not defined elsewhere in GCC, and other functions called
through the macros defined in the :samp:`.h` file.

.. toctree::
  :maxdepth: 2

  target-description-macros-and-functions/the-global-targetm-variable
  target-description-macros-and-functions/controlling-the-compilation-driver-gcc
  target-description-macros-and-functions/run-time-target-specification
  target-description-macros-and-functions/defining-data-structures-for-per-function-information
  target-description-macros-and-functions/storage-layout
  target-description-macros-and-functions/layout-of-source-language-data-types
  target-description-macros-and-functions/register-usage
  target-description-macros-and-functions/register-classes
  target-description-macros-and-functions/stack-layout-and-calling-conventions
  target-description-macros-and-functions/implementing-the-varargs-macros
  target-description-macros-and-functions/support-for-nested-functions
  target-description-macros-and-functions/implicit-calls-to-library-routines
  target-description-macros-and-functions/addressing-modes
  target-description-macros-and-functions/anchored-addresses
  target-description-macros-and-functions/condition-code-status
  target-description-macros-and-functions/describing-relative-costs-of-operations
  target-description-macros-and-functions/adjusting-the-instruction-scheduler
  target-description-macros-and-functions/dividing-the-output-into-sections-texts-data
  target-description-macros-and-functions/position-independent-code
  target-description-macros-and-functions/defining-the-output-assembler-language
  target-description-macros-and-functions/controlling-debugging-information-format
  target-description-macros-and-functions/cross-compilation-and-floating-point
  target-description-macros-and-functions/mode-switching-instructions
  target-description-macros-and-functions/defining-target-specific-uses-of-attribute
  target-description-macros-and-functions/emulating-tls
  target-description-macros-and-functions/defining-coprocessor-specifics-for-mips-targets
  target-description-macros-and-functions/parameters-for-precompiled-header-validity-checking
  target-description-macros-and-functions/c++-abi-parameters
  target-description-macros-and-functions/d-abi-parameters
  target-description-macros-and-functions/adding-support-for-named-address-spaces
  target-description-macros-and-functions/miscellaneous-parameters