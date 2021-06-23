..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _openrisc-options:

OpenRISC Options
^^^^^^^^^^^^^^^^

.. index:: OpenRISC Options

These options are defined for OpenRISC:

.. option:: -mboard=name

  Configure a board specific runtime.  This will be passed to the linker for
  newlib board library linking.  The default is ``or1ksim``.

.. option:: -mnewlib

  This option is ignored; it is for compatibility purposes only.  This used to
  select linker and preprocessor options for use with newlib.

.. option:: -msoft-div, -mhard-div

  Select software or hardware divide ( ``l.div``, ``l.divu`` ) instructions.
  This default is hardware divide.

.. option:: -msoft-mul, -mhard-mul

  Select software or hardware multiply ( ``l.mul``, ``l.muli`` ) instructions.
  This default is hardware multiply.

.. option:: -msoft-float, -mhard-float

  Select software or hardware for floating point operations.
  The default is software.

.. option:: -mdouble-float

  When :option:`-mhard-float` is selected, enables generation of double-precision
  floating point instructions.  By default functions from :samp:`libgcc` are used
  to perform double-precision floating point operations.

.. option:: -munordered-float

  When :option:`-mhard-float` is selected, enables generation of unordered
  floating point compare and set flag ( ``lf.sfun*`` ) instructions.  By default
  functions from :samp:`libgcc` are used to perform unordered floating point
  compare and set flag operations.

.. option:: -mcmov

  Enable generation of conditional move ( ``l.cmov`` ) instructions.  By
  default the equivalent will be generated using set and branch.

.. option:: -mror

  Enable generation of rotate right ( ``l.ror`` ) instructions.  By default
  functions from :samp:`libgcc` are used to perform rotate right operations.

.. option:: -mrori

  Enable generation of rotate right with immediate ( ``l.rori`` ) instructions.
  By default functions from :samp:`libgcc` are used to perform rotate right with
  immediate operations.

.. option:: -msext

  Enable generation of sign extension ( ``l.ext*`` ) instructions.  By default
  memory loads are used to perform sign extension.

.. option:: -msfimm

  Enable generation of compare and set flag with immediate ( ``l.sf*i`` )
  instructions.  By default extra instructions will be generated to store the
  immediate to a register first.

.. option:: -mshftimm

  Enable generation of shift with immediate ( ``l.srai``, ``l.srli``,
  ``l.slli`` ) instructions.  By default extra instructions will be generated
  to store the immediate to a register first.