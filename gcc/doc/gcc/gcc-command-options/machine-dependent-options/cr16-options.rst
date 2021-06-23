..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _cr16-options:

CR16 Options
^^^^^^^^^^^^

.. index:: CR16 Options

These options are defined specifically for the CR16 ports.

.. option:: -mmac

  Enable the use of multiply-accumulate instructions. Disabled by default.

.. option:: -mcr16cplus, -mcr16c

  Generate code for CR16C or CR16C+ architecture. CR16C+ architecture
  is default.

.. option:: -msim

  Links the library libsim.a which is in compatible with simulator. Applicable
  to ELF compiler only.

.. option:: -mint32

  Choose integer type as 32-bit wide.

.. option:: -mbit-ops

  Generates ``sbit`` / ``cbit`` instructions for bit manipulations.

.. option:: -mdata-model=model

  Choose a data model. The choices for :samp:`{model}` are :samp:`near`,
  :samp:`far` or :samp:`medium`. :samp:`medium` is default.
  However, :samp:`far` is not valid with :option:`-mcr16c`, as the
  CR16C architecture does not support the far data model.