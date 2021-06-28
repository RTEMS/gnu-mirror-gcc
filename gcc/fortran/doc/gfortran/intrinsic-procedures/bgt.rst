..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _bgt:

BGT --- Bitwise greater than
****************************

.. index:: BGT

.. index:: bitwise comparison

.. function:: BGT

  Determines whether an integral is a bitwise greater than another.

  :param I:
    Shall be of ``INTEGER`` type.

  :param J:
    Shall be of ``INTEGER`` type, and of the same kind
    as :samp:`{I}`.

  :return:
    The return value is of type ``LOGICAL`` and of the default kind.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = BGT(I, J)

  :samp:`{See also}:`
    BGE,
    BLE,
    BLT