..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: MERGE, array, merge arrays, array, combine arrays

.. _merge:

MERGE --- Merge variables
*************************

.. function:: MERGE(TSOURCE, FSOURCE, MASK)

  Select values from two arrays according to a logical mask.  The result
  is equal to :samp:`{TSOURCE}` if :samp:`{MASK}` is ``.TRUE.``, or equal to
  :samp:`{FSOURCE}` if it is ``.FALSE.``.

  :param TSOURCE:
    May be of any type.

  :param FSOURCE:
    Shall be of the same type and type parameters
    as :samp:`{TSOURCE}`.

  :param MASK:
    Shall be of type ``LOGICAL``.

  :return:
    The result is of the same type and type parameters as :samp:`{TSOURCE}`.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = MERGE(TSOURCE, FSOURCE, MASK)