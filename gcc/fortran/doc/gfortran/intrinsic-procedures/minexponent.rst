..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: MINEXPONENT, model representation, minimum exponent

.. _minexponent:

MINEXPONENT --- Minimum exponent of a real kind
***********************************************

.. function:: MINEXPONENT(X)

  ``MINEXPONENT(X)`` returns the minimum exponent in the model of the
  type of ``X``.

  :param X:
    Shall be of type ``REAL``.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = MINEXPONENT(X)

  :samp:`{Example}:`
    See ``MAXEXPONENT`` for an example.