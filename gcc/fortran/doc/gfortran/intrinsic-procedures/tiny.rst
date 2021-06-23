..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _tiny:

TINY --- Smallest positive number of a real kind
************************************************

.. index:: TINY

.. index:: limits, smallest number

.. index:: model representation, smallest number

.. function:: TINY(X)

  ``TINY(X)`` returns the smallest positive (non zero) number
  in the model of the type of ``X``.

  :param X:
    Shall be of type ``REAL``.

  :return:
    The return value is of the same type and kind as :samp:`{X}`

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = TINY(X)

  :samp:`{Example}:`
    See ``HUGE`` for an example.