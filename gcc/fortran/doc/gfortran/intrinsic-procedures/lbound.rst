..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _lbound:

LBOUND --- Lower dimension bounds of an array
*********************************************

.. index:: LBOUND

.. index:: array, lower bound

.. function:: LBOUND

  Returns the lower bounds of an array, or a single lower bound
  along the :samp:`{DIM}` dimension.

  :param ARRAY:
    Shall be an array, of any type.

  :param DIM:
    (Optional) Shall be a scalar ``INTEGER``.

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.
    If :samp:`{DIM}` is absent, the result is an array of the lower bounds of
    :samp:`{ARRAY}`.  If :samp:`{DIM}` is present, the result is a scalar
    corresponding to the lower bound of the array along that dimension.  If
    :samp:`{ARRAY}` is an expression rather than a whole array or array
    structure component, or if it has a zero extent along the relevant
    dimension, the lower bound is taken to be 1.

  :samp:`{Standard}:`
    Fortran 90 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = LBOUND(ARRAY [, DIM [, KIND]])

  :samp:`{See also}:`
    UBOUND,
    LCOBOUND