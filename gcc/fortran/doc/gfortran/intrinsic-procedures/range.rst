..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: RANGE, model representation, range

.. _range:

RANGE --- Decimal exponent range
********************************

.. function:: RANGE(X)

  ``RANGE(X)`` returns the decimal exponent range in the model of the
  type of ``X``.

  :param X:
    Shall be of type ``INTEGER``, ``REAL``
    or ``COMPLEX``.

  :return:
    The return value is of type ``INTEGER`` and of the default integer
    kind.

  :samp:`{Standard}:`
    Fortran 90 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = RANGE(X)

  :samp:`{Example}:`
    See ``PRECISION`` for an example.

  :samp:`{See also}:`
    :ref:`SELECTED_REAL_KIND`,
    :ref:`PRECISION`