..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _selected_real_kind:

SELECTED_REAL_KIND --- Choose real kind
***************************************

.. index:: SELECTED_REAL_KIND

.. index:: real kind

.. index:: kind, real

.. index:: radix, real

.. function:: SELECTED_REAL_KIND(P,R)

  ``SELECTED_REAL_KIND(P,R)`` returns the kind value of a real data type
  with decimal precision of at least ``P`` digits, exponent range of
  at least ``R``, and with a radix of ``RADIX``.

  :param P:
    (Optional) shall be a scalar and of type ``INTEGER``.

  :param R:
    (Optional) shall be a scalar and of type ``INTEGER``.

  :param RADIX:
    (Optional) shall be a scalar and of type ``INTEGER``.

  :return:
    ``SELECTED_REAL_KIND`` returns the value of the kind type parameter of
    a real data type with decimal precision of at least ``P`` digits, a
    decimal exponent range of at least ``R``, and with the requested
    ``RADIX``. If the ``RADIX`` parameter is absent, real kinds with
    any radix can be returned. If more than one real data type meet the
    criteria, the kind of the data type with the smallest decimal precision
    is returned. If no real data type matches the criteria, the result is

  :samp:`{Standard}:`
    Fortran 90 and later, with ``RADIX`` Fortran 2008 or later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SELECTED_REAL_KIND([P, R, RADIX])

  :samp:`{Example}:`

    .. code-block:: fortran

      program real_kinds
        integer,parameter :: p6 = selected_real_kind(6)
        integer,parameter :: p10r100 = selected_real_kind(10,100)
        integer,parameter :: r400 = selected_real_kind(r=400)
        real(kind=p6) :: x
        real(kind=p10r100) :: y
        real(kind=r400) :: z

        print *, precision(x), range(x)
        print *, precision(y), range(y)
        print *, precision(z), range(z)
      end program real_kinds

  :samp:`{See also}:`
    PRECISION,
    RANGE,
    RADIX