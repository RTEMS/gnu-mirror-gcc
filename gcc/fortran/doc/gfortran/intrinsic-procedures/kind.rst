..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _kind:

KIND --- Kind of an entity
**************************

.. index:: KIND

.. index:: kind

.. function:: KIND(X)

  ``KIND(X)`` returns the kind value of the entity :samp:`{X}`.

  :param X:
    Shall be of type ``LOGICAL``, ``INTEGER``,
    ``REAL``, ``COMPLEX`` or ``CHARACTER``.  It may be scalar or
    array valued.

  :return:
    The return value is a scalar of type ``INTEGER`` and of the default
    integer kind.

  :samp:`{Standard}:`
    Fortran 95 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    K = KIND(X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_kind
        integer,parameter :: kc = kind(' ')
        integer,parameter :: kl = kind(.true.)

        print *, "The default character kind is ", kc
        print *, "The default logical kind is ", kl
      end program test_kind