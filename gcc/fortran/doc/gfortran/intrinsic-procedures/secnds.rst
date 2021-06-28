..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _secnds:

SECNDS --- Time function
************************

.. index:: SECNDS

.. index:: time, elapsed

.. index:: elapsed time

.. function:: SECNDS(X)

  ``SECNDS(X)`` gets the time in seconds from the real-time system clock.
  :samp:`{X}` is a reference time, also in seconds. If this is zero, the time in
  seconds from midnight is returned. This function is non-standard and its
  use is discouraged.

  :param T:
    Shall be of type ``REAL(4)``.

  :param X:
    Shall be of type ``REAL(4)``.

  :return:
    None

  :samp:`{Standard}:`
    GNU extension

  :samp:`{Class}:`
    Function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = SECNDS (X)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_secnds
          integer :: i
          real(4) :: t1, t2
          print *, secnds (0.0)   ! seconds since midnight
          t1 = secnds (0.0)       ! reference time
          do i = 1, 10000000      ! do something
          end do
          t2 = secnds (t1)        ! elapsed time
          print *, "Something took ", t2, " seconds."
      end program test_secnds