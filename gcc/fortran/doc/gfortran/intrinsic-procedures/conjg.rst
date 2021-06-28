..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _conjg:

CONJG --- Complex conjugate function
************************************

.. index:: CONJG

.. index:: DCONJG

.. index:: complex conjugate

.. function:: CONJG(Z)

  ``CONJG(Z)`` returns the conjugate of :samp:`{Z}`.  If :samp:`{Z}` is ``(x, y)``
  then the result is ``(x, -y)``

  :param Z:
    The type shall be ``COMPLEX``.

  :return:
    The return value is of type ``COMPLEX``.

  :samp:`{Standard}:`
    Fortran 77 and later, has an overload that is a GNU extension

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    Z = CONJG(Z)

  :samp:`{Example}:`

    .. code-block:: fortran

      program test_conjg
          complex :: z = (2.0, 3.0)
          complex(8) :: dz = (2.71_8, -3.14_8)
          z= conjg(z)
          print *, z
          dz = dconjg(dz)
          print *, dz
      end program test_conjg

  :samp:`{Specific names}:`
    =============  ================  ==============  =============
    Name           Argument          Return type     Standard
    =============  ================  ==============  =============
    ``DCONJG(Z)``  ``COMPLEX(8) Z``  ``COMPLEX(8)``  GNU extension
    =============  ================  ==============  =============