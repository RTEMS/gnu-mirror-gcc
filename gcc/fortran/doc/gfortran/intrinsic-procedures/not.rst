..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _not:

NOT --- Logical negation
************************

.. index:: NOT

.. index:: BNOT

.. index:: INOT

.. index:: JNOT

.. index:: KNOT

.. index:: bits, negate

.. index:: bitwise logical not

.. index:: logical not, bitwise

.. function:: NOT

  ``NOT`` returns the bitwise Boolean inverse of :samp:`{I}`.

  :param I:
    The type shall be ``INTEGER``.

  :return:
    The return type is ``INTEGER``, of the same kind as the
    argument.

  :samp:`{Standard}:`
    Fortran 90 and later, has overloads that are GNU extensions

  :samp:`{Class}:`
    Elemental function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = NOT(I)

  :samp:`{Specific names}:`
    ===========  ================  ==============  ====================
    Name         Argument          Return type     Standard
    ===========  ================  ==============  ====================
    ``NOT(A)``   ``INTEGER A``     ``INTEGER``     Fortran 95 and later
    ``BNOT(A)``  ``INTEGER(1) A``  ``INTEGER(1)``  GNU extension
    ``INOT(A)``  ``INTEGER(2) A``  ``INTEGER(2)``  GNU extension
    ``JNOT(A)``  ``INTEGER(4) A``  ``INTEGER(4)``  GNU extension
    ``KNOT(A)``  ``INTEGER(8) A``  ``INTEGER(8)``  GNU extension
    ===========  ================  ==============  ====================

  :samp:`{See also}:`
    IAND,
    IEOR,
    IOR,
    IBITS,
    IBSET,
    IBCLR