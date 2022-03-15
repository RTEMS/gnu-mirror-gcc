..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: EXTENDS_TYPE_OF

.. _extends_type_of:

EXTENDS_TYPE_OF ---  Query dynamic type for extension
*****************************************************

.. function:: EXTENDS_TYPE_OF(A, MOLD)

  Query dynamic type for extension.

  :param A:
    Shall be an object of extensible declared type or
    unlimited polymorphic.

  :param MOLD:
    Shall be an object of extensible declared type or
    unlimited polymorphic.

  :return:
    The return value is a scalar of type default logical. It is true if and only if
    the dynamic type of A is an extension type of the dynamic type of MOLD.

  :samp:`{Standard}:`
    Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

    .. code-block:: fortran

      RESULT = EXTENDS_TYPE_OF(A, MOLD)

  :samp:`{See also}:`
    :ref:`SAME_TYPE_AS`