..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _iso_c_binding:

ISO_C_BINDING
*************

:samp:`{Standard}:`
  Fortran 2003 and later, GNU extensions

  The following intrinsic procedures are provided by the module; their
  definition can be found in the section Intrinsic Procedures of this
  manual.

C_ASSOCIATEDC_F_POINTERC_F_PROCPOINTERC_FUNLOCC_LOCC_SIZEOF

.. TODO: Vertical spacing between C_FUNLOC and C_LOC wrong in PDF,
   don't really know why.

The ``ISO_C_BINDING`` module provides the following named constants of
type default integer, which can be used as KIND type parameters.

In addition to the integer named constants required by the Fortran 2003
standard and ``C_PTRDIFF_T`` of TS 29113, GNU Fortran provides as an
extension named constants for the 128-bit integer types supported by the
C compiler: ``C_INT128_T, C_INT_LEAST128_T, C_INT_FAST128_T``.
Furthermore, if ``__float128`` is supported in C, the named constants
``C_FLOAT128, C_FLOAT128_COMPLEX`` are defined.

=============  =========================  ===================================  =========
Fortran Type   Named constant             C type                               Extension
=============  =========================  ===================================  =========
``INTEGER``    ``C_INT``                  ``int``
``INTEGER``    ``C_SHORT``                ``short int``
``INTEGER``    ``C_LONG``                 ``long int``
``INTEGER``    ``C_LONG_LONG``            ``long long int``
``INTEGER``    ``C_SIGNED_CHAR``          ``signed char`` / ``unsigned char``
``INTEGER``    ``C_SIZE_T``               ``size_t``
``INTEGER``    ``C_INT8_T``               ``int8_t``
``INTEGER``    ``C_INT16_T``              ``int16_t``
``INTEGER``    ``C_INT32_T``              ``int32_t``
``INTEGER``    ``C_INT64_T``              ``int64_t``
``INTEGER``    ``C_INT128_T``             ``int128_t``                         Ext.
``INTEGER``    ``C_INT_LEAST8_T``         ``int_least8_t``
``INTEGER``    ``C_INT_LEAST16_T``        ``int_least16_t``
``INTEGER``    ``C_INT_LEAST32_T``        ``int_least32_t``
``INTEGER``    ``C_INT_LEAST64_T``        ``int_least64_t``
``INTEGER``    ``C_INT_LEAST128_T``       ``int_least128_t``                   Ext.
``INTEGER``    ``C_INT_FAST8_T``          ``int_fast8_t``
``INTEGER``    ``C_INT_FAST16_T``         ``int_fast16_t``
``INTEGER``    ``C_INT_FAST32_T``         ``int_fast32_t``
``INTEGER``    ``C_INT_FAST64_T``         ``int_fast64_t``
``INTEGER``    ``C_INT_FAST128_T``        ``int_fast128_t``                    Ext.
``INTEGER``    ``C_INTMAX_T``             ``intmax_t``
``INTEGER``    ``C_INTPTR_T``             ``intptr_t``
``INTEGER``    ``C_PTRDIFF_T``            ``ptrdiff_t``                        TS 29113
``REAL``       ``C_FLOAT``                ``float``
``REAL``       ``C_DOUBLE``               ``double``
``REAL``       ``C_LONG_DOUBLE``          ``long double``
``REAL``       ``C_FLOAT128``             ``__float128``                       Ext.
``COMPLEX``    ``C_FLOAT_COMPLEX``        ``float _Complex``
``COMPLEX``    ``C_DOUBLE_COMPLEX``       ``double _Complex``
``COMPLEX``    ``C_LONG_DOUBLE_COMPLEX``  ``long double _Complex``
``REAL``       ``C_FLOAT128_COMPLEX``     ``__float128 _Complex``              Ext.
``LOGICAL``    ``C_BOOL``                 ``_Bool``
``CHARACTER``  ``C_CHAR``                 ``char``
=============  =========================  ===================================  =========

Additionally, the following parameters of type ``CHARACTER(KIND=C_CHAR)``
are defined.

=====================  ===============  ========
Name                   C definition     Value
=====================  ===============  ========
``C_NULL_CHAR``        null character   ``'\0'``
``C_ALERT``            alert            ``'\a'``
``C_BACKSPACE``        backspace        ``'\b'``
``C_FORM_FEED``        form feed        ``'\f'``
``C_NEW_LINE``         new line         ``'\n'``
``C_CARRIAGE_RETURN``  carriage return  ``'\r'``
``C_HORIZONTAL_TAB``   horizontal tab   ``'\t'``
``C_VERTICAL_TAB``     vertical tab     ``'\v'``
=====================  ===============  ========

Moreover, the following two named constants are defined:

=================  ============
Name               Type
=================  ============
``C_NULL_PTR``     ``C_PTR``
``C_NULL_FUNPTR``  ``C_FUNPTR``
=================  ============

Both are equivalent to the value ``NULL`` in C.