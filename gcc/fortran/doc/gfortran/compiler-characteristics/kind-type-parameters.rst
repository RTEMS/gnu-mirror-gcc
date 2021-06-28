..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _kind-type-parameters:

KIND Type Parameters
********************

.. index:: kind

The ``KIND`` type parameters supported by GNU Fortran for the primitive
data types are:

.. envvar:: INTEGER

  1, 2, 4, 8 [#f1]_, 16 [#f1]_, default: 4 [#f2]_

.. envvar:: LOGICAL

  1, 2, 4, 8 [#f1]_, 16 [#f1]_, default: 4 [#f2]_

.. envvar:: REAL

  4, 8, 10 [#f1]_, 16 [#f1]_, default: 4 [#f3]_

.. envvar:: COMPLEX

  4, 8, 10 [#f1]_, 16 [#f1]_, default: 4 [#f3]_

``DOUBLE PRECISION``
  4, 8, 10 [#f1]_, 16 [#f1]_, default: 8 [#f3]_

.. envvar:: CHARACTER

  1, 4, default: 1

.. [#f1] not available on all systems
.. [#f2] unless :option:`-fdefault-integer-8` is used
.. [#f3] unless :option:`-fdefault-real-8` is used (see Fortran Dialect Options)

The ``KIND`` value matches the storage size in bytes, except for
``COMPLEX`` where the storage size is twice as much (or both real and
imaginary part are a real value of the given size).  It is recommended to use
the SELECTED_CHAR_KIND, SELECTED_INT_KIND and
SELECTED_REAL_KIND intrinsics or the ``INT8``, ``INT16``,
``INT32``, ``INT64``, ``REAL32``, ``REAL64``, and ``REAL128``
parameters of the ``ISO_FORTRAN_ENV`` module instead of the concrete values.
The available kind parameters can be found in the constant arrays
``CHARACTER_KINDS``, ``INTEGER_KINDS``, ``LOGICAL_KINDS`` and
``REAL_KINDS`` in the ISO_FORTRAN_ENV module.  For C interoperability,
the kind parameters of the ISO_C_BINDING module should be used.