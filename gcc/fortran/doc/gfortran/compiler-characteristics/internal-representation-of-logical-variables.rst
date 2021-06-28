..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _internal-representation-of-logical-variables:

Internal representation of LOGICAL variables
********************************************

.. index:: logical, variable representation

The Fortran standard does not specify how variables of ``LOGICAL``
type are represented, beyond requiring that ``LOGICAL`` variables
of default kind have the same storage size as default ``INTEGER``
and ``REAL`` variables.  The GNU Fortran internal representation is
as follows.

A ``LOGICAL(KIND=N)`` variable is represented as an
``INTEGER(KIND=N)`` variable, however, with only two permissible
values: ``1`` for ``.TRUE.`` and ``0`` for
``.FALSE.``.  Any other integer value results in undefined behavior.

See also Argument passing conventions and Interoperability with C.