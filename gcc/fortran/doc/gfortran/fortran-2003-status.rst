..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fortran-2003-status:

Fortran 2003 status
*******************

GNU Fortran supports several Fortran 2003 features; an incomplete
list can be found below.  See also the
`wiki page <https://gcc.gnu.org/wiki/Fortran2003>`_ about Fortran 2003.

* Procedure pointers including procedure-pointer components with
  ``PASS`` attribute.

* Procedures which are bound to a derived type (type-bound procedures)
  including ``PASS``, ``PROCEDURE`` and ``GENERIC``, and
  operators bound to a type.

* Abstract interfaces and type extension with the possibility to
  override type-bound procedures or to have deferred binding.

* Polymorphic entities (' ``CLASS`` ') for derived types and unlimited
  polymorphism (' ``CLASS(*)`` ') -- including ``SAME_TYPE_AS``,
  ``EXTENDS_TYPE_OF`` and ``SELECT TYPE`` for scalars and arrays and
  finalization.

* Generic interface names, which have the same name as derived types,
  are now supported. This allows one to write constructor functions.  Note
  that Fortran does not support static constructor functions.  For static
  variables, only default initialization or structure-constructor
  initialization are available.

* The ``ASSOCIATE`` construct.

* Interoperability with C including enumerations,

* In structure constructors the components with default values may be
  omitted.

* Extensions to the ``ALLOCATE`` statement, allowing for a
  type-specification with type parameter and for allocation and initialization
  from a ``SOURCE=`` expression; ``ALLOCATE`` and ``DEALLOCATE``
  optionally return an error message string via ``ERRMSG=``.

* Reallocation on assignment: If an intrinsic assignment is
  used, an allocatable variable on the left-hand side is automatically allocated
  (if unallocated) or reallocated (if the shape is different). Currently, scalar
  deferred character length left-hand sides are correctly handled but arrays
  are not yet fully implemented.

* Deferred-length character variables and scalar deferred-length character
  components of derived types are supported. (Note that array-valued components
  are not yet implemented.)

* Transferring of allocations via ``MOVE_ALLOC``.

* The ``PRIVATE`` and ``PUBLIC`` attributes may be given individually
  to derived-type components.

* In pointer assignments, the lower bound may be specified and
  the remapping of elements is supported.

* For pointers an ``INTENT`` may be specified which affect the
  association status not the value of the pointer target.

* Intrinsics ``command_argument_count``, ``get_command``,
  ``get_command_argument``, and ``get_environment_variable``.

* Support for Unicode characters (ISO 10646) and UTF-8, including
  the ``SELECTED_CHAR_KIND`` and ``NEW_LINE`` intrinsic functions.

* Support for binary, octal and hexadecimal (BOZ) constants in the
  intrinsic functions ``INT``, ``REAL``, ``CMPLX`` and ``DBLE``.

* Support for namelist variables with allocatable and pointer
  attribute and nonconstant length type parameter.

*
  .. index:: array, constructors

  .. index:: [...]

  Array constructors using square brackets.  That is, ``[...]`` rather
  than ``(/.../)``.  Type-specification for array constructors like
  ``(/ some-type :: ... /)``.

* Extensions to the specification and initialization expressions,
  including the support for intrinsics with real and complex arguments.

* Support for the asynchronous input/output.

*
  .. index:: FLUSH statement

  .. index:: statement, FLUSH

  ``FLUSH`` statement.

*
  .. index:: IOMSG= specifier

  ``IOMSG=`` specifier for I/O statements.

*
  .. index:: ENUM statement

  .. index:: ENUMERATOR statement

  .. index:: statement, ENUM

  .. index:: statement, ENUMERATOR

  .. index:: fshort-enums

  Support for the declaration of enumeration constants via the
  ``ENUM`` and ``ENUMERATOR`` statements.  Interoperability with
  :command:`gcc` is guaranteed also for the case where the
  :command:`-fshort-enums` command line option is given.

*
  .. index:: TR 15581

  TR 15581:

  *
    .. index:: ALLOCATABLE dummy arguments

    ``ALLOCATABLE`` dummy arguments.

  *
    .. index:: ALLOCATABLE function results

    ``ALLOCATABLE`` function results

  *
    .. index:: ALLOCATABLE components of derived types

    ``ALLOCATABLE`` components of derived types

*
  .. index:: STREAM I/O

  .. index:: ACCESS='STREAM' I/O

  The ``OPEN`` statement supports the ``ACCESS='STREAM'`` specifier,
  allowing I/O without any record structure.

* Namelist input/output for internal files.

* Minor I/O features: Rounding during formatted output, using of
  a decimal comma instead of a decimal point, setting whether a plus sign
  should appear for positive numbers. On systems where ``strtod`` honours
  the rounding mode, the rounding mode is also supported for input.

*
  .. index:: PROTECTED statement

  .. index:: statement, PROTECTED

  The ``PROTECTED`` statement and attribute.

*
  .. index:: VALUE statement

  .. index:: statement, VALUE

  The ``VALUE`` statement and attribute.

*
  .. index:: VOLATILE statement

  .. index:: statement, VOLATILE

  The ``VOLATILE`` statement and attribute.

*
  .. index:: IMPORT statement

  .. index:: statement, IMPORT

  The ``IMPORT`` statement, allowing to import
  host-associated derived types.

* The intrinsic modules ``ISO_FORTRAN_ENVIRONMENT`` is supported,
  which contains parameters of the I/O units, storage sizes. Additionally,
  procedures for C interoperability are available in the ``ISO_C_BINDING``
  module.

*
  .. index:: USE, INTRINSIC statement

  .. index:: statement, USE, INTRINSIC

  .. index:: ISO_FORTRAN_ENV statement

  .. index:: statement, ISO_FORTRAN_ENV

  ``USE`` statement with ``INTRINSIC`` and ``NON_INTRINSIC``
  attribute; supported intrinsic modules: ``ISO_FORTRAN_ENV``,
  ``ISO_C_BINDING``, ``OMP_LIB`` and ``OMP_LIB_KINDS``,
  and ``OPENACC``.

* Renaming of operators in the ``USE`` statement.