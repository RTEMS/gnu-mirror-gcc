..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fortran-2008-status:

Fortran 2008 status
*******************

The latest version of the Fortran standard is ISO/IEC 1539-1:2010, informally
known as Fortran 2008.  The official version is available from International
Organization for Standardization (ISO) or its national member organizations.
The the final draft (FDIS) can be downloaded free of charge from
http://www.nag.co.uk/sc22wg5/links.html.  Fortran is developed by the
Working Group 5 of Sub-Committee 22 of the Joint Technical Committee 1 of the
International Organization for Standardization and the International
Electrotechnical Commission (IEC).  This group is known as
`WG5 <http://www.nag.co.uk/sc22wg5/>`_.

The GNU Fortran compiler supports several of the new features of Fortran 2008;
the `wiki <https://gcc.gnu.org/wiki/Fortran2008Status>`_ has some information
about the current Fortran 2008 implementation status.  In particular, the
following is implemented.

* The :option:`-std`:samp:`=f2008` option and support for the file extensions
  :samp:`.f08` and :samp:`.F08`.

* The ``OPEN`` statement now supports the ``NEWUNIT=`` option,
  which returns a unique file unit, thus preventing inadvertent use of the
  same unit in different parts of the program.

* The ``g0`` format descriptor and unlimited format items.

* The mathematical intrinsics ``ASINH``, ``ACOSH``, ``ATANH``,
  ``ERF``, ``ERFC``, ``GAMMA``, ``LOG_GAMMA``, ``BESSEL_J0``,
  ``BESSEL_J1``, ``BESSEL_JN``, ``BESSEL_Y0``, ``BESSEL_Y1``,
  ``BESSEL_YN``, ``HYPOT``, ``NORM2``, and ``ERFC_SCALED``.

* Using complex arguments with ``TAN``, ``SINH``, ``COSH``,
  ``TANH``, ``ASIN``, ``ACOS``, and ``ATAN`` is now possible;
  ``ATAN`` ( :samp:`{Y}`, :samp:`{X}` ) is now an alias for ``ATAN2`` ( :samp:`{Y}`, :samp:`{X}` ).

* Support of the ``PARITY`` intrinsic functions.

* The following bit intrinsics: ``LEADZ`` and ``TRAILZ`` for
  counting the number of leading and trailing zero bits, ``POPCNT`` and
  ``POPPAR`` for counting the number of one bits and returning the parity;
  ``BGE``, ``BGT``, ``BLE``, and ``BLT`` for bitwise comparisons;
  ``DSHIFTL`` and ``DSHIFTR`` for combined left and right shifts,
  ``MASKL`` and ``MASKR`` for simple left and right justified masks,
  ``MERGE_BITS`` for a bitwise merge using a mask, ``SHIFTA``,
  ``SHIFTL`` and ``SHIFTR`` for shift operations, and the
  transformational bit intrinsics ``IALL``, ``IANY`` and ``IPARITY``.

* Support of the ``EXECUTE_COMMAND_LINE`` intrinsic subroutine.

* Support for the ``STORAGE_SIZE`` intrinsic inquiry function.

* The ``INT{8,16,32}`` and ``REAL{32,64,128}`` kind type
  parameters and the array-valued named constants ``INTEGER_KINDS``,
  ``LOGICAL_KINDS``, ``REAL_KINDS`` and ``CHARACTER_KINDS`` of
  the intrinsic module ``ISO_FORTRAN_ENV``.

* The module procedures ``C_SIZEOF`` of the intrinsic module
  ``ISO_C_BINDINGS`` and ``COMPILER_VERSION`` and ``COMPILER_OPTIONS``
  of ``ISO_FORTRAN_ENV``.

* Coarray support for serial programs with :option:`-fcoarray`:samp:`=single` flag
  and experimental support for multiple images with the :option:`-fcoarray`:samp:`=lib`
  flag.

* Submodules are supported. It should noted that ``MODULEs`` do not
  produce the smod file needed by the descendent ``SUBMODULEs`` unless they
  contain at least one ``MODULE PROCEDURE`` interface. The reason for this is
  that ``SUBMODULEs`` are useless without ``MODULE PROCEDUREs``. See
  http://j3-fortran.org/doc/meeting/207/15-209.txt for a discussion and a draft
  interpretation. Adopting this interpretation has the advantage that code that
  does not use submodules does not generate smod files.

* The ``DO CONCURRENT`` construct is supported.

* The ``BLOCK`` construct is supported.

* The ``STOP`` and the new ``ERROR STOP`` statements now
  support all constant expressions. Both show the signals which were signaling
  at termination.

* Support for the ``CONTIGUOUS`` attribute.

* Support for ``ALLOCATE`` with ``MOLD``.

* Support for the ``IMPURE`` attribute for procedures, which
  allows for ``ELEMENTAL`` procedures without the restrictions of
  ``PURE``.

* Null pointers (including ``NULL()`` ) and not-allocated variables
  can be used as actual argument to optional non-pointer, non-allocatable
  dummy arguments, denoting an absent argument.

* Non-pointer variables with ``TARGET`` attribute can be used as
  actual argument to ``POINTER`` dummies with ``INTENT(IN)``.

* Pointers including procedure pointers and those in a derived
  type (pointer components) can now be initialized by a target instead
  of only by ``NULL``.

* The ``EXIT`` statement (with construct-name) can be now be
  used to leave not only the ``DO`` but also the ``ASSOCIATE``,
  ``BLOCK``, ``IF``, ``SELECT CASE`` and ``SELECT TYPE``
  constructs.

* Internal procedures can now be used as actual argument.

* Minor features: obsolesce diagnostics for ``ENTRY`` with
  :option:`-std`:samp:`=f2008`; a line may start with a semicolon; for internal
  and module procedures ``END`` can be used instead of
  ``END SUBROUTINE`` and ``END FUNCTION`` ; ``SELECTED_REAL_KIND``
  now also takes a ``RADIX`` argument; intrinsic types are supported
  for ``TYPE`` ( :samp:`{intrinsic-type-spec}` ); multiple type-bound procedures
  can be declared in a single ``PROCEDURE`` statement; implied-shape
  arrays are supported for named constants ( ``PARAMETER`` ).