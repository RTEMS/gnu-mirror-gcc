..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _fortran-2018-status:

Status of Fortran 2018 support
******************************

* ERROR STOP in a PURE procedure
  An ``ERROR STOP`` statement is permitted in a ``PURE``
  procedure.

* IMPLICIT NONE with a spec-list
  Support the ``IMPLICIT NONE`` statement with an
  ``implicit-none-spec-list``.

* Behavior of INQUIRE with the RECL= specifier

  The behavior of the ``INQUIRE`` statement with the ``RECL=``
  specifier now conforms to Fortran 2018.

TS 29113 Status (Further Interoperability with C)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran supports some of the new features of the Technical
Specification (TS) 29113 on Further Interoperability of Fortran with C.
The `wiki <https://gcc.gnu.org/wiki/TS29113Status>`_ has some information
about the current TS 29113 implementation status.  In particular, the
following is implemented.

See also Further Interoperability of Fortran with C.

* The ``OPTIONAL`` attribute is allowed for dummy arguments
  of ``BIND(C) procedures.``

* The ``RANK`` intrinsic is supported.

* GNU Fortran's implementation for variables with ``ASYNCHRONOUS``
  attribute is compatible with TS 29113.

* Assumed types ( ``TYPE(*)`` ).

* Assumed-rank ( ``DIMENSION(..)`` ).

* ISO_Fortran_binding (now in Fortran 2018 18.4) is implemented such that
  conversion of the array descriptor for assumed type or assumed rank arrays is
  done in the library. The include file ISO_Fortran_binding.h is can be found in
  ``~prefix/lib/gcc/$target/$version``.

TS 18508 Status (Additional Parallel Features)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

GNU Fortran supports the following new features of the Technical
Specification 18508 on Additional Parallel Features in Fortran:

* The new atomic ADD, CAS, FETCH and ADD/OR/XOR, OR and XOR intrinsics.

* The ``CO_MIN`` and ``CO_MAX`` and ``SUM`` reduction intrinsics.
  And the ``CO_BROADCAST`` and ``CO_REDUCE`` intrinsic, except that those
  do not support polymorphic types or types with allocatable, pointer or
  polymorphic components.

* Events ( ``EVENT POST``, ``EVENT WAIT``, ``EVENT_QUERY`` )

* Failed images ( ``FAIL IMAGE``, ``IMAGE_STATUS``,
  ``FAILED_IMAGES``, ``STOPPED_IMAGES`` )

.. -
   Compiler Characteristics
   -