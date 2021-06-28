..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_places:

OMP_PLACES -- Specifies on which CPUs the theads should be placed
*****************************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  The thread placement can be either specified using an abstract name or by an
  explicit list of the places.  The abstract names ``threads``, ``cores``
  and ``sockets`` can be optionally followed by a positive number in
  parentheses, which denotes the how many places shall be created.  With
  ``threads`` each place corresponds to a single hardware thread; ``cores``
  to a single core with the corresponding number of hardware threads; and with
  ``sockets`` the place corresponds to a single socket.  The resulting
  placement can be shown by setting the :envvar:`OMP_DISPLAY_ENV` environment
  variable.

  Alternatively, the placement can be specified explicitly as comma-separated
  list of places.  A place is specified by set of nonnegative numbers in curly
  braces, denoting the denoting the hardware threads.  The hardware threads
  belonging to a place can either be specified as comma-separated list of
  nonnegative thread numbers or using an interval.  Multiple places can also be
  either specified by a comma-separated list of places or by an interval.  To
  specify an interval, a colon followed by the count is placed after after
  the hardware thread number or the place.  Optionally, the length can be
  followed by a colon and the stride number -- otherwise a unit stride is
  assumed.  For instance, the following specifies the same places list:
  ``"{0,1,2}, {3,4,6}, {7,8,9}, {10,11,12}"`` ;
  ``"{0:3}, {3:3}, {7:3}, {10:3}"`` ; and ``"{0:2}:4:3"``.

  If :envvar:`OMP_PLACES` and :envvar:`GOMP_CPU_AFFINITY` are unset and
  :envvar:`OMP_PROC_BIND` is either unset or ``false``, threads may be moved
  between CPUs following no placement policy.

:samp:`{See also}:`
  OMP_PROC_BIND, GOMP_CPU_AFFINITY, omp_get_proc_bind,
  OMP_DISPLAY_ENV

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.5