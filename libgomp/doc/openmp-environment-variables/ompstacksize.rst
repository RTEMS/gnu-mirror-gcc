..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_stacksize:

OMP_STACKSIZE -- Set default thread stack size
**********************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Set the default thread stack size in kilobytes, unless the number
  is suffixed by ``B``, ``K``, ``M`` or ``G``, in which
  case the size is, respectively, in bytes, kilobytes, megabytes
  or gigabytes.  This is different from ``pthread_attr_setstacksize``
  which gets the number of bytes as an argument.  If the stack size cannot
  be set due to system constraints, an error is reported and the initial
  stack size is left unchanged.  If undefined, the stack size is system
  dependent.

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.7