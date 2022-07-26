..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. index:: Environment Variable

.. _omp_stacksize:

OMP_STACKSIZE -- Set default thread stack size
**********************************************

Description:
  Set the default thread stack size in kilobytes, unless the number
  is suffixed by ``B``, ``K``, ``M`` or ``G``, in which
  case the size is, respectively, in bytes, kilobytes, megabytes
  or gigabytes.  This is different from ``pthread_attr_setstacksize``
  which gets the number of bytes as an argument.  If the stack size cannot
  be set due to system constraints, an error is reported and the initial
  stack size is left unchanged.  If undefined, the stack size is system
  dependent.

Reference:
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.7