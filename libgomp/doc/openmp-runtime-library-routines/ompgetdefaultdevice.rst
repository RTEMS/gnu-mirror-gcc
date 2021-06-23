..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_default_device:

omp_get_default_device -- Get the default device for target regions
*******************************************************************

:samp:`{Description}:`
  Get the default device for target regions without device clause.

:samp:`{C/C++}:`

  ============  =====================================
  *Prototype*:  ``int omp_get_default_device(void);``
  ============  =====================================

:samp:`{Fortran}:`

  ============  =============================================
  *Interface*:  ``integer function omp_get_default_device()``
  ============  =============================================

:samp:`{See also}:`
  OMP_DEFAULT_DEVICE, omp_set_default_device

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.30.