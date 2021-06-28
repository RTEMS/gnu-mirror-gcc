..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_get_initial_device:

omp_get_initial_device -- Return device number of initial device
****************************************************************

:samp:`{Description}:`
  This function returns a device number that represents the host device.
  For OpenMP 5.1, this must be equal to the value returned by the
  ``omp_get_num_devices`` function.

C/C++

  ============  =====================================
  *Prototype*:  ``int omp_get_initial_device(void);``
  ============  =====================================

:samp:`{Fortran}:`

  ============  =============================================
  *Interface*:  ``integer function omp_get_initial_device()``
  ============  =============================================

:samp:`{See also}:`
  omp_get_num_devices

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.35.