..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_default_device:

OMP_DEFAULT_DEVICE -- Set the device used in target regions
***********************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  Set to choose the device which is used in a ``target`` region, unless the
  value is overridden by ``omp_set_default_device`` or by a ``device``
  clause.  The value shall be the nonnegative device number. If no device with
  the given device number exists, the code is executed on the host.  If unset,
  device number 0 will be used.

:samp:`{See also}:`
  omp_get_default_device, omp_set_default_device,

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.13