..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_get_device_num:

omp_get_device_num -- Return device number of current device
************************************************************

Description:
  This function returns a device number that represents the device that the
  current thread is executing on. For OpenMP 5.0, this must be equal to the
  value returned by the ``omp_get_initial_device`` function when called
  from the host.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``int omp_get_device_num(void);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``integer function omp_get_device_num()``

See also:
  :ref:`omp_get_initial_device`

Reference:
  `OpenMP specification v5.0 <https://www.openmp.org>`_, Section 3.2.37.