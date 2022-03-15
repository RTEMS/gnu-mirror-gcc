..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_set_max_active_levels:

omp_set_max_active_levels -- Limits the number of active parallel regions
*************************************************************************

:samp:`{Description}:`
  This function limits the maximum allowed number of nested, active
  parallel regions.  :samp:`{max_levels}` must be less or equal to
  the value returned by ``omp_get_supported_active_levels``.

:samp:`{C/C++}:`

  ============  ===================================================
  *Prototype*:  ``void omp_set_max_active_levels(int max_levels);``
  ============  ===================================================

:samp:`{Fortran}:`

  ============  ====================================================
  *Interface*:  ``subroutine omp_set_max_active_levels(max_levels)``
                ``integer max_levels``
  ============  ====================================================

:samp:`{See also}:`
  :ref:`omp_get_max_active_levels`, :ref:`omp_get_active_level`,
  :ref:`omp_get_supported_active_levels`

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.15.