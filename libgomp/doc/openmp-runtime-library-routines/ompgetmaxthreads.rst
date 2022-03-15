..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_get_max_threads:

omp_get_max_threads -- Maximum number of threads of parallel region
*******************************************************************

:samp:`{Description}:`
  Return the maximum number of threads used for the current parallel region
  that does not use the clause ``num_threads``.

:samp:`{C/C++}:`

  ============  ==================================
  *Prototype*:  ``int omp_get_max_threads(void);``
  ============  ==================================

:samp:`{Fortran}:`

  ============  ==========================================
  *Interface*:  ``integer function omp_get_max_threads()``
  ============  ==========================================

:samp:`{See also}:`
  :ref:`omp_set_num_threads`, :ref:`omp_set_dynamic`, :ref:`omp_get_thread_limit`

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.2.3.