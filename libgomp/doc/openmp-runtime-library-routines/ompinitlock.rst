..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_init_lock:

omp_init_lock -- Initialize simple lock
***************************************

Description:
  Initialize a simple lock.  After initialization, the lock is in
  an unlocked state.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_init_lock(omp_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_init_lock(svar)``
     * -
       - ``integer(omp_lock_kind), intent(out) :: svar``

See also:
  :ref:`omp_destroy_lock`

Reference:
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.1.