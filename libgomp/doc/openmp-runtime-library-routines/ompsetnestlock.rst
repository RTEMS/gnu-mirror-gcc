..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_set_nest_lock:

omp_set_nest_lock -- Wait for and set nested lock
*************************************************

Description:
  Before setting a nested lock, the lock variable must be initialized by
  ``omp_init_nest_lock``.  The calling thread is blocked until the lock
  is available.  If the lock is already held by the current thread, the
  nesting count for the lock is incremented.

C/C++:
  .. list-table::

     * - *Prototype*:
       - ``void omp_set_nest_lock(omp_nest_lock_t *lock);``

Fortran:
  .. list-table::

     * - *Interface*:
       - ``subroutine omp_set_nest_lock(nvar)``
     * -
       - ``integer(omp_nest_lock_kind), intent(inout) :: nvar``

See also:
  :ref:`omp_init_nest_lock`, :ref:`omp_unset_nest_lock`

Reference:
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 3.3.4.