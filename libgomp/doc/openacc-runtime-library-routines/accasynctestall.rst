..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_async_test_all:

acc_async_test_all -- Tests for completion of all asynchronous operations.
**************************************************************************

Description
  This function tests for completion of all asynchronous operations.
  In C/C++, a non-zero value will be returned to indicate all asynchronous
  operations have completed. While Fortran will return a ``true``. If
  any asynchronous operation has not completed, C/C++ returns a zero and
  Fortran returns a ``false``.

:samp:`{C/C++}:`

  ============  =================================
  *Prototype*:  ``int acc_async_test_all(void);``
  ============  =================================

:samp:`{Fortran}:`

  ============  ==============================
  *Interface*:  ``function acc_async_test()``
                ``logical acc_get_device_num``
  ============  ==============================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.10.