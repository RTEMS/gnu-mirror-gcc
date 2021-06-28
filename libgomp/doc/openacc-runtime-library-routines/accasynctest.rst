..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_async_test:

acc_async_test -- Test for completion of a specific asynchronous operation.
***************************************************************************

Description
  This function tests for completion of the asynchronous operation specified
  in :samp:`{arg}`. In C/C++, a non-zero value will be returned to indicate
  the specified asynchronous operation has completed. While Fortran will return
  a ``true``. If the asynchronous operation has not completed, C/C++ returns
  a zero and Fortran returns a ``false``.

:samp:`{C/C++}:`

  ============  ================================
  *Prototype*:  ``int acc_async_test(int arg);``
  ============  ================================

:samp:`{Fortran}:`

  ============  =====================================
  *Interface*:  ``function acc_async_test(arg)``
                ``integer(kind=acc_handle_kind) arg``
                ``logical acc_async_test``
  ============  =====================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.9.