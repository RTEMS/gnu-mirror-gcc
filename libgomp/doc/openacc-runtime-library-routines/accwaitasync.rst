..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_wait_async:

acc_wait_async -- Wait for completion of asynchronous operations.
*****************************************************************

Description
  This function enqueues a wait operation on queue :samp:`{async}` for any and all
  asynchronous operations enqueued on queue :samp:`{arg}`.

:samp:`{C/C++}:`

  ============  =======================================
  *Prototype*:  ``acc_wait_async(int arg, int async);``
  ============  =======================================

:samp:`{Fortran}:`

  ============  =========================================
  *Interface*:  ``subroutine acc_wait_async(arg, async)``
                ``integer(acc_handle_kind) arg, async``
  ============  =========================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.12.