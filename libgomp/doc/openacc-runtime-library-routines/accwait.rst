..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_wait:

acc_wait -- Wait for completion of a specific asynchronous operation.
*********************************************************************

Description
  This function waits for completion of the asynchronous operation
  specified in :samp:`{arg}`.

:samp:`{C/C++}:`

  ========================================  ========================
  *Prototype*:                              ``acc_wait(arg);``
  *Prototype (OpenACC 1.0 compatibility)*:  ``acc_async_wait(arg);``
  ========================================  ========================

:samp:`{Fortran}:`

  ========================================  ==================================
  *Interface*:                              ``subroutine acc_wait(arg)``
                                            ``integer(acc_handle_kind) arg``
  *Interface (OpenACC 1.0 compatibility)*:  ``subroutine acc_async_wait(arg)``
                                            ``integer(acc_handle_kind) arg``
  ========================================  ==================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.11.