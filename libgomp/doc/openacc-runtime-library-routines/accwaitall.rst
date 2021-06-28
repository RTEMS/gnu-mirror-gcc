..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_wait_all:

acc_wait_all -- Waits for completion of all asynchronous operations.
********************************************************************

Description
  This function waits for the completion of all asynchronous operations.

:samp:`{C/C++}:`

  ========================================  =============================
  *Prototype*:                              ``acc_wait_all(void);``
  *Prototype (OpenACC 1.0 compatibility)*:  ``acc_async_wait_all(void);``
  ========================================  =============================

:samp:`{Fortran}:`

  ========================================  ===================================
  *Interface*:                              ``subroutine acc_wait_all()``
  *Interface (OpenACC 1.0 compatibility)*:  ``subroutine acc_async_wait_all()``
  ========================================  ===================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.13.