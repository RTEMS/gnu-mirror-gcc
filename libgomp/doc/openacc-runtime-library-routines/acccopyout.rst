..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_copyout:

acc_copyout -- Copy device memory to host memory.
*************************************************

Description
  This function copies mapped device memory to host memory which is specified
  by host address :samp:`{a}` for a length :samp:`{len}` bytes in C/C++.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

:samp:`{C/C++}:`

  ============  =================================================================
  *Prototype*:  ``acc_copyout(h_void *a, size_t len);``
  *Prototype*:  ``acc_copyout_async(h_void *a, size_t len, int async);``
  *Prototype*:  ``acc_copyout_finalize(h_void *a, size_t len);``
  *Prototype*:  ``acc_copyout_finalize_async(h_void *a, size_t len, int async);``
  ============  =================================================================

:samp:`{Fortran}:`

  ============  ========================================================
  *Interface*:  ``subroutine acc_copyout(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_copyout(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_copyout_async(a, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_copyout_async(a, len, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_copyout_finalize(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_copyout_finalize(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_copyout_finalize_async(a, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_copyout_finalize_async(a, len, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
                ``integer(acc_handle_kind) :: async``
  ============  ========================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.22.