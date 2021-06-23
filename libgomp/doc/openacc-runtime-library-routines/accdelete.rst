..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_delete:

acc_delete -- Free device memory.
*********************************

Description
  This function frees previously allocated device memory specified by
  the device address :samp:`{a}` and the length of :samp:`{len}` bytes.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

:samp:`{C/C++}:`

  ============  ================================================================
  *Prototype*:  ``acc_delete(h_void *a, size_t len);``
  *Prototype*:  ``acc_delete_async(h_void *a, size_t len, int async);``
  *Prototype*:  ``acc_delete_finalize(h_void *a, size_t len);``
  *Prototype*:  ``acc_delete_finalize_async(h_void *a, size_t len, int async);``
  ============  ================================================================

:samp:`{Fortran}:`

  ============  =======================================================
  *Interface*:  ``subroutine acc_delete(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_delete(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_delete_async(a, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_delete_async(a, len, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_delete_finalize(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_delete_finalize(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_delete_async_finalize(a, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer(acc_handle_kind) :: async``
  *Interface*:  ``subroutine acc_delete_async_finalize(a, len, async)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
                ``integer(acc_handle_kind) :: async``
  ============  =======================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.23.