..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_present_or_copyin:

acc_present_or_copyin -- If the data is not present on the device, allocate device memory and copy from host memory.
********************************************************************************************************************

Description
  This function tests if the host data specified by :samp:`{a}` and of length
  :samp:`{len}` is present or not. If it is not present, then device memory
  will be allocated and the host memory copied. The device address of
  the newly allocated device memory is returned.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

  Note that ``acc_present_or_copyin`` and ``acc_pcopyin`` exist for
  backward compatibility with OpenACC 2.0; use acc_copyin instead.

:samp:`{C/C++}:`

  ============  =======================================================
  *Prototype*:  ``void *acc_present_or_copyin(h_void *a, size_t len);``
  *Prototype*:  ``void *acc_pcopyin(h_void *a, size_t len);``
  ============  =======================================================

:samp:`{Fortran}:`

  ============  ============================================
  *Interface*:  ``subroutine acc_present_or_copyin(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_present_or_copyin(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_pcopyin(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_pcopyin(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  ============  ============================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.20.