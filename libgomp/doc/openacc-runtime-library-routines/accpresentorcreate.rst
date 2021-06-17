..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_present_or_create:

acc_present_or_create -- If the data is not present on the device, allocate device memory and map it to host memory.
********************************************************************************************************************

Description
  This function tests if the host data specified by :samp:`{a}` and of length
  :samp:`{len}` is present or not. If it is not present, then device memory
  will be allocated and mapped to host memory. In C/C++, the device address
  of the newly allocated device memory is returned.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes.

  Note that ``acc_present_or_create`` and ``acc_pcreate`` exist for
  backward compatibility with OpenACC 2.0; use acc_create instead.

:samp:`{C/C++}:`

  ============  ======================================================
  *Prototype*:  ``void *acc_present_or_create(h_void *a, size_t len)``
  *Prototype*:  ``void *acc_pcreate(h_void *a, size_t len)``
  ============  ======================================================

:samp:`{Fortran}:`

  ============  ============================================
  *Interface*:  ``subroutine acc_present_or_create(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_present_or_create(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  *Interface*:  ``subroutine acc_pcreate(a)``
                ``type, dimension(:[,:]...) :: a``
  *Interface*:  ``subroutine acc_pcreate(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
  ============  ============================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.21.