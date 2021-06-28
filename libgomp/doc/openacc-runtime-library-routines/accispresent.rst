..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_is_present:

acc_is_present -- Indicate whether host variable / array is present on device.
******************************************************************************

Description
  This function indicates whether the specified host address in :samp:`{a}` and a
  length of :samp:`{len}` bytes is present on the device. In C/C++, a non-zero
  value is returned to indicate the presence of the mapped memory on the
  device. A zero is returned to indicate the memory is not mapped on the
  device.

  In Fortran, two (2) forms are supported. In the first form, :samp:`{a}` specifies
  a contiguous array section. The second form :samp:`{a}` specifies a variable or
  array element and :samp:`{len}` specifies the length in bytes. If the host
  memory is mapped to device memory, then a ``true`` is returned. Otherwise,
  a ``false`` is return to indicate the mapped memory is not present.

:samp:`{C/C++}:`

  ============  ==============================================
  *Prototype*:  ``int acc_is_present(h_void *a, size_t len);``
  ============  ==============================================

:samp:`{Fortran}:`

  ============  ===================================
  *Interface*:  ``function acc_is_present(a)``
                ``type, dimension(:[,:]...) :: a``
                ``logical acc_is_present``
  *Interface*:  ``function acc_is_present(a, len)``
                ``type, dimension(:[,:]...) :: a``
                ``integer len``
                ``logical acc_is_present``
  ============  ===================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.30.