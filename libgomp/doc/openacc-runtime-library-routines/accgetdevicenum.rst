..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_get_device_num:

acc_get_device_num -- Get device number to be used.
***************************************************

Description
  This function returns which device number associated with the specified device
  type :samp:`{devicetype}`, will be used when executing a parallel or kernels
  region.

:samp:`{C/C++}:`

  ============  ====================================================
  *Prototype*:  ``int acc_get_device_num(acc_device_t devicetype);``
  ============  ====================================================

:samp:`{Fortran}:`

  ============  ============================================
  *Interface*:  ``function acc_get_device_num(devicetype)``
                ``integer(kind=acc_device_kind) devicetype``
                ``integer acc_get_device_num``
  ============  ============================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.5.