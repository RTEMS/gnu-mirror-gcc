..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_set_device_num:

acc_set_device_num -- Set device number to use.
***********************************************

Description
  This function will indicate to the runtime which device number,
  specified by :samp:`{devicenum}`, associated with the specified device
  type :samp:`{devicetype}`.

:samp:`{C/C++}:`

  ============  ===============================================================
  *Prototype*:  ``acc_set_device_num(int devicenum, acc_device_t devicetype);``
  ============  ===============================================================

:samp:`{Fortran}:`

  ============  ========================================================
  *Interface*:  ``subroutine acc_set_device_num(devicenum, devicetype)``
                ``integer devicenum``
                ``integer(kind=acc_device_kind) devicetype``
  ============  ========================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.4.