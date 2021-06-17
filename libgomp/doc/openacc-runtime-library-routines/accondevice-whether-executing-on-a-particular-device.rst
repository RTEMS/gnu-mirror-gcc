..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_on_device:

acc_on_device -- Whether executing on a particular device
*********************************************************

:samp:`{Description}:`
  This function returns whether the program is executing on a particular
  device specified in :samp:`{devicetype}`. In C/C++ a non-zero value is
  returned to indicate the device is executing on the specified device type.
  In Fortran, ``true`` will be returned. If the program is not executing
  on the specified device type C/C++ will return a zero, while Fortran will
  return ``false``.

:samp:`{C/C++}:`

  ============  ===========================================
  *Prototype*:  ``acc_on_device(acc_device_t devicetype);``
  ============  ===========================================

:samp:`{Fortran}:`

  ============  =======================================
  *Interface*:  ``function acc_on_device(devicetype)``
                ``integer(acc_device_kind) devicetype``
                ``logical acc_on_device``
  ============  =======================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.17.