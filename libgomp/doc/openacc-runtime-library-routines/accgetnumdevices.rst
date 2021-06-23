..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _acc_get_num_devices:

acc_get_num_devices -- Get number of devices for given device type
******************************************************************

Description
  This function returns a value indicating the number of devices available
  for the device type specified in :samp:`{devicetype}`.

:samp:`{C/C++}:`

  ============  =====================================================
  *Prototype*:  ``int acc_get_num_devices(acc_device_t devicetype);``
  ============  =====================================================

:samp:`{Fortran}:`

  ============  ====================================================
  *Interface*:  ``integer function acc_get_num_devices(devicetype)``
                ``integer(kind=acc_device_kind) devicetype``
  ============  ====================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.1.