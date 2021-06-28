..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_set_device_type:

acc_set_device_type -- Set type of device accelerator to use.
*************************************************************

Description
  This function indicates to the runtime library which device type, specified
  in :samp:`{devicetype}`, to use when executing a parallel or kernels region.

:samp:`{C/C++}:`

  ============  =================================================
  *Prototype*:  ``acc_set_device_type(acc_device_t devicetype);``
  ============  =================================================

:samp:`{Fortran}:`

  ============  ==============================================
  *Interface*:  ``subroutine acc_set_device_type(devicetype)``
                ``integer(kind=acc_device_kind) devicetype``
  ============  ==============================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.2.