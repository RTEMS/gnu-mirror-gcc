..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_attach:

acc_attach -- Let device pointer point to device-pointer target.
****************************************************************

Description
  This function updates a pointer on the device from pointing to a host-pointer
  address to pointing to the corresponding device data.

:samp:`{C/C++}:`

  ============  ==============================================
  *Prototype*:  ``acc_attach(h_void **ptr);``
  *Prototype*:  ``acc_attach_async(h_void **ptr, int async);``
  ============  ==============================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.34.