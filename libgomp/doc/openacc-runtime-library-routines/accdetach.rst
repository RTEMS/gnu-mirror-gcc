..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_detach:

acc_detach -- Let device pointer point to host-pointer target.
**************************************************************

Description
  This function updates a pointer on the device from pointing to a device-pointer
  address to pointing to the corresponding host data.

:samp:`{C/C++}:`

  ============  =======================================================
  *Prototype*:  ``acc_detach(h_void **ptr);``
  *Prototype*:  ``acc_detach_async(h_void **ptr, int async);``
  *Prototype*:  ``acc_detach_finalize(h_void **ptr);``
  *Prototype*:  ``acc_detach_finalize_async(h_void **ptr, int async);``
  ============  =======================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.35.