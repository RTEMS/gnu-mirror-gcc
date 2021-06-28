..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_hostptr:

acc_hostptr -- Get host pointer associated with specific device address.
************************************************************************

Description
  This function returns the host address that has been mapped to the
  device address specified by :samp:`{d}`.

:samp:`{C/C++}:`

  ============  =================================
  *Prototype*:  ``void *acc_hostptr(d_void *d);``
  ============  =================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.29.