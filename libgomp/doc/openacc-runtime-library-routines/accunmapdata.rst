..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_unmap_data:

acc_unmap_data -- Unmap device memory from host memory.
*******************************************************

Description
  This function unmaps previously mapped device and host memory. The latter
  specified by :samp:`{h}`.

:samp:`{C/C++}:`

  ============  ==============================
  *Prototype*:  ``acc_unmap_data(h_void *h);``
  ============  ==============================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.27.