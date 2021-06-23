..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_get_cuda_stream:

acc_get_cuda_stream -- Get CUDA stream handle.
**********************************************

Description
  This function returns the CUDA stream handle for the queue :samp:`{async}`.
  This handle is the same as used by the CUDA Runtime or Driver API's.

:samp:`{C/C++}:`

  ============  =========================================
  *Prototype*:  ``void *acc_get_cuda_stream(int async);``
  ============  =========================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  A.2.1.3.