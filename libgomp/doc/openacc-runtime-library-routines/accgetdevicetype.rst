..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _acc_get_device_type:

acc_get_device_type -- Get type of device accelerator to be used.
*****************************************************************

Description
  This function returns what device type will be used when executing a
  parallel or kernels region.

  This function returns ``acc_device_none`` if
  ``acc_get_device_type`` is called from
  ``acc_ev_device_init_start``, ``acc_ev_device_init_end``
  callbacks of the OpenACC Profiling Interface (OpenACC Profiling
  Interface), that is, if the device is currently being initialized.

:samp:`{C/C++}:`

  ============  ===========================================
  *Prototype*:  ``acc_device_t acc_get_device_type(void);``
  ============  ===========================================

:samp:`{Fortran}:`

  ============  =====================================================
  *Interface*:  ``function acc_get_device_type(void)``
                ``integer(kind=acc_device_kind) acc_get_device_type``
  ============  =====================================================

:samp:`{Reference}:`
  `OpenACC specification v2.6 <https://www.openacc.org>`_, section
  3.2.3.