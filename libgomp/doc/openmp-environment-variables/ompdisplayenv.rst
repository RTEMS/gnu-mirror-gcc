..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _omp_display_env:

OMP_DISPLAY_ENV -- Show OpenMP version and environment variables
****************************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  If set to ``TRUE``, the OpenMP version number and the values
  associated with the OpenMP environment variables are printed to ``stderr``.
  If set to ``VERBOSE``, it additionally shows the value of the environment
  variables which are GNU extensions.  If undefined or set to ``FALSE``,
  this information will not be shown.

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.12