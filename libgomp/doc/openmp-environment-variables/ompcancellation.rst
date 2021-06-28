..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _omp_cancellation:

OMP_CANCELLATION -- Set whether cancellation is activated
*********************************************************

.. index:: Environment Variable

:samp:`{Description}:`
  If set to ``TRUE``, the cancellation is activated.  If set to ``FALSE`` or
  if unset, cancellation is disabled and the ``cancel`` construct is ignored.

:samp:`{See also}:`
  omp_get_cancellation

:samp:`{Reference}:`
  `OpenMP specification v4.5 <https://www.openmp.org>`_, Section 4.11