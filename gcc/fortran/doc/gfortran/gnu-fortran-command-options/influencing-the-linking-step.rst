..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _link-options:

Influencing the linking step
****************************

.. index:: options, linking

.. index:: linking, static

These options come into play when the compiler links object files into an
executable output file. They are meaningless if the compiler is not doing
a link step.

.. option:: -static-libgfortran

  .. index:: static-libgfortran

  On systems that provide :samp:`libgfortran` as a shared and a static
  library, this option forces the use of the static version. If no
  shared version of :samp:`libgfortran` was built when the compiler was
  configured, this option has no effect.