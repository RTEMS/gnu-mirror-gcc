..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _lto:

Link Time Optimization
----------------------

.. index:: lto

.. index:: whopr

.. index:: wpa

.. index:: ltrans

Link Time Optimization (LTO) gives GCC the capability of
dumping its internal representation (GIMPLE) to disk,
so that all the different compilation units that make up
a single executable can be optimized as a single module.
This expands the scope of inter-procedural optimizations
to encompass the whole program (or, rather, everything
that is visible at link time).

.. toctree::
  :maxdepth: 2

  link-time-optimization/design-overview
  link-time-optimization/lto-file-sections
  link-time-optimization/using-summary-information-in-ipa-passes
  link-time-optimization/whole-program-assumptions-linker-plugin-and-symbol-visibilities
  link-time-optimization/internal-flags-controlling-lto1