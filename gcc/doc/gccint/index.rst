..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

Welcome to gccint documentation!
================================

Introduction
============

.. index:: introduction

This manual documents the internals of the GNU compilers, including
how to port them to new targets and some information about how to
write front ends for new languages.  It corresponds to the compilers
|package_version|
version |gcc_version|.  The use of the GNU compilers is documented in a
separate manual.  See :ref:`Introduction <top>`.

This manual is mainly a reference manual rather than a tutorial.  It
discusses how to contribute to GCC (see :ref:`contributing`), the
characteristics of the machines supported by GCC as hosts and targets
(see :ref:`portability`), how GCC relates to the ABIs on such systems
(see :ref:`interface`), and the characteristics of the languages for
which GCC front ends are written (see :ref:`languages`).  It then
describes the GCC source tree structure and build system, some of the
interfaces to GCC front ends, and how support for a target system is
implemented in GCC.

Additional tutorial information is linked to from
http://gcc.gnu.org/readings.html.

Contents:

.. toctree::
  :maxdepth: 1

  copyright
  contributing-to-gcc-development
  gcc-and-portability
  interfacing-to-gcc-output
  the-gcc-low-level-runtime-library
  language-front-ends-in-gcc
  source-tree-structure-and-build-system
  testsuites
  option-specification-files
  passes-and-files-of-the-compiler
  sizes-and-offsets-as-runtime-invariants
  generic
  gimple
  analysis-and-optimization-of-gimple-tuples
  rtl-representation
  control-flow-graph
  analysis-and-representation-of-loops
  machine-descriptions
  target-macros
  host-configuration
  makefile-fragments
  collect2
  standard-header-file-directories
  memory-management-and-type-information
  plugins
  link-time-optimization
  match-and-simplify
  static-analyzer
  user-experience-guidelines

  funding
  contributors-to-gcc
  general-public-license-3
  gnu-free-documentation-license

  option-index