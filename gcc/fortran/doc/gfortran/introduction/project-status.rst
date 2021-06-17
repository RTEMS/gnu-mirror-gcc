..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _project-status:

Project Status
**************

As soon as :command:`gfortran` can parse all of the statements correctly,
it will be in the 'larva' state.
When we generate code, the 'puppa' state.
When :command:`gfortran` is done,
we'll see if it will be a beautiful butterfly,
or just a big bug....

--Andy Vaught, April 2000

The start of the GNU Fortran 95 project was announced on
the GCC homepage in March 18, 2000
(even though Andy had already been working on it for a while,
of course).

The GNU Fortran compiler is able to compile nearly all
standard-compliant Fortran 95, Fortran 90, and Fortran 77 programs,
including a number of standard and non-standard extensions, and can be
used on real-world programs.  In particular, the supported extensions
include OpenMP, Cray-style pointers, some old vendor extensions, and several
Fortran 2003 and Fortran 2008 features, including TR 15581.  However, it is
still under development and has a few remaining rough edges.
There also is initial support for OpenACC.

At present, the GNU Fortran compiler passes the
`NIST Fortran 77 Test Suite <http://www.fortran-2000.com/ArnaudRecipes/fcvs21_f95.html>`_, and produces acceptable results on the
`LAPACK Test Suite <http://www.netlib.org/lapack/faq.html#1.21>`_.
It also provides respectable performance on
the `Polyhedron Fortran
compiler benchmarks <http://www.polyhedron.com/fortran-compiler-comparisons/polyhedron-benchmark-suite>`_ and the
`Livermore Fortran Kernels test <http://www.netlib.org/benchmark/livermore>`_.  It has been used to compile a number of
large real-world programs, including
`the HARMONIE and HIRLAM weather forecasting code <http://hirlam.org/>`_ and
`the Tonto quantum chemistry package <http://physical-chemistry.scb.uwa.edu.au/tonto/wiki/index.php/Main_Page>`_; see
https://gcc.gnu.org/wiki/GfortranApps for an extended list.

Among other things, the GNU Fortran compiler is intended as a replacement
for G77.  At this point, nearly all programs that could be compiled with
G77 can be compiled with GNU Fortran, although there are a few minor known
regressions.

The primary work remaining to be done on GNU Fortran falls into three
categories: bug fixing (primarily regarding the treatment of invalid
code and providing useful error messages), improving the compiler
optimizations and the performance of compiled code, and extending the
compiler to support future standards---in particular, Fortran 2003,
Fortran 2008 and Fortran 2018.

.. -
   Standards
   -