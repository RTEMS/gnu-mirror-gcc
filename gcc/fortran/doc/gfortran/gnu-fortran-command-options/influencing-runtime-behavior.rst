..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _runtime-options:

Influencing runtime behavior
****************************

.. index:: options, runtime

These options affect the runtime behavior of programs compiled with GNU Fortran.

.. option:: -fconvert=conversion

  .. index:: fconvert=conversion

  Specify the representation of data for unformatted files.  Valid
  values for conversion are: :samp:`native`, the default; :samp:`swap`,
  swap between big- and little-endian; :samp:`big-endian`, use big-endian
  representation for unformatted files; :samp:`little-endian`, use little-endian
  representation for unformatted files.

  This option has an effect only when used in the main program.
  The ``CONVERT`` specifier and the GFORTRAN_CONVERT_UNIT environment
  variable override the default specified by :option:`-fconvert`.

.. option:: -frecord-marker=length

  .. index:: frecord-marker=length

  Specify the length of record markers for unformatted files.
  Valid values for :samp:`{length}` are 4 and 8.  Default is 4.
  This is different from previous versions of :command:`gfortran`,
  which specified a default record marker length of 8 on most
  systems.  If you want to read or write files compatible
  with earlier versions of :command:`gfortran`, use :option:`-frecord-marker`:samp:`=8`.

.. option:: -fmax-subrecord-length=length

  .. index:: fmax-subrecord-length=length

  Specify the maximum length for a subrecord.  The maximum permitted
  value for length is 2147483639, which is also the default.  Only
  really useful for use by the gfortran testsuite.

.. option:: -fsign-zero

  .. index:: fsign-zero

  When enabled, floating point numbers of value zero with the sign bit set
  are written as negative number in formatted output and treated as
  negative in the ``SIGN`` intrinsic.  :option:`-fno-sign-zero` does not
  print the negative sign of zero values (or values rounded to zero for I/O)
  and regards zero as positive number in the ``SIGN`` intrinsic for
  compatibility with Fortran 77. The default is :option:`-fsign-zero`.