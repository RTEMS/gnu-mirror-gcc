..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _directory-options:

Options for directory search
****************************

.. index:: directory, options

.. index:: options, directory search

.. index:: search path

.. index:: INCLUDE directive

.. index:: directive, INCLUDE

These options affect how GNU Fortran searches
for files specified by the ``INCLUDE`` directive and where it searches
for previously compiled modules.

It also affects the search paths used by :command:`cpp` when used to preprocess
Fortran source.

.. option:: -Idir

  .. index:: Idir

  .. index:: directory, search paths for inclusion

  .. index:: inclusion, directory search paths for

  .. index:: search paths, for included files

  .. index:: paths, search

  .. index:: module search path

  These affect interpretation of the ``INCLUDE`` directive
  (as well as of the ``#include`` directive of the :command:`cpp`
  preprocessor).

  Also note that the general behavior of :option:`-I` and
  ``INCLUDE`` is pretty much the same as of :option:`-I` with
  ``#include`` in the :command:`cpp` preprocessor, with regard to
  looking for :samp:`header.gcc` files and other such things.

  This path is also used to search for :samp:`.mod` files when previously
  compiled modules are required by a ``USE`` statement.

  See :ref:`Options for Directory Search <directory-options>`, for information on the
  :option:`-I` option.

.. option:: -Jdir

  .. index:: Jdir

  .. index:: Mdir

  .. index:: paths, search

  .. index:: module search path

  This option specifies where to put :samp:`.mod` files for compiled modules.
  It is also added to the list of directories to searched by an ``USE``
  statement.

  The default is the current directory.

.. option:: -fintrinsic-modules-path dir

  .. index:: fintrinsic-modules-pathdir

  .. index:: paths, search

  .. index:: module search path

  This option specifies the location of pre-compiled intrinsic modules, if
  they are not in the default location expected by the compiler.