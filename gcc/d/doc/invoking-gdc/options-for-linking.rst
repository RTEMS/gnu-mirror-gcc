..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

    .. _linking:

Options for Linking
*******************

.. index:: options, linking

.. index:: linking, static

These options come into play when the compiler links object files into an
executable output file.  They are meaningless if the compiler is not doing
a link step.

:samp:`-defaultlib={libname}`

  .. index:: -defaultlib=

  Specify the library to use instead of libphobos when linking.  Options
  specifying the linkage of libphobos, such as :option:`-static-libphobos`
  or :option:`-shared-libphobos`, are ignored.

:samp:`-debuglib={libname}`

  .. index:: -debuglib=

  Specify the debug library to use instead of libphobos when linking.
  This option has no effect unless the :option:`-g` option was also given
  on the command line.  Options specifying the linkage of libphobos, such
  as :option:`-static-libphobos` or :option:`-shared-libphobos`, are ignored.

``-nophoboslib``

  .. index:: -nophoboslib

  Do not use the Phobos or D runtime library when linking.  Options specifying
  the linkage of libphobos, such as :option:`-static-libphobos` or
  :option:`-shared-libphobos`, are ignored.  The standard system libraries are
  used normally, unless :option:`-nostdlib` or :option:`-nodefaultlibs` is used.

``-shared-libphobos``

  .. index:: -shared-libphobos

  On systems that provide :samp:`libgphobos` and :samp:`libgdruntime` as a
  shared and a static library, this option forces the use of the shared
  version.  If no shared version was built when the compiler was configured,
  this option has no effect.

``-static-libphobos``

  .. index:: -static-libphobos

  On systems that provide :samp:`libgphobos` and :samp:`libgdruntime` as a
  shared and a static library, this option forces the use of the static
  version.  If no static version was built when the compiler was configured,
  this option has no effect.