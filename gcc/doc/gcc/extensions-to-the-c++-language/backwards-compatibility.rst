..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _backwards-compatibility:

Backwards Compatibility
***********************

.. index:: Backwards Compatibility

.. index:: ARM [Annotated C++ Reference Manual]

Now that there is a definitive ISO standard C++, G++ has a specification
to adhere to.  The C++ language evolved over time, and features that
used to be acceptable in previous drafts of the standard, such as the ARM
[Annotated C++ Reference Manual], are no longer accepted.  In order to allow
compilation of C++ written to such drafts, G++ contains some backwards
compatibilities.  *All such backwards compatibility features are
liable to disappear in future versions of G++.* They should be considered
deprecated.   See :ref:`deprecated-features`.

``Implicit C language``
  Old C system header files did not contain an ``extern "C" {...}``
  scope to set the language.  On such systems, all system header files are
  implicitly scoped inside a C language scope.  Such headers must
  correctly prototype function argument types, there is no leeway for
  ``()`` to indicate an unspecified set of arguments.

..  LocalWords:  emph deftypefn builtin ARCv2EM SIMD builtins msimd
    LocalWords:  typedef v4si v8hi DMA dma vdiwr vdowr