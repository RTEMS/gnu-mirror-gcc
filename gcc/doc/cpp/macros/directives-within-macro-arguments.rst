..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _directives-within-macro-arguments:

Directives Within Macro Arguments
*********************************

.. index:: macro arguments and directives

Occasionally it is convenient to use preprocessor directives within
the arguments of a macro.  The C and C++ standards declare that
behavior in these cases is undefined.  GNU CPP
processes arbitrary directives within macro arguments in
exactly the same way as it would have processed the directive were the
function-like macro invocation not present.

If, within a macro invocation, that macro is redefined, then the new
definition takes effect in time for argument pre-expansion, but the
original definition is still used for argument replacement.  Here is a
pathological example:

.. code-block:: c++

  #define f(x) x x
  f (1
  #undef f
  #define f 2
  f)

which expands to

.. code-block:: c++

  1 2 1 2

with the semantics described above.