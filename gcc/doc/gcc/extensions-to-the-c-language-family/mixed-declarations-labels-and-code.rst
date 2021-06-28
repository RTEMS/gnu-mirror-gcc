..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _mixed-labels-and-declarations:

Mixed Declarations, Labels and Code
***********************************

.. index:: mixed declarations and code

.. index:: declarations, mixed with code

.. index:: code, mixed with declarations

ISO C99 and ISO C++ allow declarations and code to be freely mixed
within compound statements.  ISO C2X allows labels to be
placed before declarations and at the end of a compound statement.
As an extension, GNU C also allows all this in C90 mode.  For example,
you could do:

.. code-block:: c++

  int i;
  /* ... */
  i++;
  int j = i + 2;

Each identifier is visible from where it is declared until the end of
the enclosing block.