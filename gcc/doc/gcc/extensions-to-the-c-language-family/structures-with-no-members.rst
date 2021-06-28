..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _empty-structures:

Structures with No Members
**************************

.. index:: empty structures

.. index:: zero-size structures

GCC permits a C structure to have no members:

.. code-block:: c++

  struct empty {
  };

The structure has size zero.  In C++, empty structures are part
of the language.  G++ treats empty structures as if they had a single
member of type ``char``.