..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _statement-attributes:

Statement Attributes
********************

.. index:: Statement Attributes

GCC allows attributes to be set on null statements.  See :ref:`attribute-syntax`,
for details of the exact syntax for using attributes.  Other attributes are
available for functions (see :ref:`function-attributes`), variables
(see :ref:`variable-attributes`), labels (see :ref:`label-attributes`), enumerators
(see :ref:`enumerator-attributes`), and for types (see :ref:`type-attributes`).

This example uses the ``fallthrough`` statement attribute to indicate that
the :option:`-Wimplicit-fallthrough` warning should not be emitted:

.. code-block:: c++

  switch (cond)
    {
    case 1:
      bar (1);
      __attribute__((fallthrough));
    case 2:
      ...
    }

``fallthrough``

  .. index:: fallthrough statement attribute

  The ``fallthrough`` attribute with a null statement serves as a
  fallthrough statement.  It hints to the compiler that a statement
  that falls through to another case label, or user-defined label
  in a switch statement is intentional and thus the
  :option:`-Wimplicit-fallthrough` warning must not trigger.  The
  fallthrough attribute may appear at most once in each attribute
  list, and may not be mixed with other attributes.  It can only
  be used in a switch statement (the compiler will issue an error
  otherwise), after a preceding statement and before a logically
  succeeding case label, or user-defined label.