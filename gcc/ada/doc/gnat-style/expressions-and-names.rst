.. _expressions-and-names:

Expressions and Names
---------------------

..  -

.. index:: Expressions and names

* Every operator must be surrounded by spaces. An exception is that
  this rule does not apply to the exponentiation operator, for which
  there are no specific layout rules. The reason for this exception
  is that sometimes it makes clearer reading to leave out the spaces
  around exponentiation.

  .. index:: Operators

  .. code-block:: ada

           E := A * B**2 + 3 * (C - D);

* Use parentheses where they clarify the intended association of operands
  with operators:

  .. index:: Parenthesization of expressions

  .. code-block:: ada

           (A / B) * C

..  -

