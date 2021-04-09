Loop Statements
***************

.. index:: Loop statements

* When possible, have ``for`` or ``while`` on one line with the
  condition and the ``loop`` keyword.

  .. code-block:: ada

           for J in S'Range loop
              ...
           end loop;

  If the condition is too long, split the condition (see 'If
  statements' above) and align ``loop`` with the ``for`` or
  ``while`` keyword.

  .. index:: Alignment (in a loop statement)

  .. code-block:: ada

          while long_condition_that_has_to_be_split
            and then continued_on_the_next_line
          loop
             ...
          end loop;

  If the loop_statement has an identifier, it is laid out as follows:

  .. code-block:: ada

          Outer : while not condition loop
             ...
          end Outer;

