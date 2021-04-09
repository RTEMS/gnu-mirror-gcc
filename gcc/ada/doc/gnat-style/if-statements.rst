If Statements
*************

..  -

.. index:: if statement

* When the ``if``, ``elsif`` or ``else`` keywords fit on the
  same line with the condition and the ``then`` keyword, then the
  statement is formatted as follows:

  .. index:: Alignment (in an if statement)

  .. code-block:: ada

            if condition then
               ...
            elsif condition then
               ...
            else
               ...
            end if;

  When the above layout is not possible, ``then`` should be aligned
  with ``if``, and conditions should preferably be split before an
  ``and`` or ``or`` keyword a follows:

  .. code-block:: ada

            if long_condition_that_has_to_be_split
              and then continued_on_the_next_line
            then
               ...
            end if;

  The ``elsif``, ``else`` and ``end if`` always line up with
  the ``if`` keyword.  The preferred location for splitting the line
  is before ``and`` or ``or``.  The continuation of a condition is
  indented with two spaces or as many as needed to make nesting clear.
  As an exception, if conditions are closely related either of the
  following is allowed:

  .. code-block:: ada

         if x = lakdsjfhlkashfdlkflkdsalkhfsalkdhflkjdsahf
              or else
            x = asldkjhalkdsjfhhfd
              or else
            x = asdfadsfadsf
         then
           ...
         end if;

         if x = lakdsjfhlkashfdlkflkdsalkhfsalkdhflkjdsahf or else
            x = asldkjhalkdsjfhhfd                         or else
            x = asdfadsfadsf
         then
           ...
         end if;

* Conditions should use short-circuit forms ( ``and then``,
  ``or else`` ), except when the operands are boolean variables
  or boolean constants.

  .. index:: Short-circuit forms

* Complex conditions in ``if`` statements are indented two characters:

  .. index:: Indentation (in if statements)

  .. code-block:: ada

          if this_complex_condition
            and then that_other_one
            and then one_last_one
          then
             ...
          end if;

  There are some cases where complex conditionals can be laid out
  in manners that do not follow these rules to preserve better
  parallelism between branches, e.g.

  .. code-block:: ada

          if xyz.abc (gef) = 'c'
               or else
             xyz.abc (gef) = 'x'
          then
             ...
          end if;

* Every ``if`` block is preceded and followed by a blank line, except
  where it begins or ends a sequence_of_statements.

  .. index:: Blank lines (in an if statement)

  .. code-block:: ada

            A := 5;

            if A = 5 then
               null;
            end if;

            A := 6;

