Case Statements
***************

.. index:: case statements

* Layout is as below.  For long ``case`` statements, the extra indentation
  can be saved by aligning the ``when`` clauses with the opening ``case``.

  .. code-block:: ada

           case expression is
              when condition =>
                 ...
              when condition =>
                 ...
           end case;

