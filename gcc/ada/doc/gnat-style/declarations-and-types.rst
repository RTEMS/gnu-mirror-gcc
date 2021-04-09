.. _declarations-and-types:

Declarations and Types
----------------------

..  -

.. index:: Declarations and Types

* In entity declarations, colons must be surrounded by spaces.  Colons
  should be aligned.

  .. index:: Alignment (in declarations)

  .. code-block:: ada

            Entity1   : Integer;
            My_Entity : Integer;

* Declarations should be grouped in a logical order.
  Related groups of declarations may be preceded by a header comment.

* All local subprograms in a subprogram or package body should be declared
  before the first local subprogram body.

* Do not declare local entities that hide global entities.

  .. index:: Hiding of outer entities

* Do not declare multiple variables in one declaration that spans lines.
  Start a new declaration on each line, instead.

* The defining_identifiers of global declarations serve as
  comments of a sort.  So don't choose terse names, but look for names
  that give useful information instead.

* Local names can be shorter, because they are used only within
  one context, where comments explain their purpose.

* When starting an initialization or default expression on the line that follows
  the declaration line, use 2 characters for indentation.

  .. code-block:: ada

            Entity1 : Integer :=
              Function_Name (Parameters, For_Call);

* If an initialization or default expression needs to be continued on subsequent
  lines, the continuations should be indented from the start of the expression.

  .. code-block:: ada

            Entity1 : Integer := Long_Function_Name
                                   (parameters for call);

..  -

