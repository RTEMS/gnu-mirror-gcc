Comments
********

..  -

.. index:: Comments

* A comment starts with ``--`` followed by two spaces.
  The only exception to this rule (i.e. one space is tolerated) is when the
  comment ends with a single space followed by ``--``.
  It is also acceptable to have only one space between ``--`` and the start
  of the comment when the comment is at the end of a line,
  after some Ada code.

* Every sentence in a comment should start with an upper-case letter (including
  the first letter of the comment).

  .. index:: Casing (in comments)

* When declarations are commented with 'hanging' comments, i.e.
  comments after the declaration, there is no blank line before the
  comment, and if it is absolutely necessary to have blank lines within
  the comments, e.g. to make paragraph separations within a single comment,
  these blank lines *do* have a ``--`` (unlike the
  normal rule, which is to use entirely blank lines for separating
  comment paragraphs).  The comment starts at same level of indentation
  as code it is commenting.

  .. index:: Blank lines (in comments)

  .. index:: Indentation

  .. code-block:: ada

           z : Integer;
           --  Integer value for storing value of z
           --
           --  The previous line was a blank line.

* Comments that are dubious or incomplete, or that comment on possibly
  wrong or incomplete code, should be preceded or followed by ``???``.

* Comments in a subprogram body must generally be surrounded by blank lines.
  An exception is a comment that follows a line containing a single keyword
  ( ``begin``, ``else``, ``loop`` ):

  .. code-block:: ada

           begin
              --  Comment for the next statement

              A := 5;

              --  Comment for the B statement

              B := 6;
           end;

* In sequences of statements, comments at the end of the lines should be
  aligned.

  .. index:: Alignment (in comments)

  .. code-block:: ada

            My_Identifier := 5;      --  First comment
            Other_Id := 6;           --  Second comment

* Short comments that fit on a single line are *not* ended with a
  period.  Comments taking more than a line are punctuated in the normal
  manner.

* Comments should focus on *why* instead of *what*.
  Descriptions of what subprograms do go with the specification.

* Comments describing a subprogram spec should specifically mention the
  formal argument names.  General rule: write a comment that does not
  depend on the names of things.  The names are supplementary, not
  sufficient, as comments.

* *Do not* put two spaces after periods in comments.

..  -

