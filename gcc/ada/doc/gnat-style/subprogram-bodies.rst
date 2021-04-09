Subprogram Bodies
*****************

..  -

.. index:: Subprogram bodies

* Function and procedure bodies should usually be sorted alphabetically. Do
  not attempt to sort them in some logical order by functionality. For a
  sequence of subprogram specs, a general alphabetical sorting is also
  usually appropriate, but occasionally it makes sense to group by major
  function, with appropriate headers.

* All subprograms have a header giving the function name, with the following
  format:

  .. code-block:: ada

          -----------------
          -- My_Function --
          -----------------

          procedure My_Function is
          begin
            ...
          end My_Function;

  Note that the name in the header is preceded by a single space,
  not two spaces as for other comments. These headers are used on
  nested subprograms as well as outer level subprograms. They may
  also be used as headers for sections of comments, or collections
  of declarations that are related.

* Every subprogram body must have a preceding subprogram_declaration,
  which includes proper client documentation so that you do not need to
  read the subprogram body in order to understand what the subprogram does and
  how to call it. All subprograms should be documented, without exceptions.

* 
  .. index:: Blank lines (in subprogram bodies)

  A sequence of declarations may optionally be separated from the following
  begin by a blank line.  Just as we optionally allow blank lines in general
  between declarations, this blank line should be present only if it improves
  readability. Generally we avoid this blank line if the declarative part is
  small (one or two lines) and the body has no blank lines, and we include it
  if the declarative part is long or if the body has blank lines.

* If the declarations in a subprogram contain at least one nested
  subprogram body, then just before the ``begin`` of the enclosing
  subprogram, there is a comment line and a blank line:

  .. code-block:: ada

        --  Start of processing for Enclosing_Subprogram

        begin
          ...
        end Enclosing_Subprogram;

* When nested subprograms are present, variables that are referenced by any
  nested subprogram should precede the nested subprogram specs. For variables
  that are not referenced by nested procedures, the declarations can either also
  be before any of the nested subprogram specs (this is the old style, more
  generally used). Or then can come just before the begin, with a header. The
  following example shows the two possible styles:

  .. code-block:: ada

        procedure Style1 is
           Var_Referenced_In_Nested      : Integer;
           Var_Referenced_Only_In_Style1 : Integer;

           proc Nested;
           --  Comments ...

           ------------
           -- Nested --
           ------------

           procedure Nested is
           begin
              ...
           end Nested;

        --  Start of processing for Style1

        begin
           ...
        end Style1;

        procedure Style2 is
           Var_Referenced_In_Nested : Integer;

           proc Nested;
           --  Comments ...

           ------------
           -- Nested --
           ------------

           procedure Nested is
           begin
              ...
           end Nested;

           --  Local variables

           Var_Referenced_Only_In_Style2 : Integer;

        --  Start of processing for Style2

        begin
           ...
        end Style2;

  For new code, we generally prefer Style2, but we do not insist on
  modifying all legacy occurrences of Style1, which is still much
  more common in the sources.

..  -

