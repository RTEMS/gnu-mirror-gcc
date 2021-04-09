Subprogram Declarations
***********************

..  -

* Do not write the ``in`` for parameters.

  .. code-block:: ada

          function Length (S : String) return Integer;

* When the declaration line for a procedure or a function is too long to fit
  the entire declaration (including the keyword procedure or function) on a
  single line, then fold it, putting a single parameter on a line, aligning
  the colons, as in:

  .. code-block:: ada

         procedure Set_Heading
           (Source : String;
            Count  : Natural;
            Pad    : Character := Space;
            Fill   : Boolean   := True);

  In the case of a function, if the entire spec does not fit on one line, then
  the return may appear after the last parameter, as in:

  .. code-block:: ada

          function Head
            (Source : String;
             Count  : Natural;
             Pad    : Character := Space) return String;

  Or it may appear on its own as a separate line. This form is preferred when
  putting the return on the same line as the last parameter would result in
  an overlong line. The return type may optionally be aligned with the types
  of the parameters (usually we do this aligning if it results only in a small
  number of extra spaces, and otherwise we don't attempt to align). So two
  alternative forms for the above spec are:

  .. code-block:: ada

          function Head
            (Source : String;
             Count  : Natural;
             Pad    : Character := Space)
             return   String;

          function Head
            (Source : String;
             Count  : Natural;
             Pad    : Character := Space)
             return String;

