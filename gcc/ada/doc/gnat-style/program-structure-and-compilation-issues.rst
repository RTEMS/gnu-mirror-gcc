.. _program-structure:

Program Structure and Compilation Issues
----------------------------------------

..  -

.. index:: Program structure

* Every GNAT source file must be compiled with the ``-gnatg``
  switch to check the coding style.
  (Note that you should look at
  style.adb to see the lexical rules enforced by ``-gnatg`` ).

  .. index:: -gnatg option (to gcc)

  .. index:: style.adb file

* Each source file should contain only one compilation unit.

* Filenames should be 8 or fewer characters, followed by the ``.adb``
  extension for a body or ``.ads`` for a spec.

  .. index:: File name length

* Unit names should be distinct when 'krunch'ed to 8 characters
  (see krunch.ads) and the filenames should match the unit name,
  except that they are all lower case.

  .. index:: krunch.ads file

.. **********************************
   * GNU Free Documentation License *
   **********************************

