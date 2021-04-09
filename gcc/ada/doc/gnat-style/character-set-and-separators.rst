Character Set and Separators
****************************

..  -

.. index:: Character set

.. index:: ASCII

.. index:: Separators

.. index:: End-of-line

.. index:: Line length

.. index:: Indentation

* The character set used should be plain 7-bit ASCII.
  The only separators allowed are space and the end-of-line sequence.
  No other control character or format effector (such as ``HT``,
  ``VT``, ``FF`` )
  should be used.
  The normal end-of-line sequence is used, which may be
  ``LF``, ``CR/LF`` or ``CR``,
  depending on the host system.  An optional ``SUB``
  ( ``16#1A#`` ) may be present as the
  last character in the file on hosts using that character as file terminator.

* Files that are checked in or distributed should be in host format.

* A line should never be longer than 79 characters, not counting the line
  separator.

* Lines must not have trailing blanks.

* Indentation is 3 characters per level for ``if`` statements, loops, and
  ``case`` statements.
  For exact information on required spacing between lexical
  elements, see file style.adb.

  .. index:: style.adb file

