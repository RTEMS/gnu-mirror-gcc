Identifiers
***********

..  -

.. index:: Identifiers

* Identifiers will start with an upper case letter, and each letter following
  an underscore will be upper case.

  .. index:: Casing (for identifiers)

  Short acronyms may be all upper case.
  All other letters are lower case.
  An exception is for identifiers matching a foreign language.  In particular,
  we use all lower case where appropriate for C.

* Use underscores to separate words in an identifier.

  .. index:: Underscores

* Try to limit your use of abbreviations in identifiers.
  It is ok to make a few abbreviations, explain what they mean, and then
  use them frequently, but don't use lots of obscure abbreviations.  An
  example is the ``ALI`` word which stands for Ada Library
  Information and is by convention always written in upper-case when
  used in entity names.

  .. code-block:: ada

           procedure Find_ALI_Files;

* Don't use the variable name ``I``, use ``J`` instead; ``I`` is too
  easily confused with ``1`` in some fonts.  Similarly don't use the
  variable ``O``, which is too easily mistaken for the number ``0``.

