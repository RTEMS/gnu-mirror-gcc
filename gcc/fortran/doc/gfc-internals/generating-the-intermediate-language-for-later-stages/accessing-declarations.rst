..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _accessing-declarations:

Accessing declarations
**********************

``gfc_symbol``, ``gfc_charlen`` and other front-end structures
contain a ``backend_decl`` variable, which contains the ``tree``
used for accessing that entity in the middle-end.

Accessing declarations is usually done by functions called
``gfc_get*``.

.. -
   LibGFortran
   -