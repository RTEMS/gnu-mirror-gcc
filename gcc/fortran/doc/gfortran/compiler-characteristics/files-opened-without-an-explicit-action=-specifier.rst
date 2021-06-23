..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _files-opened-without-an-explicit-action=-specifier:

Files opened without an explicit ACTION= specifier
**************************************************

.. index:: open, action

The Fortran standard says that if an ``OPEN`` statement is executed
without an explicit ``ACTION=`` specifier, the default value is
processor dependent.  GNU Fortran behaves as follows:

* Attempt to open the file with ``ACTION='READWRITE'``

* If that fails, try to open with ``ACTION='READ'``

* If that fails, try to open with ``ACTION='WRITE'``

* If that fails, generate an error