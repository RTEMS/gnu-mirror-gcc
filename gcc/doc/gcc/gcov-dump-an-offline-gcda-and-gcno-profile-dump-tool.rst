..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

    .. _gcov-dump:

gcov-dump---an Offline Gcda and Gcno Profile Dump Tool
------------------------------------------------------

Description
^^^^^^^^^^^

:command:`gcov-dump` is a tool you can use in conjunction with GCC to
dump content of gcda and gcno profile files offline.

.. only:: man

  Synopsis
  ^^^^^^^^

  gcov-dump
       [ :option:`-v` | :option:`--version` ]
       [ :option:`-h` | :option:`--help` ]
       [ :option:`-l` | :option:`--long` ]
       [ :option:`-p` | :option:`--positions` ]
       [ :option:`-r` | :option:`--raw` ]
       [ :samp:`{gcovfiles}` ]

Options
^^^^^^^

``-h`` ``--help``
  Display help about using :command:`gcov-dump` (on the standard output), and
  exit without doing any further processing.

``-l`` ``--long``
  Dump content of records.

``-p`` ``--positions``
  Dump positions of records.

``-r`` ``--raw``
  Print content records in raw format.

``-v`` ``--version``
  Display the :command:`gcov-dump` version number (on the standard output),
  and exit without doing any further processing.

.. only:: man

  .. include:: copyright.rst