..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _lto-dump:

lto-dump---Tool for dumping LTO object files.
---------------------------------------------

Description
^^^^^^^^^^^

:command:`lto-dump` is a tool you can use in conjunction with GCC to
dump link time optimization object files.

Synopsis
^^^^^^^^

lto-dump
     [ :option:`-list` ]
     [ :option:`-demangle` ]
     [ :option:`-defined-only` ]
     [ :option:`-print-value` ]
     [ :option:`-name-sort` ]
     [ :option:`-size-sort` ]
     [ :option:`-reverse-sort` ]
     [ :option:`-no-sort` ]
     [ :option:`-symbol` =]
     [ :option:`-objects` ]
     [ :option:`-type-stats` ]
     [ :option:`-tree-stats` ]
     [ :option:`-gimple-stats` ]
     [ :option:`-dump-level` =]
     [ :option:`-dump-body` =]
     [ :option:`-help` ] :samp:`{lto-dump}`

Options
^^^^^^^

``-list``
  Dumps list of details of functions and variables.

``-demangle``
  Dump the demangled output.

``-defined-only``
  Dump only the defined symbols.

``-print-value``
  Dump initial values of the variables.

``-name-sort``
  Sort the symbols alphabetically.

``-size-sort``
  Sort the symbols according to size.

``-reverse-sort``
  Dump the symbols in reverse order.

``-no-sort``
  Dump the symbols in order of occurrence.

``-symbol=``
  Dump the details of specific symbol.

``-objects``
  Dump the details of LTO objects.

``-type-stats``
  Dump the statistics of tree types.

``-tree-stats``
  Dump the statistics of trees.

``-gimple-stats``
  Dump the statistics of gimple statements.

``-dump-level=``
  For deciding the optimization level of body.

``-dump-body=``
  Dump the specific gimple body.

``-help``
  Display the dump tool help.


.. only:: man

  .. include:: copyright.rst