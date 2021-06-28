..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _warnings:

Warnings
********

.. index:: options to control warnings

.. index:: warning messages

.. index:: messages, warning

.. index:: suppressing warnings

Warnings are diagnostic messages that report constructions that
are not inherently erroneous but that are risky or suggest there
is likely to be a bug in the program.  Unless :option:`-Werror` is
specified, they do not prevent compilation of the program.

``-Wall``

  .. index:: -Wall

  .. index:: -Wno-all

  Turns on all warnings messages.  Warnings are not a defined part of
  the D language, and all constructs for which this may generate a
  warning message are valid code.

``-Walloca``

  .. index:: -Walloca

  This option warns on all uses of "alloca" in the source.

:samp:`-Walloca-larger-than={n}`

  .. index:: -Walloca-larger-than

  .. index:: -Wno-alloca-larger-than

  Warn on unbounded uses of alloca, and on bounded uses of alloca
  whose bound can be larger than :samp:`{n}` bytes.
  :option:`-Wno-alloca-larger-than` disables
  :option:`-Walloca-larger-than` warning and is equivalent to
  :option:`-Walloca-larger-than`:samp:`={SIZE_MAX}` or larger.

``-Wcast-result``

  .. index:: -Wcast-result

  .. index:: -Wno-cast-result

  Warn about casts that will produce a null or zero result.  Currently
  this is only done for casting between an imaginary and non-imaginary
  data type, or casting between a D and C++ class.

``-Wno-deprecated``

  .. index:: -Wdeprecated

  .. index:: -Wno-deprecated

  Do not warn about usage of deprecated features and symbols with
  ``deprecated`` attributes.

``-Werror``

  .. index:: -Werror

  .. index:: -Wno-error

  Turns all warnings into errors.

``-Wspeculative``

  .. index:: -Wspeculative

  .. index:: -Wno-speculative

  List all error messages from speculative compiles, such as
  ``__traits(compiles, ...)``.  This option does not report
  messages as warnings, and these messages therefore never become
  errors when the :option:`-Werror` option is also used.

``-Wtemplates``

  .. index:: -Wtemplates

  .. index:: -Wno-templates

  Warn when a template instantiation is encountered.  Some coding
  rules disallow templates, and this may be used to enforce that rule.

``-Wunknown-pragmas``

  .. index:: -Wunknown-pragmas

  .. index:: -Wno-unknown-pragmas

  Warn when a ``pragma()`` is encountered that is not understood by
  :command:`gdc`.  This differs from :option:`-fignore-unknown-pragmas`
  where a pragma that is part of the D language, but not implemented by
  the compiler, won't get reported.

``-Wno-varargs``

  .. index:: Wvarargs

  .. index:: Wno-varargs

  Do not warn upon questionable usage of the macros used to handle variable
  arguments like ``va_start``.

``-fignore-unknown-pragmas``

  .. index:: -fignore-unknown-pragmas

  .. index:: -fno-ignore-unknown-pragmas

  Turns off errors for unsupported pragmas.

:samp:`-fmax-errors={n}`

  .. index:: -fmax-errors

  Limits the maximum number of error messages to :samp:`{n}`, at which point
  :command:`gdc` bails out rather than attempting to continue processing the
  source code.  If :samp:`{n}` is 0 (the default), there is no limit on the
  number of error messages produced.

``-fsyntax-only``

  .. index:: -fsyntax-only

  .. index:: -fno-syntax-only

  Check the code for syntax errors, but do not actually compile it.  This
  can be used in conjunction with :option:`-fdoc` or :option:`-H` to generate
  files for each module present on the command-line, but no other output
  file.

:samp:`-ftransition={id}`

  .. index:: -ftransition

  Report additional information about D language changes identified by
  :samp:`{id}`.  The following values are supported:

  :samp:`all`
    List information on all language changes.

  :samp:`complex`
    List all usages of complex or imaginary types.

  :samp:`dip1000`
    Implements http://wiki.dlang.org/DIP1000 (experimental).

  :samp:`dip25`
    Implements http://wiki.dlang.org/DIP25 (experimental).

  :samp:`field`
    List all non-mutable fields which occupy an object instance.

  :samp:`nogc`
    List all hidden GC allocations.

  :samp:`tls`
    List all variables going into thread local storage.