..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _preprocessing-options:

Enable and customize preprocessing
**********************************

.. index:: preprocessor

.. index:: options, preprocessor

.. index:: CPP

Preprocessor related options. See section
Preprocessing and conditional compilation for more detailed
information on preprocessing in :command:`gfortran`.

.. option:: -cpp, -nocpp

  .. index:: cpp

  .. index:: fpp

  .. index:: preprocessor, enable

  .. index:: preprocessor, disable

  Enable preprocessing. The preprocessor is automatically invoked if
  the file extension is :samp:`.fpp`, :samp:`.FPP`,  :samp:`.F`, :samp:`.FOR`,
  :samp:`.FTN`, :samp:`.F90`, :samp:`.F95`, :samp:`.F03` or :samp:`.F08`. Use
  this option to manually enable preprocessing of any kind of Fortran file.

  To disable preprocessing of files with any of the above listed extensions,
  use the negative form: :option:`-nocpp`.

  The preprocessor is run in traditional mode. Any restrictions of the
  file-format, especially the limits on line length, apply for
  preprocessed output as well, so it might be advisable to use the
  :option:`-ffree-line-length-none` or :option:`-ffixed-line-length-none`
  options.

.. option:: -dM

  .. index:: dM

  .. index:: preprocessor, debugging

  .. index:: debugging, preprocessor

  Instead of the normal output, generate a list of ``'#define'``
  directives for all the macros defined during the execution of the
  preprocessor, including predefined macros. This gives you a way
  of finding out what is predefined in your version of the preprocessor.
  Assuming you have no file :samp:`foo.f90`, the command

  .. code-block:: fortran

      touch foo.f90; gfortran -cpp -E -dM foo.f90

  will show all the predefined macros.

.. option:: -dD

  .. index:: dD

  .. index:: preprocessor, debugging

  .. index:: debugging, preprocessor

  Like :option:`-dM` except in two respects: it does not include the
  predefined macros, and it outputs both the ``#define`` directives
  and the result of preprocessing. Both kinds of output go to the
  standard output file.

.. option:: -dN

  .. index:: dN

  .. index:: preprocessor, debugging

  .. index:: debugging, preprocessor

  Like :option:`-dD`, but emit only the macro names, not their expansions.

.. option:: -dU

  .. index:: dU

  .. index:: preprocessor, debugging

  .. index:: debugging, preprocessor

  Like dD except that only macros that are expanded, or whose
  definedness is tested in preprocessor directives, are output; the
  output is delayed until the use or test of the macro; and ``'#undef'``
  directives are also output for macros tested but undefined at the time.

.. option:: -dI

  .. index:: dI

  .. index:: preprocessor, debugging

  .. index:: debugging, preprocessor

  Output ``'#include'`` directives in addition to the result
  of preprocessing.

.. option:: -fworking-directory

  .. index:: fworking-directory

  .. index:: preprocessor, working directory

  Enable generation of linemarkers in the preprocessor output that will
  let the compiler know the current working directory at the time of
  preprocessing. When this option is enabled, the preprocessor will emit,
  after the initial linemarker, a second linemarker with the current
  working directory followed by two slashes. GCC will use this directory,
  when it is present in the preprocessed input, as the directory emitted
  as the current working directory in some debugging information formats.
  This option is implicitly enabled if debugging information is enabled,
  but this can be inhibited with the negated form
  :option:`-fno-working-directory`. If the :option:`-P` flag is present
  in the command line, this option has no effect, since no ``#line``
  directives are emitted whatsoever.

.. option:: -idirafter dir

  .. index:: idirafter dir

  .. index:: preprocessing, include path

  Search :samp:`{dir}` for include files, but do it after all directories
  specified with :option:`-I` and the standard system directories have
  been exhausted. :samp:`{dir}` is treated as a system include directory.
  If dir begins with ``=``, then the ``=`` will be replaced by
  the sysroot prefix; see :option:`--sysroot` and :option:`-isysroot`.

.. option:: -imultilib dir

  .. index:: imultilib dir

  .. index:: preprocessing, include path

  Use :samp:`{dir}` as a subdirectory of the directory containing target-specific
  C++ headers.

.. option:: -iprefix prefix

  .. index:: iprefix prefix

  .. index:: preprocessing, include path

  Specify :samp:`{prefix}` as the prefix for subsequent :option:`-iwithprefix`
  options. If the :samp:`{prefix}` represents a directory, you should include
  the final ``'/'``.

.. option:: -isysroot dir

  .. index:: isysroot dir

  .. index:: preprocessing, include path

  This option is like the :option:`--sysroot` option, but applies only to
  header files. See the :option:`--sysroot` option for more information.

.. option:: -iquote dir

  .. index:: iquote dir

  .. index:: preprocessing, include path

  Search :samp:`{dir}` only for header files requested with ``#include "file"`` ;
  they are not searched for ``#include <file>``, before all directories
  specified by :option:`-I` and before the standard system directories. If
  :samp:`{dir}` begins with ``=``, then the ``=`` will be replaced by the
  sysroot prefix; see :option:`--sysroot` and :option:`-isysroot`.

.. option:: -isystem dir

  .. index:: isystem dir

  .. index:: preprocessing, include path

  Search :samp:`{dir}` for header files, after all directories specified by
  :option:`-I` but before the standard system directories. Mark it as a
  system directory, so that it gets the same special treatment as is
  applied to the standard system directories. If :samp:`{dir}` begins with
  ``=``, then the ``=`` will be replaced by the sysroot prefix;
  see :option:`--sysroot` and :option:`-isysroot`.

.. option:: -nostdinc

  .. index:: nostdinc

  Do not search the standard system directories for header files. Only
  the directories you have specified with :option:`-I` options (and the
  directory of the current file, if appropriate) are searched.

.. option:: -undef

  .. index:: undef

  Do not predefine any system-specific or GCC-specific macros.
  The standard predefined macros remain defined.

.. option:: -Apredicate=answer

  .. index:: Apredicate=answer

  .. index:: preprocessing, assertion

  Make an assertion with the predicate :samp:`{predicate}` and answer :samp:`{answer}`.
  This form is preferred to the older form -A predicate(answer), which is still
  supported, because it does not use shell special characters.

.. option:: -A-predicate=answer

  .. index:: A-predicate=answer

  .. index:: preprocessing, assertion

  Cancel an assertion with the predicate :samp:`{predicate}` and answer :samp:`{answer}`.

.. option:: -C

  .. index:: C

  .. index:: preprocessing, keep comments

  Do not discard comments. All comments are passed through to the output
  file, except for comments in processed directives, which are deleted
  along with the directive.

  You should be prepared for side effects when using :option:`-C` ; it causes
  the preprocessor to treat comments as tokens in their own right. For example,
  comments appearing at the start of what would be a directive line have the
  effect of turning that line into an ordinary source line, since the first
  token on the line is no longer a ``'#'``.

  Warning: this currently handles C-Style comments only. The preprocessor
  does not yet recognize Fortran-style comments.

.. option:: -CC

  .. index:: CC

  .. index:: preprocessing, keep comments

  Do not discard comments, including during macro expansion. This is like
  :option:`-C`, except that comments contained within macros are also passed
  through to the output file where the macro is expanded.

  In addition to the side-effects of the :option:`-C` option, the :option:`-CC`
  option causes all C++-style comments inside a macro to be converted to C-style
  comments. This is to prevent later use of that macro from inadvertently
  commenting out the remainder of the source line. The :option:`-CC` option
  is generally used to support lint comments.

  Warning: this currently handles C- and C++-Style comments only. The
  preprocessor does not yet recognize Fortran-style comments.

.. option:: -Dname

  .. index:: Dname

  .. index:: preprocessing, define macros

  Predefine name as a macro, with definition ``1``.

.. option:: -Dname=definition

  .. index:: Dname=definition

  .. index:: preprocessing, define macros

  The contents of :samp:`{definition}` are tokenized and processed as if they
  appeared during translation phase three in a ``'#define'`` directive.
  In particular, the definition will be truncated by embedded newline
  characters.

  If you are invoking the preprocessor from a shell or shell-like program
  you may need to use the shell's quoting syntax to protect characters such
  as spaces that have a meaning in the shell syntax.

  If you wish to define a function-like macro on the command line, write
  its argument list with surrounding parentheses before the equals sign
  (if any). Parentheses are meaningful to most shells, so you will need
  to quote the option. With sh and csh, ``-D'name(args...)=definition'``
  works.

  :option:`-D` and :option:`-U` options are processed in the order they are
  given on the command line. All -imacros file and -include file options
  are processed after all -D and -U options.

.. option:: -H

  .. index:: H

  Print the name of each header file used, in addition to other normal
  activities. Each name is indented to show how deep in the ``'#include'``
  stack it is.

.. option:: -P

  .. index:: P

  .. index:: preprocessing, no linemarkers

  Inhibit generation of linemarkers in the output from the preprocessor.
  This might be useful when running the preprocessor on something that
  is not C code, and will be sent to a program which might be confused
  by the linemarkers.

.. option:: -Uname

  .. index:: Uname

  .. index:: preprocessing, undefine macros

  Cancel any previous definition of :samp:`{name}`, either built in or provided
  with a :option:`-D` option.