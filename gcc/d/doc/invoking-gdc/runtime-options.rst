..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _runtime-options:

Runtime Options
***************

.. index:: options, runtime

These options affect the runtime behavior of programs compiled with
:command:`gdc`.

``-fall-instantiations``

  .. index:: -fall-instantiations

  .. index:: -fno-all-instantiations

  Generate code for all template instantiations.  The default template emission
  strategy is to not generate code for declarations that were either
  instantiated speculatively, such as from ``__traits(compiles, ...)``, or
  that come from an imported module not being compiled.

``-fno-assert``

  .. index:: -fassert

  .. index:: -fno-assert

  Turn off code generation for ``assert`` contracts.

``-fno-bounds-check``

  .. index:: -fbounds-check

  .. index:: -fno-bounds-check

  Turns off array bounds checking for all functions, which can improve
  performance for code that uses arrays extensively.  Note that this
  can result in unpredictable behavior if the code in question actually
  does violate array bounds constraints.  It is safe to use this option
  if you are sure that your code never throws a ``RangeError``.

:samp:`-fbounds-check={value}`

  .. index:: -fbounds-check=

  An alternative to :option:`-fbounds-check` that allows more control
  as to where bounds checking is turned on or off.  The following values
  are supported:

  :samp:`on`
    Turns on array bounds checking for all functions.

  :samp:`safeonly`
    Turns on array bounds checking only for ``@safe`` functions.

  :samp:`off`
    Turns off array bounds checking completely.

``-fno-builtin``

  .. index:: -fbuiltin

  .. index:: -fno-builtin

  Don't recognize built-in functions unless they begin with the prefix
  :samp:`__builtin_`.  By default, the compiler will recognize when a
  function in the ``core.stdc`` package is a built-in function.

  ``-fdebug``
  :samp:`-fdebug={value}`

  .. index:: -fdebug

  .. index:: -fno-debug

  Turn on compilation of conditional ``debug`` code into the program.
  The :option:`-fdebug` option itself sets the debug level to ``1``,
  while :option:`-fdebug` = enables ``debug`` code that are identified
  by any of the following values:

  :samp:`level`
    Sets the debug level to :samp:`{level}`, any ``debug`` code <= :samp:`{level}`
    is compiled into the program.

  :samp:`ident`
    Turns on compilation of any ``debug`` code identified by :samp:`{ident}`.

``-fno-druntime``

  .. index:: -fdruntime

  .. index:: -fno-druntime

  Implements https://dlang.org/spec/betterc.html.  Assumes that
  compilation targets an environment without a D runtime library.

  This is equivalent to compiling with the following options:

  .. code-block:: c++

    gdc -nophoboslib -fno-exceptions -fno-moduleinfo -fno-rtti

``-fno-invariants``

  .. index:: -finvariants

  .. index:: -fno-invariants

  Turns off code generation for class ``invariant`` contracts.

``-fno-moduleinfo``

  .. index:: -fmoduleinfo

  .. index:: -fno-moduleinfo

  Turns off generation of the ``ModuleInfo`` and related functions
  that would become unreferenced without it, which may allow linking
  to programs not written in D.  Functions that are not be generated
  include module constructors and destructors ( ``static this`` and
  ``static ~this`` ), ``unittest`` code, and ``DSO`` registry
  functions for dynamically linked code.

:samp:`-fonly={filename}`

  .. index:: -fonly

  Tells the compiler to parse and run semantic analysis on all modules
  on the command line, but only generate code for the module specified
  by :samp:`{filename}`.

``-fno-postconditions``

  .. index:: -fpostconditions

  .. index:: -fno-postconditions

  Turns off code generation for postcondition ``out`` contracts.

``-fno-preconditions``

  .. index:: -fpreconditions

  .. index:: -fno-preconditions

  Turns off code generation for precondition ``in`` contracts.

``-frelease``

  .. index:: -frelease

  .. index:: -fno-release

  Turns on compiling in release mode, which means not emitting runtime
  checks for contracts and asserts.  Array bounds checking is not done
  for ``@system`` and ``@trusted`` functions, and assertion
  failures are undefined behavior.

  This is equivalent to compiling with the following options:

  .. code-block:: c++

    gdc -fno-assert -fbounds-check=safe -fno-invariants \
        -fno-postconditions -fno-preconditions -fno-switch-errors

``-fno-rtti``

  .. index:: -frtti

  .. index:: -fno-rtti

  Turns off generation of run-time type information for all user defined types.
  Any code that uses features of the language that require access to this
  information will result in an error.

``-fno-switch-errors``

  .. index:: -fswitch-errors

  .. index:: -fno-switch-errors

  This option controls what code is generated when no case is matched
  in a ``final switch`` statement.  The default run time behavior
  is to throw a ``SwitchError``.  Turning off :option:`-fswitch-errors`
  means that instead the execution of the program is immediately halted.

``-funittest``

  .. index:: -funittest

  .. index:: -fno-unittest

  Turns on compilation of ``unittest`` code, and turns on the
  ``version(unittest)`` identifier.  This implies :option:`-fassert`.

:samp:`-fversion={value}`

  .. index:: -fversion

  Turns on compilation of conditional ``version`` code into the program
  identified by any of the following values:

  :samp:`level`
    Sets the version level to :samp:`{level}`, any ``version`` code >= :samp:`{level}`
    is compiled into the program.

  :samp:`ident`
    Turns on compilation of ``version`` code identified by :samp:`{ident}`.

``-fno-weak-templates``

  .. index:: -fweak-templates

  .. index:: -fno-weak-templates

  Turns off emission of declarations that can be defined in multiple objects as
  weak symbols.  The default is to emit all public symbols as weak, unless the
  target lacks support for weak symbols.  Disabling this option means that common
  symbols are instead put in COMDAT or become private.