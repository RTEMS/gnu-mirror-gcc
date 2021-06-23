..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _error-and-warning-options:

Options to request or suppress errors and warnings
**************************************************

.. index:: options, warnings

.. index:: options, errors

.. index:: warnings, suppressing

.. index:: messages, error

.. index:: messages, warning

.. index:: suppressing warnings

Errors are diagnostic messages that report that the GNU Fortran compiler
cannot compile the relevant piece of source code.  The compiler will
continue to process the program in an attempt to report further errors
to aid in debugging, but will not produce any compiled output.

Warnings are diagnostic messages that report constructions which
are not inherently erroneous but which are risky or suggest there is
likely to be a bug in the program.  Unless :option:`-Werror` is specified,
they do not prevent compilation of the program.

You can request many specific warnings with options beginning :option:`-W`,
for example :option:`-Wimplicit` to request warnings on implicit
declarations.  Each of these specific warning options also has a
negative form beginning :option:`-Wno-` to turn off warnings;
for example, :option:`-Wno-implicit`.  This manual lists only one of the
two forms, whichever is not the default.

These options control the amount and kinds of errors and warnings produced
by GNU Fortran:

.. option:: -fmax-errors=n

  .. index:: fmax-errors=n

  .. index:: errors, limiting

  Limits the maximum number of error messages to :samp:`{n}`, at which point
  GNU Fortran bails out rather than attempting to continue processing the
  source code.  If :samp:`{n}` is 0, there is no limit on the number of error
  messages produced.

.. option:: -fsyntax-only

  .. index:: fsyntax-only

  .. index:: syntax checking

  Check the code for syntax errors, but do not actually compile it.  This
  will generate module files for each module present in the code, but no
  other output file.

.. option:: -Wpedantic, -pedantic

  .. index:: pedantic

  .. index:: Wpedantic

  Issue warnings for uses of extensions to Fortran.
  :option:`-pedantic` also applies to C-language constructs where they
  occur in GNU Fortran source files, such as use of :samp:`\\e` in a
  character constant within a directive like ``#include``.

  Valid Fortran programs should compile properly with or without
  this option.
  However, without this option, certain GNU extensions and traditional
  Fortran features are supported as well.
  With this option, many of them are rejected.

  Some users try to use :option:`-pedantic` to check programs for conformance.
  They soon find that it does not do quite what they want---it finds some
  nonstandard practices, but not all.
  However, improvements to GNU Fortran in this area are welcome.

  This should be used in conjunction with :option:`-std`:samp:`=f95`,
  :option:`-std`:samp:`=f2003`, :option:`-std`:samp:`=f2008` or :option:`-std`:samp:`=f2018`.

.. option:: -pedantic-errors

  .. index:: pedantic-errors

  Like :option:`-pedantic`, except that errors are produced rather than
  warnings.

.. option:: -Wall

  .. index:: Wall

  .. index:: all warnings

  .. index:: warnings, all

  Enables commonly used warning options pertaining to usage that
  we recommend avoiding and that we believe are easy to avoid.
  This currently includes :option:`-Waliasing`, :option:`-Wampersand`,
  :option:`-Wconversion`, :option:`-Wsurprising`, :option:`-Wc-binding-type`,
  :option:`-Wintrinsics-std`, :option:`-Wtabs`, :option:`-Wintrinsic-shadow`,
  :option:`-Wline-truncation`, :option:`-Wtarget-lifetime`,
  :option:`-Winteger-division`, :option:`-Wreal-q-constant`, :option:`-Wunused`
  and :option:`-Wundefined-do-loop`.

.. option:: -Waliasing

  .. index:: Waliasing

  .. index:: aliasing

  .. index:: warnings, aliasing

  Warn about possible aliasing of dummy arguments. Specifically, it warns
  if the same actual argument is associated with a dummy argument with
  ``INTENT(IN)`` and a dummy argument with ``INTENT(OUT)`` in a call
  with an explicit interface.

  The following example will trigger the warning.

  .. code-block:: fortran

      interface
        subroutine bar(a,b)
          integer, intent(in) :: a
          integer, intent(out) :: b
        end subroutine
      end interface
      integer :: a

      call bar(a,a)

.. option:: -Wampersand

  .. index:: Wampersand

  .. index:: warnings, ampersand

  .. index:: &

  Warn about missing ampersand in continued character constants. The
  warning is given with :option:`-Wampersand`, :option:`-pedantic`,
  :option:`-std`:samp:`=f95`, :option:`-std`:samp:`=f2003`, :option:`-std`:samp:`=f2008` and
  :option:`-std`:samp:`=f2018`. Note: With no ampersand given in a continued
  character constant, GNU Fortran assumes continuation at the first
  non-comment, non-whitespace character after the ampersand that
  initiated the continuation.

.. option:: -Warray-temporaries

  .. index:: Warray-temporaries

  .. index:: warnings, array temporaries

  Warn about array temporaries generated by the compiler.  The information
  generated by this warning is sometimes useful in optimization, in order to
  avoid such temporaries.

.. option:: -Wc-binding-type

  .. index:: Wc-binding-type

  .. index:: warning, C binding type

  Warn if the a variable might not be C interoperable.  In particular, warn if
  the variable has been declared using an intrinsic type with default kind
  instead of using a kind parameter defined for C interoperability in the
  intrinsic ``ISO_C_Binding`` module.  This option is implied by
  :option:`-Wall`.

.. option:: -Wcharacter-truncation

  .. index:: Wcharacter-truncation

  .. index:: warnings, character truncation

  Warn when a character assignment will truncate the assigned string.

.. option:: -Wline-truncation

  .. index:: Wline-truncation

  .. index:: warnings, line truncation

  Warn when a source code line will be truncated.  This option is
  implied by :option:`-Wall`.  For free-form source code, the default is
  :option:`-Werror`:samp:`=line-truncation` such that truncations are reported as
  error.

.. option:: -Wconversion

  .. index:: Wconversion

  .. index:: warnings, conversion

  .. index:: conversion

  Warn about implicit conversions that are likely to change the value of
  the expression after conversion. Implied by :option:`-Wall`.

.. option:: -Wconversion-extra

  .. index:: Wconversion-extra

  .. index:: warnings, conversion

  .. index:: conversion

  Warn about implicit conversions between different types and kinds. This
  option does *not* imply :option:`-Wconversion`.

.. option:: -Wextra

  .. index:: Wextra

  .. index:: extra warnings

  .. index:: warnings, extra

  Enables some warning options for usages of language features which
  may be problematic. This currently includes :option:`-Wcompare-reals`,
  :option:`-Wunused-parameter` and :option:`-Wdo-subscript`.

.. option:: -Wfrontend-loop-interchange

  .. index:: Wfrontend-loop-interchange

  .. index:: warnings, loop interchange

  .. index:: loop interchange, warning

  Warn when using :option:`-ffrontend-loop-interchange` for performing loop
  interchanges.

.. option:: -Wimplicit-interface

  .. index:: Wimplicit-interface

  .. index:: warnings, implicit interface

  Warn if a procedure is called without an explicit interface.
  Note this only checks that an explicit interface is present.  It does not
  check that the declared interfaces are consistent across program units.

.. option:: -Wimplicit-procedure

  .. index:: Wimplicit-procedure

  .. index:: warnings, implicit procedure

  Warn if a procedure is called that has neither an explicit interface
  nor has been declared as ``EXTERNAL``.

.. option:: -Winteger-division

  .. index:: Winteger-division

  .. index:: warnings, integer division

  .. index:: warnings, division of integers

  Warn if a constant integer division truncates its result.
  As an example, 3/5 evaluates to 0.

.. option:: -Wintrinsics-std

  .. index:: Wintrinsics-std

  .. index:: warnings, non-standard intrinsics

  .. index:: warnings, intrinsics of other standards

  Warn if :command:`gfortran` finds a procedure named like an intrinsic not
  available in the currently selected standard (with :option:`-std` ) and treats
  it as ``EXTERNAL`` procedure because of this.  :option:`-fall-intrinsics` can
  be used to never trigger this behavior and always link to the intrinsic
  regardless of the selected standard.

.. option:: -Wno-overwrite-recursive

  .. index:: Woverwrite-recursive

  .. index:: warnings, overwrite recursive

  Do not warn when :option:`-fno-automatic` is used with :option:`-frecursive`. Recursion
  will be broken if the relevant local variables do not have the attribute
  ``AUTOMATIC`` explicitly declared. This option can be used to suppress the warning
  when it is known that recursion is not broken. Useful for build environments that use
  :option:`-Werror`.

.. option:: -Wreal-q-constant

  .. index:: Wreal-q-constant

  .. index:: warnings, q exponent-letter

  Produce a warning if a real-literal-constant contains a ``q``
  exponent-letter.

.. option:: -Wsurprising

  .. index:: Wsurprising

  .. index:: warnings, suspicious code

  Produce a warning when 'suspicious' code constructs are encountered.
  While technically legal these usually indicate that an error has been made.

  This currently produces a warning under the following circumstances:

  * An INTEGER SELECT construct has a CASE that can never be matched as its
    lower value is greater than its upper value.

  * A LOGICAL SELECT construct has three CASE statements.

  * A TRANSFER specifies a source that is shorter than the destination.

  * The type of a function result is declared more than once with the same type.  If
    :option:`-pedantic` or standard-conforming mode is enabled, this is an error.

  * A ``CHARACTER`` variable is declared with negative length.

.. option:: -Wtabs

  .. index:: Wtabs

  .. index:: warnings, tabs

  .. index:: tabulators

  By default, tabs are accepted as whitespace, but tabs are not members
  of the Fortran Character Set.  For continuation lines, a tab followed
  by a digit between 1 and 9 is supported.  :option:`-Wtabs` will cause a
  warning to be issued if a tab is encountered. Note, :option:`-Wtabs` is
  active for :option:`-pedantic`, :option:`-std`:samp:`=f95`, :option:`-std`:samp:`=f2003`,
  :option:`-std`:samp:`=f2008`, :option:`-std`:samp:`=f2018` and
  :option:`-Wall`.

.. option:: -Wundefined-do-loop

  .. index:: Wundefined-do-loop

  .. index:: warnings, undefined do loop

  Warn if a DO loop with step either 1 or -1 yields an underflow or an overflow
  during iteration of an induction variable of the loop.
  This option is implied by :option:`-Wall`.

.. option:: -Wunderflow

  .. index:: Wunderflow

  .. index:: warnings, underflow

  .. index:: underflow

  Produce a warning when numerical constant expressions are
  encountered, which yield an UNDERFLOW during compilation. Enabled by default.

.. option:: -Wintrinsic-shadow

  .. index:: Wintrinsic-shadow

  .. index:: warnings, intrinsic

  .. index:: intrinsic

  Warn if a user-defined procedure or module procedure has the same name as an
  intrinsic; in this case, an explicit interface or ``EXTERNAL`` or
  ``INTRINSIC`` declaration might be needed to get calls later resolved to
  the desired intrinsic/procedure.  This option is implied by :option:`-Wall`.

.. option:: -Wuse-without-only

  .. index:: Wuse-without-only

  .. index:: warnings, use statements

  .. index:: intrinsic

  Warn if a ``USE`` statement has no ``ONLY`` qualifier and
  thus implicitly imports all public entities of the used module.

.. option:: -Wunused-dummy-argument

  .. index:: Wunused-dummy-argument

  .. index:: warnings, unused dummy argument

  .. index:: unused dummy argument

  .. index:: dummy argument, unused

  Warn about unused dummy arguments. This option is implied by :option:`-Wall`.

.. option:: -Wunused-parameter

  .. index:: Wunused-parameter

  .. index:: warnings, unused parameter

  .. index:: unused parameter

  Contrary to :command:`gcc`'s meaning of :option:`-Wunused-parameter`,
  :command:`gfortran`'s implementation of this option does not warn
  about unused dummy arguments (see :option:`-Wunused-dummy-argument` ),
  but about unused ``PARAMETER`` values. :option:`-Wunused-parameter`
  is implied by :option:`-Wextra` if also :option:`-Wunused` or
  :option:`-Wall` is used.

.. option:: -Walign-commons

  .. index:: Walign-commons

  .. index:: warnings, alignment of COMMON blocks

  .. index:: alignment of COMMON blocks

  By default, :command:`gfortran` warns about any occasion of variables being
  padded for proper alignment inside a ``COMMON`` block. This warning can be turned
  off via :option:`-Wno-align-commons`. See also :option:`-falign-commons`.

.. option:: -Wfunction-elimination

  .. index:: Wfunction-elimination

  .. index:: function elimination

  .. index:: warnings, function elimination

  Warn if any calls to impure functions are eliminated by the optimizations
  enabled by the :option:`-ffrontend-optimize` option.
  This option is implied by :option:`-Wextra`.

.. option:: -Wrealloc-lhs

  .. index:: Wrealloc-lhs

  .. index:: Reallocate the LHS in assignments, notification

  Warn when the compiler might insert code to for allocation or reallocation of
  an allocatable array variable of intrinsic type in intrinsic assignments.  In
  hot loops, the Fortran 2003 reallocation feature may reduce the performance.
  If the array is already allocated with the correct shape, consider using a
  whole-array array-spec (e.g. ``(:,:,:)`` ) for the variable on the left-hand
  side to prevent the reallocation check. Note that in some cases the warning
  is shown, even if the compiler will optimize reallocation checks away.  For
  instance, when the right-hand side contains the same variable multiplied by
  a scalar.  See also :option:`-frealloc-lhs`.

.. option:: -Wrealloc-lhs-all

  .. index:: Wrealloc-lhs-all

  Warn when the compiler inserts code to for allocation or reallocation of an
  allocatable variable; this includes scalars and derived types.

.. option:: -Wcompare-reals

  .. index:: Wcompare-reals

  Warn when comparing real or complex types for equality or inequality.
  This option is implied by :option:`-Wextra`.

.. option:: -Wtarget-lifetime

  .. index:: Wtargt-lifetime

  Warn if the pointer in a pointer assignment might be longer than the its
  target. This option is implied by :option:`-Wall`.

.. option:: -Wzerotrip

  .. index:: Wzerotrip

  Warn if a ``DO`` loop is known to execute zero times at compile
  time.  This option is implied by :option:`-Wall`.

.. option:: -Wdo-subscript

  .. index:: Wdo-subscript

  Warn if an array subscript inside a DO loop could lead to an
  out-of-bounds access even if the compiler cannot prove that the
  statement is actually executed, in cases like

  .. code-block:: fortran

      real a(3)
      do i=1,4
        if (condition(i)) then
          a(i) = 1.2
        end if
      end do

  This option is implied by :option:`-Wextra`.

.. option:: -Werror

  .. index:: Werror

  .. index:: warnings, to errors

  Turns all warnings into errors.

See :ref:`Options to Request or Suppress Errors and
Warnings <warning-options>`, for information on
more options offered by the GBE shared by :command:`gfortran`, :command:`gcc`
and other GNU compilers.

Some of these have no effect when compiling programs written in Fortran.