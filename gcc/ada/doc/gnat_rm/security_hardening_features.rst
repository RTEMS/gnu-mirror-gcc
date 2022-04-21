.. _Security_Hardening_Features:

***************************
Security Hardening Features
***************************

This chapter describes Ada extensions aimed at security hardening that
are provided by GNAT.

.. Register Scrubbing:

Register Scrubbing
==================

GNAT can generate code to zero-out hardware registers before returning
from a subprogram.

It can be enabled with the :switch:`-fzero-call-used-regs` command-line
option, to affect all subprograms in a compilation, and with a
:samp:`Machine_Attribute` pragma, to affect only specific subprograms.

.. code-block:: ada

     procedure Foo;
     pragma Machine_Attribute (Foo, "zero_call_used_regs", "used");
     --  Before returning, Foo scrubs only call-clobbered registers
     --  that it uses itself.

     function Bar return Integer;
     pragma Machine_Attribute (Bar, "zero_call_used_regs", "all");
     --  Before returning, Bar scrubs all call-clobbered registers.


For usage and more details on the command-line option, on the
``zero_call_used_regs`` attribute, and on their use with other
programming languages, see :title:`Using the GNU Compiler Collection
(GCC)`.


.. Stack Scrubbing:

Stack Scrubbing
===============

GNAT can generate code to zero-out stack frames used by subprograms.

It can be activated with the :samp:`Machine_Attribute` pragma, on
specific subprograms and variables, or their types.  (This attribute
always applies to a type, even when it is associated with a subprogram
or a variable.)

.. code-block:: ada

     function Foo returns Integer;
     pragma Machine_Attribute (Foo, "strub");
     --  Foo and its callers are modified so as to scrub the stack
     --  space used by Foo after it returns.  Shorthand for:
     --  pragma Machine_Attribute (Foo, "strub", "at-calls");

     procedure Bar;
     pragma Machine_Attribute (Bar, "strub", "internal");
     --  Bar is turned into a wrapper for its original body,
     --  and they scrub the stack used by the original body.

     Var : Integer;
     pragma Machine_Attribute (Var, "strub");
     --  Reading from Var in a subprogram enables stack scrubbing
     --  of the stack space used by the subprogram.  Furthermore, if
     --  Var is declared within a subprogram, this also enables
     --  scrubbing of the stack space used by that subprogram.


There are also :switch:`-fstrub` command-line options to control
default settings.  For usage and more details on the command-line
option, on the ``strub`` attribute, and their use with other
programming languages, see :title:`Using the GNU Compiler Collection
(GCC)`.

Note that Ada secondary stacks are not scrubbed.  The restriction
``No_Secondary_Stack`` avoids their use, and thus their accidental
preservation of data that should be scrubbed.

Attributes ``Access`` and ``Unconstrained_Access`` of variables and
constants with ``strub`` enabled require types with ``strub`` enabled;
there is no way to express an access-to-strub type otherwise.
``Unchecked_Access`` bypasses this constraint, but the resulting
access type designates a non-strub type.

.. code-block:: ada

     VI : aliased Integer;
     pragma Machine_Attribute (VI, "strub");
     XsVI : access Integer := VI'Access; -- Error.
     UXsVI : access Integer := VI'Unchecked_Access; -- OK,
     --  UXsVI does *not* enable strub in subprograms that
     --  dereference it to obtain the UXsVI.all value.

     type Strub_Int is new Integer;
     pragma Machine_Attribute (Strub_Int, "strub");
     VSI : aliased Strub_Int;
     XsVSI : access Strub_Int := VSI'Access; -- OK,
     --  VSI and XsVSI.all both enable strub in subprograms that
     --  read their values.


Every access-to-subprogram type, renaming, and overriding and
overridden dispatching operations that may refer to a subprogram with
an attribute-modified interface must be annotated with the same
interface-modifying attribute.  Access-to-subprogram types can be
explicitly converted to different strub modes, as long as they are
interface-compatible (i.e., adding or removing ``at-calls`` is not
allowed).  For example, a ``strub``-``disabled`` subprogram can be
turned ``callable`` through such an explicit conversion:

.. code-block:: ada

     type TBar is access procedure;

     type TBar_Callable is access procedure;
     pragma Machine_Attribute (TBar_Callable, "strub", "callable");
     --  The attribute modifies the procedure type, rather than the
     --  access type, because of the extra argument after "strub",
     --  only applicable to subprogram types.

     Bar_Callable_Ptr : constant TBar_Callable
		:= TBar_Callable (TBar'(Bar'Access));

     procedure Bar_Callable renames Bar_Callable_Ptr.all;
     pragma Machine_Attribute (Bar_Callable, "strub", "callable");


Note that the renaming declaration is expanded to a full subprogram
body, it won't be just an alias.  Only if it is inlined will it be as
efficient as a call by dereferencing the access-to-subprogram constant
Bar_Callable_Ptr.


.. Hardened Conditionals:

Hardened Conditionals
=====================

GNAT can harden conditionals to protect against control-flow attacks.

This is accomplished by two complementary transformations, each
activated by a separate command-line option.

The option :switch:`-fharden-compares` enables hardening of compares
that compute results stored in variables, adding verification that the
reversed compare yields the opposite result.

The option :switch:`-fharden-conditional-branches` enables hardening
of compares that guard conditional branches, adding verification of
the reversed compare to both execution paths.

These transformations are introduced late in the compilation pipeline,
long after boolean expressions are decomposed into separate compares,
each one turned into either a conditional branch or a compare whose
result is stored in a boolean variable or temporary.  Compiler
optimizations, if enabled, may also turn conditional branches into
stored compares, and vice-versa, or into operations with implied
conditionals (e.g. MIN and MAX).  Conditionals may also be optimized
out entirely, if their value can be determined at compile time, and
occasionally multiple compares can be combined into one.

It is thus difficult to predict which of these two options will affect
a specific compare operation expressed in source code.  Using both
options ensures that every compare that is neither optimized out nor
optimized into implied conditionals will be hardened.

The addition of reversed compares can be observed by enabling the dump
files of the corresponding passes, through command-line options
:switch:`-fdump-tree-hardcmp` and :switch:`-fdump-tree-hardcbr`,
respectively.

They are separate options, however, because of the significantly
different performance impact of the hardening transformations.

For usage and more details on the command-line options, see
:title:`Using the GNU Compiler Collection (GCC)`.  These options can
be used with other programming languages supported by GCC.
