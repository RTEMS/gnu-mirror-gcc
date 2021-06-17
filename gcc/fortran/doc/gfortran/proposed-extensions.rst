..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

  .. _proposed-extensions:

Proposed Extensions
*******************

Here's a list of proposed extensions for the GNU Fortran compiler, in no particular
order.  Most of these are necessary to be fully compatible with
existing Fortran compilers, but they are not part of the official
J3 Fortran 95 standard.

Compiler extensions:
^^^^^^^^^^^^^^^^^^^^

* User-specified alignment rules for structures.

* Automatically extend single precision constants to double.

* Compile code that conserves memory by dynamically allocating common and
  module storage either on stack or heap.

* Compile flag to generate code for array conformance checking (suggest -CC).

* User control of symbol names (underscores, etc).

* Compile setting for maximum size of stack frame size before spilling
  parts to static or heap.

* Flag to force local variables into static space.

* Flag to force local variables onto stack.

Environment Options
^^^^^^^^^^^^^^^^^^^

* Pluggable library modules for random numbers, linear algebra.
  LA should use BLAS calling conventions.

* Environment variables controlling actions on arithmetic exceptions like
  overflow, underflow, precision loss---Generate NaN, abort, default.
  action.

* Set precision for fp units that support it (i387).

* Variable for setting fp rounding mode.

* Variable to fill uninitialized variables with a user-defined bit
  pattern.

* Environment variable controlling filename that is opened for that unit
  number.

* Environment variable to clear/trash memory being freed.

* Environment variable to control tracing of allocations and frees.

* Environment variable to display allocated memory at normal program end.

* Environment variable for filename for \* IO-unit.

* Environment variable for temporary file directory.

* Environment variable forcing standard output to be line buffered (Unix).

.. -
   GNU General Public License
   -