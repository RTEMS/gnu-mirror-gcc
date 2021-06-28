..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _asynchronous-i-o:

Asynchronous I/O
****************

.. index:: input/output, asynchronous

.. index:: asynchronous I/O

Asynchronous I/O is supported if the program is linked against the
POSIX thread library. If that is not the case, all I/O is performed
as synchronous. On systems which do not support pthread condition
variables, such as AIX, I/O is also performed as synchronous.

On some systems, such as Darwin or Solaris, the POSIX thread library
is always linked in, so asynchronous I/O is always performed. On other
sytems, such as Linux, it is necessary to specify :option:`-pthread`,
:option:`-lpthread` or :option:`-fopenmp` during the linking step.

.. -
   Extensions
   -
   Maybe this chapter should be merged with the 'Standards' section,
   whenever that is written :-)