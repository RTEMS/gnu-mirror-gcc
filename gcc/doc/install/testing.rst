..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _testing:

Testing
-------

.. index:: Testing

.. index:: Testing

.. index:: Testsuite

Before you install GCC, we encourage you to run the testsuites and to
compare your results with results from a similar configuration that have
been submitted to the
`gcc-testresults mailing list <http://gcc.gnu.org/ml/gcc-testresults/>`_.
Some of these archived results are linked from the build status lists
at http://gcc.gnu.org/buildstat.html, although not everyone who
reports a successful build runs the testsuites and submits the results.
This step is optional and may require you to download additional software,
but it can give you confidence in your new GCC installation or point out
problems before you install and start using your new GCC.

First, you must have download.htmldownloaded the testsuites.
These are part of the full distribution, but if you downloaded the
'core' compiler plus any front ends, you must download the testsuites
separately.

Second, you must have the testing tools installed.  This includes
`DejaGnu <http://www.gnu.org/software/dejagnu/>`_, Tcl, and Expect;
the DejaGnu site has links to these.
Some optional tests also require Python3 and pytest module.

If the directories where :command:`runtest` and :command:`expect` were
installed are not in the :envvar:`PATH`, you may need to set the following
environment variables appropriately, as in the following example (which
assumes that DejaGnu has been installed under :samp:`/usr/local`):

.. code-block:: bash

  TCL_LIBRARY = /usr/local/share/tcl8.0
  DEJAGNULIBS = /usr/local/share/dejagnu

(On systems such as Cygwin, these paths are required to be actual
paths, not mounts or links; presumably this is due to some lack of
portability in the DejaGnu code.)

Finally, you can run the testsuite (which may take a long time):

.. code-block:: bash

  cd objdir; make -k check

This will test various components of GCC, such as compiler
front ends and runtime libraries.  While running the testsuite, DejaGnu
might emit some harmless messages resembling
:samp:`WARNING: Couldn't find the global config file.` or
:samp:`WARNING: Couldn't find tool init file` that can be ignored.

If you are testing a cross-compiler, you may want to run the testsuite
on a simulator as described at http://gcc.gnu.org/simtest-howto.html.

.. toctree::
  :maxdepth: 2

  how-can-you-run-the-testsuite-on-selected-tests
  passing-options-and-running-multiple-testsuites
  how-to-interpret-test-results
  submitting-test-results