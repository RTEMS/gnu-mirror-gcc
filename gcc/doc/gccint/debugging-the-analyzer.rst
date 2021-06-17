..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _debugging-the-analyzer:

Debugging the Analyzer
**********************

.. index:: analyzer, debugging

.. index:: static analyzer, debugging

Special Functions for Debugging the Analyzer
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The analyzer recognizes various special functions by name, for use
in debugging the analyzer.  Declarations can be seen in the testsuite
in :samp:`analyzer-decls.h`.  None of these functions are actually
implemented.

Add:

.. code-block:: c++

    __analyzer_break ();

to the source being analyzed to trigger a breakpoint in the analyzer when
that source is reached.  By putting a series of these in the source, it's
much easier to effectively step through the program state as it's analyzed.

The analyzer handles:

.. code-block:: c++

  __analyzer_describe (0, expr);

by emitting a warning describing the 2nd argument (which can be of any
type), at a verbosity level given by the 1st argument.  This is for use when
debugging, and may be of use in DejaGnu tests.

.. code-block:: c++

  __analyzer_dump ();

will dump the copious information about the analyzer's state each time it
reaches the call in its traversal of the source.

.. code-block:: c++

  extern void __analyzer_dump_capacity (const void *ptr);

will emit a warning describing the capacity of the base region of
the region pointed to by the 1st argument.

.. code-block:: c++

  __analyzer_dump_path ();

will emit a placeholder 'note' diagnostic with a path to that call site,
if the analyzer finds a feasible path to it.

The builtin ``__analyzer_dump_exploded_nodes`` will emit a warning
after analysis containing information on all of the exploded nodes at that
program point:

.. code-block:: c++

    __analyzer_dump_exploded_nodes (0);

will output the number of 'processed' nodes, and the IDs of
both 'processed' and 'merger' nodes, such as:

.. code-block:: c++

  warning: 2 processed enodes: [EN: 56, EN: 58] merger(s): [EN: 54-55, EN: 57, EN: 59]

With a non-zero argument

.. code-block:: c++

    __analyzer_dump_exploded_nodes (1);

it will also dump all of the states within the 'processed' nodes.

.. code-block:: c++

     __analyzer_dump_region_model ();

will dump the region_model's state to stderr.

.. code-block:: c++

  __analyzer_eval (expr);

will emit a warning with text "TRUE", FALSE" or "UNKNOWN" based on the
truthfulness of the argument.  This is useful for writing DejaGnu tests.

Other Debugging Techniques
^^^^^^^^^^^^^^^^^^^^^^^^^^

The option :option:`-fdump-analyzer-json` will dump both the supergraph
and the exploded graph in compressed JSON form.

One approach when tracking down where a particular bogus state is
introduced into the ``exploded_graph`` is to add custom code to
``program_state::validate``.

.. Free Software Foundation, Inc.