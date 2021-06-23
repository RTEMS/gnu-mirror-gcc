..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _static-analyzer-options:

Options That Control Static Analysis
************************************

.. option:: -fanalyzer

  This option enables an static analysis of program flow which looks
  for 'interesting' interprocedural paths through the
  code, and issues warnings for problems found on them.

  This analysis is much more expensive than other GCC warnings.

  Enabling this option effectively enables the following warnings:

  :option:`-Wanalyzer-double-fclose`
  :option:`-Wanalyzer-double-free`
  :option:`-Wanalyzer-exposure-through-output-file`
  :option:`-Wanalyzer-file-leak`
  :option:`-Wanalyzer-free-of-non-heap`
  :option:`-Wanalyzer-malloc-leak`
  :option:`-Wanalyzer-mismatching-deallocation`
  :option:`-Wanalyzer-possible-null-argument`
  :option:`-Wanalyzer-possible-null-dereference`
  :option:`-Wanalyzer-null-argument`
  :option:`-Wanalyzer-null-dereference`
  :option:`-Wanalyzer-shift-count-negative`
  :option:`-Wanalyzer-shift-count-overflow`
  :option:`-Wanalyzer-stale-setjmp-buffer`
  :option:`-Wanalyzer-tainted-array-index`
  :option:`-Wanalyzer-unsafe-call-within-signal-handler`
  :option:`-Wanalyzer-use-after-free`
  :option:`-Wanalyzer-use-of-pointer-in-stale-stack-frame`
  :option:`-Wanalyzer-write-to-const`
  :option:`-Wanalyzer-write-to-string-literal`

  This option is only available if GCC was configured with analyzer
  support enabled.

.. option:: -fno-analyzer

  Default option value for :option:`-fanalyzer`.

.. option:: -Wanalyzer-too-complex

  If :option:`-fanalyzer` is enabled, the analyzer uses various heuristics
  to attempt to explore the control flow and data flow in the program,
  but these can be defeated by sufficiently complicated code.

  By default, the analysis silently stops if the code is too
  complicated for the analyzer to fully explore and it reaches an internal
  limit.  The :option:`-Wanalyzer-too-complex` option warns if this occurs.

.. option:: -Wno-analyzer-too-complex

  Default option value for :option:`-Wanalyzer-too-complex`.

.. option:: -Wno-analyzer-double-fclose

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-double-fclose` to disable it.

  This diagnostic warns for paths through the code in which a ``FILE *``
  can have ``fclose`` called on it more than once.

.. option:: -Wanalyzer-double-fclose

  Default option value for :option:`-Wno-analyzer-double-fclose`.

.. option:: -Wno-analyzer-double-free

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-double-free` to disable it.

  This diagnostic warns for paths through the code in which a pointer
  can have a deallocator called on it more than once, either ``free``,
  or a deallocator referenced by attribute ``malloc``.

.. option:: -Wanalyzer-double-free

  Default option value for :option:`-Wno-analyzer-double-free`.

.. option:: -Wno-analyzer-exposure-through-output-file

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-exposure-through-output-file`
  to disable it.

  This diagnostic warns for paths through the code in which a
  security-sensitive value is written to an output file
  (such as writing a password to a log file).

.. option:: -Wanalyzer-exposure-through-output-file

  Default option value for :option:`-Wno-analyzer-exposure-through-output-file`.

.. option:: -Wno-analyzer-file-leak

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-file-leak`
  to disable it.

  This diagnostic warns for paths through the code in which a
  ``<stdio.h>`` ``FILE *`` stream object is leaked.

.. option:: -Wanalyzer-file-leak

  Default option value for :option:`-Wno-analyzer-file-leak`.

.. option:: -Wno-analyzer-free-of-non-heap

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-free-of-non-heap`
  to disable it.

  This diagnostic warns for paths through the code in which ``free``
  is called on a non-heap pointer (e.g. an on-stack buffer, or a global).

.. option:: -Wanalyzer-free-of-non-heap

  Default option value for :option:`-Wno-analyzer-free-of-non-heap`.

.. option:: -Wno-analyzer-malloc-leak

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-malloc-leak`
  to disable it.

  This diagnostic warns for paths through the code in which a
  pointer allocated via an allocator is leaked: either ``malloc``,
  or a function marked with attribute ``malloc``.

.. option:: -Wanalyzer-malloc-leak

  Default option value for :option:`-Wno-analyzer-malloc-leak`.

.. option:: -Wno-analyzer-mismatching-deallocation

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-mismatching-deallocation`
  to disable it.

  This diagnostic warns for paths through the code in which the
  wrong deallocation function is called on a pointer value, based on
  which function was used to allocate the pointer value.  The diagnostic
  will warn about mismatches between ``free``, scalar ``delete``
  and vector ``delete[]``, and those marked as allocator/deallocator
  pairs using attribute ``malloc``.

.. option:: -Wanalyzer-mismatching-deallocation

  Default option value for :option:`-Wno-analyzer-mismatching-deallocation`.

.. option:: -Wno-analyzer-possible-null-argument

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-possible-null-argument` to disable it.

  This diagnostic warns for paths through the code in which a
  possibly-NULL value is passed to a function argument marked
  with ``__attribute__((nonnull))`` as requiring a non-NULL
  value.

.. option:: -Wanalyzer-possible-null-argument

  Default option value for :option:`-Wno-analyzer-possible-null-argument`.

.. option:: -Wno-analyzer-possible-null-dereference

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-possible-null-dereference` to disable it.

  This diagnostic warns for paths through the code in which a
  possibly-NULL value is dereferenced.

.. option:: -Wanalyzer-possible-null-dereference

  Default option value for :option:`-Wno-analyzer-possible-null-dereference`.

.. option:: -Wno-analyzer-null-argument

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-null-argument` to disable it.

  This diagnostic warns for paths through the code in which a
  value known to be NULL is passed to a function argument marked
  with ``__attribute__((nonnull))`` as requiring a non-NULL
  value.

.. option:: -Wanalyzer-null-argument

  Default option value for :option:`-Wno-analyzer-null-argument`.

.. option:: -Wno-analyzer-null-dereference

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-null-dereference` to disable it.

  This diagnostic warns for paths through the code in which a
  value known to be NULL is dereferenced.

.. option:: -Wanalyzer-null-dereference

  Default option value for :option:`-Wno-analyzer-null-dereference`.

.. option:: -Wno-analyzer-shift-count-negative

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-shift-count-negative` to disable it.

  This diagnostic warns for paths through the code in which a
  shift is attempted with a negative count.  It is analogous to
  the :option:`-Wshift-count-negative` diagnostic implemented in
  the C/C++ front ends, but is implemented based on analyzing
  interprocedural paths, rather than merely parsing the syntax tree.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-shift-count-negative

  Default option value for :option:`-Wno-analyzer-shift-count-negative`.

.. option:: -Wno-analyzer-shift-count-overflow

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-shift-count-overflow` to disable it.

  This diagnostic warns for paths through the code in which a
  shift is attempted with a count greater than or equal to the
  precision of the operand's type.  It is analogous to
  the :option:`-Wshift-count-overflow` diagnostic implemented in
  the C/C++ front ends, but is implemented based on analyzing
  interprocedural paths, rather than merely parsing the syntax tree.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-shift-count-overflow

  Default option value for :option:`-Wno-analyzer-shift-count-overflow`.

.. option:: -Wno-analyzer-stale-setjmp-buffer

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-stale-setjmp-buffer` to disable it.

  This diagnostic warns for paths through the code in which
  ``longjmp`` is called to rewind to a ``jmp_buf`` relating
  to a ``setjmp`` call in a function that has returned.

  When ``setjmp`` is called on a ``jmp_buf`` to record a rewind
  location, it records the stack frame.  The stack frame becomes invalid
  when the function containing the ``setjmp`` call returns.  Attempting
  to rewind to it via ``longjmp`` would reference a stack frame that
  no longer exists, and likely lead to a crash (or worse).

.. option:: -Wanalyzer-stale-setjmp-buffer

  Default option value for :option:`-Wno-analyzer-stale-setjmp-buffer`.

.. option:: -Wno-analyzer-tainted-array-index

  This warning requires both :option:`-fanalyzer` and
  :option:`-fanalyzer-checker`:samp:`=taint` to enable it;
  use :option:`-Wno-analyzer-tainted-array-index` to disable it.

  This diagnostic warns for paths through the code in which a value
  that could be under an attacker's control is used as the index
  of an array access without being sanitized.

.. option:: -Wanalyzer-tainted-array-index

  Default option value for :option:`-Wno-analyzer-tainted-array-index`.

.. option:: -Wno-analyzer-unsafe-call-within-signal-handler

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-unsafe-call-within-signal-handler` to disable it.

  This diagnostic warns for paths through the code in which a
  function known to be async-signal-unsafe (such as ``fprintf`` ) is
  called from a signal handler.

.. option:: -Wanalyzer-unsafe-call-within-signal-handler

  Default option value for :option:`-Wno-analyzer-unsafe-call-within-signal-handler`.

.. option:: -Wno-analyzer-use-after-free

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-use-after-free` to disable it.

  This diagnostic warns for paths through the code in which a
  pointer is used after a deallocator is called on it: either ``free``,
  or a deallocator referenced by attribute ``malloc``.

.. option:: -Wanalyzer-use-after-free

  Default option value for :option:`-Wno-analyzer-use-after-free`.

.. option:: -Wno-analyzer-use-of-pointer-in-stale-stack-frame

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-use-of-pointer-in-stale-stack-frame`
  to disable it.

  This diagnostic warns for paths through the code in which a pointer
  is dereferenced that points to a variable in a stale stack frame.

.. option:: -Wanalyzer-use-of-pointer-in-stale-stack-frame

  Default option value for :option:`-Wno-analyzer-use-of-pointer-in-stale-stack-frame`.

.. option:: -Wno-analyzer-write-to-const

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-write-to-const`
  to disable it.

  This diagnostic warns for paths through the code in which the analyzer
  detects an attempt to write through a pointer to a ``const`` object.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-write-to-const

  Default option value for :option:`-Wno-analyzer-write-to-const`.

.. option:: -Wno-analyzer-write-to-string-literal

  This warning requires :option:`-fanalyzer`, which enables it; use
  :option:`-Wno-analyzer-write-to-string-literal`
  to disable it.

  This diagnostic warns for paths through the code in which the analyzer
  detects an attempt to write through a pointer to a string literal.
  However, the analyzer does not prioritize detection of such paths, so
  false negatives are more likely relative to other warnings.

.. option:: -Wanalyzer-write-to-string-literal

  Default option value for :option:`-Wno-analyzer-write-to-string-literal`.

Pertinent parameters for controlling the exploration are:
:option:`--param analyzer-bb-explosion-factor`:samp:`={value}`,
:option:`--param analyzer-max-enodes-per-program-point`:samp:`={value}`,
:option:`--param analyzer-max-recursion-depth`:samp:`={value}`, and
:option:`--param analyzer-min-snodes-for-call-summary`:samp:`={value}`.

The following options control the analyzer.

.. option:: -fanalyzer-call-summaries

  Simplify interprocedural analysis by computing the effect of certain calls,
  rather than exploring all paths through the function from callsite to each
  possible return.

  If enabled, call summaries are only used for functions with more than one
  call site, and that are sufficiently complicated (as per
  :option:`--param analyzer-min-snodes-for-call-summary`:samp:`={value}` ).

.. option:: -fno-analyzer-call-summaries

  Default option value for :option:`-fanalyzer-call-summaries`.

.. option:: -fanalyzer-checker=name

  Restrict the analyzer to run just the named checker, and enable it.

  Some checkers are disabled by default (even with :option:`-fanalyzer` ),
  such as the ``taint`` checker that implements
  :option:`-Wanalyzer-tainted-array-index`, and this option is required
  to enable them.

.. option:: -fno-analyzer-feasibility

  This option is intended for analyzer developers.

  By default the analyzer verifies that there is a feasible control flow path
  for each diagnostic it emits: that the conditions that hold are not mutually
  exclusive.  Diagnostics for which no feasible path can be found are rejected.
  This filtering can be suppressed with :option:`-fno-analyzer-feasibility`, for
  debugging issues in this code.

.. option:: -fanalyzer-feasibility

  Default option value for :option:`-fno-analyzer-feasibility`.

.. option:: -fanalyzer-fine-grained

  This option is intended for analyzer developers.

  Internally the analyzer builds an 'exploded graph' that combines
  control flow graphs with data flow information.

  By default, an edge in this graph can contain the effects of a run
  of multiple statements within a basic block.  With
  :option:`-fanalyzer-fine-grained`, each statement gets its own edge.

.. option:: -fno-analyzer-fine-grained

  Default option value for :option:`-fanalyzer-fine-grained`.

.. option:: -fanalyzer-show-duplicate-count

  This option is intended for analyzer developers: if multiple diagnostics
  have been detected as being duplicates of each other, it emits a note when
  reporting the best diagnostic, giving the number of additional diagnostics
  that were suppressed by the deduplication logic.

.. option:: -fno-analyzer-show-duplicate-count

  Default option value for :option:`-fanalyzer-show-duplicate-count`.

.. option:: -fno-analyzer-state-merge

  This option is intended for analyzer developers.

  By default the analyzer attempts to simplify analysis by merging
  sufficiently similar states at each program point as it builds its
  'exploded graph'.  With :option:`-fno-analyzer-state-merge` this
  merging can be suppressed, for debugging state-handling issues.

.. option:: -fanalyzer-state-merge

  Default option value for :option:`-fno-analyzer-state-merge`.

.. option:: -fno-analyzer-state-purge

  This option is intended for analyzer developers.

  By default the analyzer attempts to simplify analysis by purging
  aspects of state at a program point that appear to no longer be relevant
  e.g. the values of locals that aren't accessed later in the function
  and which aren't relevant to leak analysis.

  With :option:`-fno-analyzer-state-purge` this purging of state can
  be suppressed, for debugging state-handling issues.

.. option:: -fanalyzer-state-purge

  Default option value for :option:`-fno-analyzer-state-purge`.

.. option:: -fanalyzer-transitivity

  This option enables transitivity of constraints within the analyzer.

.. option:: -fno-analyzer-transitivity

  Default option value for :option:`-fanalyzer-transitivity`.

``-fanalyzer-verbose-edges``
  This option is intended for analyzer developers.  It enables more
  verbose, lower-level detail in the descriptions of control flow
  within diagnostic paths.

``-fanalyzer-verbose-state-changes``
  This option is intended for analyzer developers.  It enables more
  verbose, lower-level detail in the descriptions of events relating
  to state machines within diagnostic paths.

:samp:`-fanalyzer-verbosity={level}`
  This option controls the complexity of the control flow paths that are
  emitted for analyzer diagnostics.

  The :samp:`{level}` can be one of:

  :samp:`0`
    At this level, interprocedural call and return events are displayed,
    along with the most pertinent state-change events relating to
    a diagnostic.  For example, for a double- ``free`` diagnostic,
    both calls to ``free`` will be shown.

  :samp:`1`
    As per the previous level, but also show events for the entry
    to each function.

  :samp:`2`
    As per the previous level, but also show events relating to
    control flow that are significant to triggering the issue
    (e.g. 'true path taken' at a conditional).

    This level is the default.

  :samp:`3`
    As per the previous level, but show all control flow events, not
    just significant ones.

  :samp:`4`
    This level is intended for analyzer developers; it adds various
    other events intended for debugging the analyzer.

.. option:: -fdump-analyzer

  Dump internal details about what the analyzer is doing to
  :samp:`{file}.analyzer.txt`.
  This option is overridden by :option:`-fdump-analyzer-stderr`.

.. option:: -fdump-analyzer-stderr

  Dump internal details about what the analyzer is doing to stderr.
  This option overrides :option:`-fdump-analyzer`.

.. option:: -fdump-analyzer-callgraph

  Dump a representation of the call graph suitable for viewing with
  GraphViz to :samp:`{file}.callgraph.dot`.

.. option:: -fdump-analyzer-exploded-graph

  Dump a representation of the 'exploded graph' suitable for viewing with
  GraphViz to :samp:`{file}.eg.dot`.
  Nodes are color-coded based on state-machine states to emphasize
  state changes.

.. option:: -fdump-analyzer-exploded-nodes

  Emit diagnostics showing where nodes in the 'exploded graph' are
  in relation to the program source.

.. option:: -fdump-analyzer-exploded-nodes-2

  Dump a textual representation of the 'exploded graph' to
  :samp:`{file}.eg.txt`.

.. option:: -fdump-analyzer-exploded-nodes-3

  Dump a textual representation of the 'exploded graph' to
  one dump file per node, to :samp:`{file}.eg-{id}.txt`.
  This is typically a large number of dump files.

.. option:: -fdump-analyzer-feasibility

  Dump internal details about the analyzer's search for feasible paths.
  The details are written in a form suitable for viewing with GraphViz
  to filenames of the form :samp:`{file}.*.fg.dot` and
  :samp:`{file}.*.tg.dot`.

.. option:: -fdump-analyzer-json

  Dump a compressed JSON representation of analyzer internals to
  :samp:`{file}.analyzer.json.gz`.  The precise format is subject
  to change.

.. option:: -fdump-analyzer-state-purge

  As per :option:`-fdump-analyzer-supergraph`, dump a representation of the
  'supergraph' suitable for viewing with GraphViz, but annotate the
  graph with information on what state will be purged at each node.
  The graph is written to :samp:`{file}.state-purge.dot`.

.. option:: -fdump-analyzer-supergraph

  Dump representations of the 'supergraph' suitable for viewing with
  GraphViz to :samp:`{file}.supergraph.dot` and to
  :samp:`{file}.supergraph-eg.dot`.  These show all of the
  control flow graphs in the program, with interprocedural edges for
  calls and returns.  The second dump contains annotations showing nodes
  in the 'exploded graph' and diagnostics associated with them.