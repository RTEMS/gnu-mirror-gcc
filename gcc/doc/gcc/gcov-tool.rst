..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _gcov-tool:

gcov-tool---an Offline Gcda Profile Processing Tool
---------------------------------------------------

.. only:: man

  Synopsis
  ^^^^^^^^

  .. code-block:: c++

    gcov-tool [global-options] SUB_COMMAND [sub_command-options] profile_dir

  gcov-tool [ :option:`-v` | :option:`--version` ] [ :option:`-h` | :option:`--help` ]

  gcov-tool merge [merge-options] :samp:`{directory1}` :samp:`{directory2}`
       [ :option:`-o` | :option:`--output` :samp:`{directory}` ]
       [ :option:`-v` | :option:`--verbose` ]
       [ :option:`-w` | :option:`--weight` :samp:`{w1,w2}` ]

  gcov-tool rewrite [rewrite-options] :samp:`{directory}`
       [ :option:`-n` | :option:`--normalize` :samp:`{long_long_value}` ]
       [ :option:`-o` | :option:`--output` :samp:`{directory}` ]
       [ :option:`-s` | :option:`--scale` :samp:`{float_or_simple-frac_value}` ]
       [ :option:`-v` | :option:`--verbose` ]

  gcov-tool overlap [overlap-options] :samp:`{directory1}` :samp:`{directory2}`
       [ :option:`-f` | :option:`--function` ]
       [ :option:`-F` | :option:`--fullname` ]
       [ :option:`-h` | :option:`--hotonly` ]
       [ :option:`-o` | :option:`--object` ]
       [ :option:`-t` | :option:`--hot_threshold` ] :samp:`{float}`
       [ :option:`-v` | :option:`--verbose` ]

Description
^^^^^^^^^^^

:command:`gcov-tool` is an offline tool to process gcc's gcda profile files.

Current gcov-tool supports the following functionalities:

* merge two sets of profiles with weights.

* read one set of profile and rewrite profile contents. One can scale or
  normalize the count values.

Examples of the use cases for this tool are:

* Collect the profiles for different set of inputs, and use this tool to merge
  them. One can specify the weight to factor in the relative importance of
  each input.

* Rewrite the profile after removing a subset of the gcda files, while maintaining
  the consistency of the summary and the histogram.

* It can also be used to debug or libgcov code as the tools shares the majority
  code as the runtime library.

Note that for the merging operation, this profile generated offline may
contain slight different values from the online merged profile. Here are
a list of typical differences:

* histogram difference: This offline tool recomputes the histogram after merging
  the counters. The resulting histogram, therefore, is precise. The online
  merging does not have this capability -- the histogram is merged from two
  histograms and the result is an approximation.

* summary checksum difference: Summary checksum uses a CRC32 operation. The value
  depends on the link list order of gcov-info objects. This order is different in
  gcov-tool from that in the online merge. It's expected to have different
  summary checksums. It does not really matter as the compiler does not use this
  checksum anywhere.

* value profile counter values difference: Some counter values for value profile
  are runtime dependent, like heap addresses. It's normal to see some difference
  in these kind of counters.

Options
^^^^^^^

``-h`` ``--help``
  Display help about using :command:`gcov-tool` (on the standard output), and
  exit without doing any further processing.

``-v`` ``--version``
  Display the :command:`gcov-tool` version number (on the standard output),
  and exit without doing any further processing.

``merge``
  Merge two profile directories.

  :samp:`-o {directory}` :samp:`--output {directory}`
    Set the output profile directory. Default output directory name is
    :samp:`{merged_profile}`.

  ``-v`` ``--verbose``
    Set the verbose mode.

  :samp:`-w {w1},{w2}` :samp:`--weight {w1},{w2}`
    Set the merge weights of the :samp:`{directory1}` and :samp:`{directory2}`,
    respectively. The default weights are 1 for both.

``rewrite``
  Read the specified profile directory and rewrite to a new directory.

  :samp:`-n {long_long_value}` ``--normalize <long_long_value>``
    Normalize the profile. The specified value is the max counter value
    in the new profile.

  :samp:`-o {directory}` :samp:`--output {directory}`
    Set the output profile directory. Default output name is :samp:`{rewrite_profile}`.

  :samp:`-s {float_or_simple-frac_value}` :samp:`--scale {float_or_simple-frac_value}`
    Scale the profile counters. The specified value can be in floating point value,
    or simple fraction value form, such 1, 2, 2/3, and 5/3.

  ``-v`` ``--verbose``
    Set the verbose mode.

``overlap``
  Compute the overlap score between the two specified profile directories.
  The overlap score is computed based on the arc profiles. It is defined as
  the sum of min (p1_counter[i] / p1_sum_all, p2_counter[i] / p2_sum_all),
  for all arc counter i, where p1_counter[i] and p2_counter[i] are two
  matched counters and p1_sum_all and p2_sum_all are the sum of counter
  values in profile 1 and profile 2, respectively.

  ``-f`` ``--function``
    Print function level overlap score.

  ``-F`` ``--fullname``
    Print full gcda filename.

  ``-h`` ``--hotonly``
    Only print info for hot objects/functions.

  ``-o`` ``--object``
    Print object level overlap score.

  :samp:`-t {float}` ``--hot_threshold <float>``
    Set the threshold for hot counter value.

  ``-v`` ``--verbose``
    Set the verbose mode.

.. only:: man

  .. include:: copyright.rst