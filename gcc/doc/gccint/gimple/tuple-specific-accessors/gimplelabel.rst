..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

GIMPLE_LABEL
^^^^^^^^^^^^

.. index:: GIMPLE_LABEL

.. function:: glabel *gimple_build_label (tree label)

  Build a ``GIMPLE_LABEL`` statement with corresponding to the tree
  label, ``LABEL``.

.. function:: tree gimple_label_label (const glabel *g)

  Return the ``LABEL_DECL`` node used by ``GIMPLE_LABEL`` statement ``G``.

.. function:: void gimple_label_set_label (glabel *g, tree label)

  Set ``LABEL`` to be the ``LABEL_DECL`` node used by ``GIMPLE_LABEL``
  statement ``G``.