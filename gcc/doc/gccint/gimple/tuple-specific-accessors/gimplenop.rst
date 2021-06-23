..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

GIMPLE_NOP
^^^^^^^^^^

.. index:: GIMPLE_NOP

.. function:: gimple gimple_build_nop (void)

  Build a ``GIMPLE_NOP`` statement.

.. function:: bool gimple_nop_p (gimple g)

  Returns ``TRUE`` if statement ``G`` is a ``GIMPLE_NOP``.