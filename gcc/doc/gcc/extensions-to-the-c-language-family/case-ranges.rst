..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _case-ranges:

Case Ranges
***********

.. index:: case ranges

.. index:: ranges in case statements

You can specify a range of consecutive values in a single ``case`` label,
like this:

.. code-block:: c++

  case low ... high:

This has the same effect as the proper number of individual ``case``
labels, one for each integer value from :samp:`{low}` to :samp:`{high}`, inclusive.

This feature is especially useful for ranges of ASCII character codes:

.. code-block:: c++

  case 'A' ... 'Z':

**Be careful:** Write spaces around the ``...``, for otherwise
it may be parsed wrong when you use it with integer values.  For example,
write this:

.. code-block:: c++

  case 1 ... 5:

rather than this:

.. code-block:: c++

  case 1...5: