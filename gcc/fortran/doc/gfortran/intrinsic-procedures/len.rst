..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _len:

LEN --- Length of a character entity
************************************

.. index:: LEN

.. index:: string, length

.. function:: LEN

  Returns the length of a character string.  If :samp:`{STRING}` is an array,
  the length of an element of :samp:`{STRING}` is returned.  Note that
  :samp:`{STRING}` need not be defined when this intrinsic is invoked, since
  only the length, not the content, of :samp:`{STRING}` is needed.

  :param STRING:
    Shall be a scalar or array of type
    ``CHARACTER``, with ``INTENT(IN)``

  :param KIND:
    (Optional) An ``INTEGER`` initialization
    expression indicating the kind parameter of the result.

  :return:
    The return value is of type ``INTEGER`` and of kind :samp:`{KIND}`. If
    :samp:`{KIND}` is absent, the return value is of default integer kind.

  :samp:`{Standard}:`
    Fortran 77 and later, with :samp:`{KIND}` argument Fortran 2003 and later

  :samp:`{Class}:`
    Inquiry function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    L = LEN(STRING [, KIND])

  :samp:`{Specific names}:`
    ===============  =============  ===========  ====================
    Name             Argument       Return type  Standard
    ===============  =============  ===========  ====================
    ``LEN(STRING)``  ``CHARACTER``  ``INTEGER``  Fortran 77 and later
    ===============  =============  ===========  ====================

  :samp:`{See also}:`
    LEN_TRIM,
    ADJUSTL,
    ADJUSTR