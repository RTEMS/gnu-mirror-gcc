..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _image_index:

IMAGE_INDEX --- Function that converts a cosubscript to an image index
**********************************************************************

.. index:: IMAGE_INDEX

.. index:: coarray, IMAGE_INDEX

.. index:: images, cosubscript to image index conversion

.. function:: IMAGE_INDEX

  Returns the image index belonging to a cosubscript.

  :param COARRAY:
    Coarray of any type.

  :param SUB:
    default integer rank-1 array of a size equal to
    the corank of :samp:`{COARRAY}`.

  :return:
    Scalar default integer with the value of the image index which corresponds
    to the cosubscripts. For invalid cosubscripts the result is zero.

  :samp:`{Standard}:`
    Fortran 2008 and later

  :samp:`{Class}:`
    Inquiry function.

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = IMAGE_INDEX(COARRAY, SUB)

  :samp:`{Example}:`

    .. code-block:: fortran

      INTEGER :: array[2,-1:4,8,*]
      ! Writes  28 (or 0 if there are fewer than 28 images)
      WRITE (*,*) IMAGE_INDEX (array, [2,0,3,1])

  :samp:`{See also}:`
    THIS_IMAGE,
    NUM_IMAGES