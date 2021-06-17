..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _num_images:

NUM_IMAGES --- Function that returns the number of images
*********************************************************

.. index:: NUM_IMAGES

.. index:: coarray, NUM_IMAGES

.. index:: images, number of

.. function:: NUM_IMAGES

  Returns the number of images.

  :param DISTANCE:
    (optional, intent(in)) Nonnegative scalar integer

  :param FAILED:
    (optional, intent(in)) Scalar logical expression

  :return:
    Scalar default-kind integer.  If :samp:`{DISTANCE}` is not present or has value 0,
    the number of images in the current team is returned. For values smaller or
    equal distance to the initial team, it returns the number of images index
    on the ancestor team which has a distance of :samp:`{DISTANCE}` from the invoking
    team. If :samp:`{DISTANCE}` is larger than the distance to the initial team, the
    number of images of the initial team is returned. If :samp:`{FAILED}` is not present
    the total number of images is returned; if it has the value ``.TRUE.``,
    the number of failed images is returned, otherwise, the number of images which
    do have not the failed status.

  :samp:`{Standard}:`
    Fortran 2008 and later. With :samp:`{DISTANCE}` or :samp:`{FAILED}` argument,
    Technical Specification (TS) 18508 or later

  :samp:`{Class}:`
    Transformational function

  :samp:`{Syntax}:`

  .. code-block:: fortran

    RESULT = NUM_IMAGES(DISTANCE, FAILED)

  :samp:`{Example}:`

    .. code-block:: fortran

      INTEGER :: value[*]
      INTEGER :: i
      value = THIS_IMAGE()
      SYNC ALL
      IF (THIS_IMAGE() == 1) THEN
        DO i = 1, NUM_IMAGES()
          WRITE(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
        END DO
      END IF

  :samp:`{See also}:`
    THIS_IMAGE,
    IMAGE_INDEX