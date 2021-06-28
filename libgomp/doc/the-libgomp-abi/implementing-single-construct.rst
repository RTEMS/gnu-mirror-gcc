..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _implementing-single-construct:

Implementing SINGLE construct
*****************************

A block like

.. code-block:: c++

    #pragma omp single
    {
      body;
    }

becomes

.. code-block:: c++

    if (GOMP_single_start ())
      body;
    GOMP_barrier ();

while

.. code-block:: c++

    #pragma omp single copyprivate(x)
      body;

becomes

.. code-block:: c++

    datap = GOMP_single_copy_start ();
    if (datap == NULL)
      {
        body;
        data.x = x;
        GOMP_single_copy_end (&data);
      }
    else
      x = datap->x;
    GOMP_barrier ();