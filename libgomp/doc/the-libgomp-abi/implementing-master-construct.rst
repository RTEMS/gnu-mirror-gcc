..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _implementing-master-construct:

Implementing MASTER construct
*****************************

.. code-block:: c++

  if (omp_get_thread_num () == 0)
    block

Alternately, we generate two copies of the parallel subfunction
and only include this in the version run by the master thread.
Surely this is not worthwhile though...