..
  Copyright 1988-2021 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _runtime-library-routines:

OpenMP Runtime Library Routines
-------------------------------

The runtime routines described here are defined by Section 3 of the OpenMP
specification in version 4.5.  The routines are structured in following
three parts:

Control threads, processors and the parallel environment.  They have C
linkage, and do not throw exceptions.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompgetactivelevel-number-of-parallel-regions
  openmp-runtime-library-routines/ompgetancestorthreadnum-ancestor-thread-id
  openmp-runtime-library-routines/ompgetcancellation-whether-cancellation-support-is-enabled
  openmp-runtime-library-routines/ompgetdefaultdevice-get-the-default-device-for-target-regions
  openmp-runtime-library-routines/ompgetdynamic-dynamic-teams-setting
  openmp-runtime-library-routines/ompgetinitialdevice-return-device-number-of-initial-device
  openmp-runtime-library-routines/ompgetlevel-obtain-the-current-nesting-level
  openmp-runtime-library-routines/ompgetmaxactivelevels-current-maximum-number-of-active-regions
  openmp-runtime-library-routines/ompgetmaxtaskpriority-maximum-priority-value
  openmp-runtime-library-routines/ompgetmaxthreads-maximum-number-of-threads-of-parallel-region
  openmp-runtime-library-routines/ompgetnested-nested-parallel-regions
  openmp-runtime-library-routines/ompgetnumdevices-number-of-target-devices
  openmp-runtime-library-routines/ompgetnumprocs-number-of-processors-online
  openmp-runtime-library-routines/ompgetnumteams-number-of-teams
  openmp-runtime-library-routines/ompgetnumthreads-size-of-the-active-team
  openmp-runtime-library-routines/ompgetprocbind-whether-theads-may-be-moved-between-cpus
  openmp-runtime-library-routines/ompgetschedule-obtain-the-runtime-scheduling-method
  openmp-runtime-library-routines/ompgetsupportedactivelevels-maximum-number-of-active-regions-supported
  openmp-runtime-library-routines/ompgetteamnum-get-team-number
  openmp-runtime-library-routines/ompgetteamsize-number-of-threads-in-a-team
  openmp-runtime-library-routines/ompgetthreadlimit-maximum-number-of-threads
  openmp-runtime-library-routines/ompgetthreadnum-current-thread-id
  openmp-runtime-library-routines/ompinparallel-whether-a-parallel-region-is-active
  openmp-runtime-library-routines/ompinfinal-whether-in-final-or-included-task-region
  openmp-runtime-library-routines/ompisinitialdevice-whether-executing-on-the-host-device
  openmp-runtime-library-routines/ompsetdefaultdevice-set-the-default-device-for-target-regions
  openmp-runtime-library-routines/ompsetdynamic-enable-disable-dynamic-teams
  openmp-runtime-library-routines/ompsetmaxactivelevels-limits-the-number-of-active-parallel-regions
  openmp-runtime-library-routines/ompsetnested-enable-disable-nested-parallel-regions
  openmp-runtime-library-routines/ompsetnumthreads-set-upper-team-size-limit
  openmp-runtime-library-routines/ompsetschedule-set-the-runtime-scheduling-method

Initialize, set, test, unset and destroy simple and nested locks.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompinitlock-initialize-simple-lock
  openmp-runtime-library-routines/ompsetlock-wait-for-and-set-simple-lock
  openmp-runtime-library-routines/omptestlock-test-and-set-simple-lock-if-available
  openmp-runtime-library-routines/ompunsetlock-unset-simple-lock
  openmp-runtime-library-routines/ompdestroylock-destroy-simple-lock
  openmp-runtime-library-routines/ompinitnestlock-initialize-nested-lock
  openmp-runtime-library-routines/ompsetnestlock-wait-for-and-set-nested-lock
  openmp-runtime-library-routines/omptestnestlock-test-and-set-nested-lock-if-available
  openmp-runtime-library-routines/ompunsetnestlock-unset-nested-lock
  openmp-runtime-library-routines/ompdestroynestlock-destroy-nested-lock

Portable, thread-based, wall clock timer.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompgetwtick-get-timer-precision
  openmp-runtime-library-routines/ompgetwtime-elapsed-wall-clock-time

Support for event objects.

.. toctree::
  :maxdepth: 2

  openmp-runtime-library-routines/ompfulfillevent-fulfill-and-destroy-an-openmp-event