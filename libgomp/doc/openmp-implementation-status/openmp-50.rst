..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _openmp-5.0:

OpenMP 5.0
**********

New features listed in Appendix B of the OpenMP specification
*************************************************************

.. This list is sorted as in OpenMP 5.1's B.3 not as in OpenMP 5.0's B.2

=======================================================================  ======  ===================================
Description                                                              Status  Comments
=======================================================================  ======  ===================================
Array shaping                                                            N
Array sections with non-unit strides in C and C++                        N
Iterators                                                                Y
``metadirective`` directive                                              N
``declare variant`` directive                                            P       simd traits not handled correctly
*target-offload-var* ICV and ``OMP_TARGET_OFFLOAD``                      Y
      env variable
Nested-parallel changes to *max-active-levels-var* ICV                   Y
``requires`` directive                                                   P       See note [#f1]_.
``teams`` construct outside an enclosing target region                   Y
Non-rectangular loop nests                                               P       Only C/C++
``!=`` as relational-op in canonical loop form for C/C++                 Y
``nonmonotonic`` as default loop schedule modifier for worksharing-loop  Y
      constructs
Collapse of associated loops that are imperfectly nested loops           N
Clauses ``if``, ``nontemporal`` and ``order(concurrent)`` in             Y
      ``simd`` construct
``atomic`` constructs in ``simd``                                        Y
``loop`` construct                                                       Y
``order(concurrent)`` clause                                             Y
``scan`` directive and ``in_scan`` modifier for the                      Y
      ``reduction`` clause
``in_reduction`` clause on ``task`` constructs                           Y
``in_reduction`` clause on ``target`` constructs                         P       ``nowait`` only stub
``task_reduction`` clause with ``taskgroup``                             Y
``task`` modifier to ``reduction`` clause                                Y
``affinity`` clause to ``task`` construct                                Y       Stub only
``detach`` clause to ``task`` construct                                  Y
``omp_fulfill_event`` runtime routine                                    Y
``reduction`` and ``in_reduction`` clauses on ``taskloop``               Y
      and ``taskloop simd`` constructs
``taskloop`` construct cancelable by ``cancel`` construct                Y
``mutexinouset`` *dependence-type* for ``depend`` clause                 Y
Predefined memory spaces, memory allocators, allocator traits            Y       Some are only stubs
Memory management routines                                               Y
``allocate`` directive                                                   N
``allocate`` clause                                                      P       initial support
``use_device_addr`` clause on ``target data``                            Y
``ancestor`` modifier on ``device`` clause                               P       Reverse offload unsupported
Implicit declare target directive                                        Y
Discontiguous array section with ``target update`` construct             N
C/C++'s lvalue expressions in ``to``, ``from``                           N
      and ``map`` clauses
C/C++'s lvalue expressions in ``depend`` clauses                         Y
Nested ``declare target`` directive                                      Y
Combined ``master`` constructs                                           Y
``depend`` clause on ``taskwait``                                        Y
Weak memory ordering clauses on ``atomic`` and ``flush`` construct       Y
``hint`` clause on the ``atomic`` construct                              Y       Stub only
``depobj`` construct and depend objects                                  Y
Lock hints were renamed to synchronization hints                         Y
``conditional`` modifier to ``lastprivate`` clause                       Y
Map-order clarifications                                                 P
``close`` *map-type-modifier*                                            Y
Mapping C/C++ pointer variables and to assign the address of             P
      device memory mapped by an array section
Mapping of Fortran pointer and allocatable variables, including pointer  P       See note [#f2]_.
      and allocatable components of variables
``defaultmap`` extensions                                                Y
``declare mapper`` directive                                             N
``omp_get_supported_active_levels`` routine                              Y
Runtime routines and environment variables to display runtime thread     Y
      affinity information
``omp_pause_resource`` and ``omp_pause_resource_all`` runtime            Y
      routines
``omp_get_device_num`` runtime routine                                   Y
OMPT interface                                                           N
OMPD interface                                                           N
=======================================================================  ======  ===================================

.. [#f1] Only fulfillable requirement are ``atomic_default_mem_order`` and ``dynamic_allocators``
.. [#f2] Mapping of vars with allocatable components unsupported

Other new OpenMP 5.0 features
*****************************

=====================================  ======  ========
Description                            Status  Comments
=====================================  ======  ========
Supporting C++'s range-based for loop  Y
=====================================  ======  ========