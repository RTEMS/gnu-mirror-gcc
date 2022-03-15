..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the GPL license file

.. _openmp-5.1:

OpenMP 5.1
**********

New features listed in Appendix B of the OpenMP specification
*************************************************************

========================================================================  ======  =============================
Description                                                               Status  Comments
========================================================================  ======  =============================
OpenMP directive as C++ attribute specifiers                              Y
``omp_all_memory`` reserved locator                                       N
*target_device trait* in OpenMP Context                                   N
``target_device`` selector set in context selectors                       N
C/C++'s ``declare variant`` directive: elision support of                 N
      preprocessed code
``declare variant`` : new clauses ``adjust_args`` and                     N
      ``append_args``
``dispatch`` construct                                                    N
device-specific ICV settings the environment variables                    N
assume directive                                                          N
``nothing`` directive                                                     Y
``error`` directive                                                       Y
``masked`` construct                                                      Y
``scope`` directive                                                       Y
Loop transformation constructs                                            N
``strict`` modifier in the ``grainsize`` and ``num_tasks``                Y
      clauses of the taskloop construct
``align`` clause/modifier in ``allocate`` directive/clause                P       C/C++ on clause only
      and ``allocator`` directive
``thread_limit`` clause to ``target`` construct                           Y
``has_device_addr`` clause to ``target`` construct                        Y
iterators in ``target update`` motion clauses and ``map``                 N
      clauses
indirect calls to the device version of a procedure or function in        N
      ``target`` regions
``interop`` directive                                                     N
``omp_interop_t`` object support in runtime routines                      N
``nowait`` clause in ``taskwait`` directive                               N
Extensions to the ``atomic`` directive                                    Y
``seq_cst`` clause on a ``flush`` construct                               Y
``inoutset`` argument to the ``depend`` clause                            N
``private`` and ``firstprivate`` argument to ``default``                  Y
      clause in C and C++
``present`` argument to ``defaultmap`` clause                             N
``omp_set_num_teams``, ``omp_set_teams_thread_limit``,                    Y
      ``omp_get_max_teams``, ``omp_get_teams_thread_limit`` runtime
      routines
``omp_target_is_accessible`` runtime routine                              N
``omp_target_memcpy_async`` and ``omp_target_memcpy_rect_async``          N
      runtime routines
``omp_get_mapped_ptr`` runtime routine                                    N
``omp_calloc``, ``omp_realloc``, ``omp_aligned_alloc`` and                Y
      ``omp_aligned_calloc`` runtime routines
``omp_alloctrait_key_t`` enum: ``omp_atv_serialized`` added,              Y
      ``omp_atv_default`` changed
``omp_display_env`` runtime routine                                       Y       Not inside ``target`` regions
``ompt_scope_endpoint_t`` enum: ``ompt_scope_beginend``                   N
``ompt_sync_region_t`` enum additions                                     N
``ompt_state_t`` enum: ``ompt_state_wait_barrier_implementation``         N
      and ``ompt_state_wait_barrier_teams``
``ompt_callback_target_data_op_emi_t``,                                   N
      ``ompt_callback_target_emi_t``, ``ompt_callback_target_map_emi_t``
      and ``ompt_callback_target_submit_emi_t``
``ompt_callback_error_t`` type                                            N
``OMP_PLACES`` syntax extensions                                          Y
``OMP_NUM_TEAMS`` and ``OMP_TEAMS_THREAD_LIMIT`` environment              Y
      variables
========================================================================  ======  =============================

Other new OpenMP 5.1 features
*****************************

=============================================================  ======  ========
Description                                                    Status  Comments
=============================================================  ======  ========
Support of strictly structured blocks in Fortran               Y
Support of structured block sequences in C/C++                 Y
``unconstrained`` and ``reproducible`` modifiers on ``order``  Y
      clause
=============================================================  ======  ========