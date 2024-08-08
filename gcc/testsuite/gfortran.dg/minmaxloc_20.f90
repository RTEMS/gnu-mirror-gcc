! { dg-do compile }
! { dg-additional-options "-O -fdump-tree-original" }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?minloc" "original" } }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?maxloc" "original" } }
!
! PR fortran/90608
! Check that all MINLOC and MAXLOC calls are inlined with optimizations,
! when ARRAY is of integral type, DIM is a constant, and MASK is a scalar.

subroutine check_maxloc
  implicit none
  integer, parameter :: data60(*) = (/ 2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1,  &
                                       2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1  /)
  integer, parameter :: data1(*) = (/ 2, 3, 2, 3,  &
                                      1, 2, 3, 2,  &
                                      3, 1, 2, 3,  &
                                      2, 3, 1, 2,  &
                                      3, 2, 3, 1  /)
  integer, parameter :: data2(*) = (/ 2, 1, 2,  &
                                      3, 2, 3,  &
                                      4, 3, 4,  &
                                      2, 1, 2,  &
                                      1, 2, 1  /)
  integer, parameter :: data3(*) = (/ 5, 1, 5,  &
                                      1, 2, 1,  &
                                      2, 1, 2,  &
                                      3, 2, 3  /)
  call check_int_const_shape_rank_3_true_mask
  call check_int_const_shape_rank_3_false_mask
  call check_int_alloc_rank_3_true_mask
  call check_int_alloc_rank_3_false_mask
contains
  subroutine check_int_const_shape_rank_3_true_mask()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    r = maxloc(a, dim = 1, mask = .true.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 21
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 22
    r = maxloc(a, dim = 2, mask = .true.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 23
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 24
    r = maxloc(a, dim = 3, mask = .true.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 25
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 26
  end subroutine
  subroutine check_int_const_shape_rank_3_false_mask()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    r = maxloc(a, dim = 1, mask = .false.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 31
    if (any(r /= 0)) stop 32
    r = maxloc(a, dim = 2, mask = .false.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 33
    if (any(r /= 0)) stop 34
    r = maxloc(a, dim = 3, mask = .false.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 35
    if (any(r /= 0)) stop 36
  end subroutine
  subroutine check_int_alloc_rank_3_true_mask()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    r = maxloc(a, dim = 1, mask = .true.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 81
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 82
    r = maxloc(a, dim = 2, mask = .true.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 83
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 84
    r = maxloc(a, dim = 3, mask = .true.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 85
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 86
  end subroutine
  subroutine check_int_alloc_rank_3_false_mask()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    r = maxloc(a, dim = 1, mask = .false.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 91
    if (any(r /= 0)) stop 92
    r = maxloc(a, dim = 2, mask = .false.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 93
    if (any(r /= 0)) stop 94
    r = maxloc(a, dim = 3, mask = .false.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 95
    if (any(r /= 0)) stop 96
  end subroutine
end subroutine

subroutine check_minloc
  implicit none
  integer, parameter :: data60(*) = (/ 7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8,  &
                                       7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8  /)
  integer, parameter :: data1(*) = (/ 2, 3, 2, 3,  &
                                      1, 2, 3, 2,  &
                                      3, 1, 2, 3,  &
                                      2, 3, 1, 2,  &
                                      3, 2, 3, 1  /)
  integer, parameter :: data2(*) = (/ 2, 1, 2,  &
                                      3, 2, 3,  &
                                      4, 3, 4,  &
                                      2, 1, 2,  &
                                      1, 2, 1  /)
  integer, parameter :: data3(*) = (/ 5, 1, 5,  &
                                      1, 2, 1,  &
                                      2, 1, 2,  &
                                      3, 2, 3  /)
  call check_int_const_shape_rank_3_true_mask
  call check_int_const_shape_rank_3_false_mask
  call check_int_alloc_rank_3_true_mask
  call check_int_alloc_rank_3_false_mask
contains
  subroutine check_int_const_shape_rank_3_true_mask()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    r = minloc(a, dim = 1, mask = .true.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 21
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 22
    r = minloc(a, dim = 2, mask = .true.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 23
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 24
    r = minloc(a, dim = 3, mask = .true.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 25
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 26
  end subroutine
  subroutine check_int_const_shape_rank_3_false_mask()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    r = minloc(a, dim = 1, mask = .false.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 31
    if (any(r /= 0)) stop 32
    r = minloc(a, dim = 2, mask = .false.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 33
    if (any(r /= 0)) stop 34
    r = minloc(a, dim = 3, mask = .false.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 35
    if (any(r /= 0)) stop 36
  end subroutine
  subroutine check_int_alloc_rank_3_true_mask()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    r = minloc(a, dim = 1, mask = .true.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 81
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 82
    r = minloc(a, dim = 2, mask = .true.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 83
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 84
    r = minloc(a, dim = 3, mask = .true.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 85
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 86
  end subroutine
  subroutine check_int_alloc_rank_3_false_mask()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    r = minloc(a, dim = 1, mask = .false.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 91
    if (any(r /= 0)) stop 92
    r = minloc(a, dim = 2, mask = .false.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 93
    if (any(r /= 0)) stop 94
    r = minloc(a, dim = 3, mask = .false.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 95
    if (any(r /= 0)) stop 96
  end subroutine
end subroutine
