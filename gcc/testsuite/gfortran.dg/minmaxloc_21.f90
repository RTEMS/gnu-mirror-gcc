! { dg-do compile }
! { dg-additional-options "-O -fdump-tree-original" }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?minloc" "original" } }
! { dg-final { scan-tree-dump-not "gfortran_\[sm\]?maxloc" "original" } }
!
! PR fortran/90608
! Check that all MINLOC and MAXLOC calls are inlined with optimizations,
! when DIM is a constant, and either ARRAY has REAL type or MASK is non-scalar.

subroutine check_real_maxloc
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
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
contains
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 51
    if (any(r /= reshape((/ real:: data1 /), (/ 4, 5 /)))) stop 52
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 53
    if (any(r /= reshape((/ real:: data2 /), (/ 3, 5 /)))) stop 54
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 55
    if (any(r /= reshape((/ real:: data3 /), (/ 3, 4 /)))) stop 56
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 61
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 62
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 63
    if (any(r /= 0)) stop 64
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 65
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 71
    if (any(r /= reshape((/ real:: data1 /), shape=(/ 4, 5 /)))) stop 72
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 73
    if (any(r /= reshape((/ real:: data2 /), shape=(/ 3, 5 /)))) stop 74
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 75
    if (any(r /= reshape((/ real:: data3 /), shape=(/ 3, 4 /)))) stop 76
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 81
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 82
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 83
    if (any(r /= 0)) stop 84
    r = maxloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 85
  end subroutine
end subroutine

subroutine check_maxloc_with_mask
  implicit none
  integer, parameter :: data60(*) = (/ 2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1,  &
                                       2, 5, 4, 6, 0, 9, 3, 5, 4, 4,  &
                                       1, 7, 3, 2, 1, 2, 5, 4, 6, 0,  &
                                       9, 3, 5, 4, 4, 1, 7, 3, 2, 1  /)
  logical, parameter :: mask60(*) = (/ .true. , .false., .false., .false., &
                                       .true. , .false., .true. , .false., &
                                       .false., .true. , .true. , .false., &
                                       .true. , .true. , .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .true. , &
                                       .true. , .false., .false., .true. , &
                                       .true. , .true. , .true. , .false., &
                                       .false., .false., .true. , .false., &
                                       .true. , .false., .true. , .true. , &
                                       .true. , .false., .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .false., &
                                       .false., .true. , .true. , .true. , &
                                       .false., .true. , .false., .true.  /)
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
  integer, parameter :: data1m(*) = (/ 1, 2, 1, 1,  &
                                       1, 3, 2, 3,  &
                                       1, 1, 1, 2,  &
                                       3, 1, 1, 3,  &
                                       2, 3, 1, 1  /)
  integer, parameter :: data2m(*) = (/ 4, 4, 0,  &
                                       1, 1, 2,  &
                                       1, 2, 2,  &
                                       2, 3, 1,  &
                                       3, 3, 2  /)
  integer, parameter :: data3m(*) = (/ 3, 2, 4,  &
                                       4, 3, 2,  &
                                       5, 4, 0,  &
                                       1, 1, 2  /)
  call check_int_const_shape_rank_3
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
contains
  subroutine check_int_const_shape_rank_3()
    integer :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    m = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 11
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 12
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 13
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 14
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 15
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 16
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 61
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 62
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 63
    if (any(r /= 0)) stop 64
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 65
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 71
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 72
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 73
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 74
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 75
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 76
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical:: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 101
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 102
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 103
    if (any(r /= 0)) stop 104
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 105
  end subroutine
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 111
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 112
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 113
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 114
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 115
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 116
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 161
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 162
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 163
    if (any(r /= 0)) stop 164
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 165
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 171
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 172
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 173
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 174
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 175
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 176
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical :: /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 201
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 202
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 203
    if (any(r /= 0)) stop 204
    r = maxloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 205
  end subroutine
end subroutine

subroutine check_real_minloc
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
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
contains
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 51
    if (any(r /= reshape((/ real:: data1 /), (/ 4, 5 /)))) stop 52
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 53
    if (any(r /= reshape((/ real:: data2 /), (/ 3, 5 /)))) stop 54
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 55
    if (any(r /= reshape((/ real:: data3 /), (/ 3, 4 /)))) stop 56
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 61
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 62
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 63
    if (any(r /= 0)) stop 64
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 65
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 71
    if (any(r /= reshape((/ real:: data1 /), shape=(/ 4, 5 /)))) stop 72
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 73
    if (any(r /= reshape((/ real:: data2 /), shape=(/ 3, 5 /)))) stop 74
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 75
    if (any(r /= reshape((/ real:: data3 /), shape=(/ 3, 4 /)))) stop 76
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 81
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 82
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 83
    if (any(r /= 0)) stop 84
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 85
  end subroutine
end subroutine

subroutine check_minloc_with_mask
  implicit none
  integer, parameter :: data60(*) = (/ 7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8,  &
                                       7, 4, 5, 3, 9, 0, 6, 4, 5, 5,  &
                                       8, 2, 6, 7, 8, 7, 4, 5, 3, 9,  &
                                       0, 6, 4, 5, 5, 8, 2, 6, 7, 8  /)
  logical, parameter :: mask60(*) = (/ .true. , .false., .false., .false., &
                                       .true. , .false., .true. , .false., &
                                       .false., .true. , .true. , .false., &
                                       .true. , .true. , .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .true. , &
                                       .true. , .false., .false., .true. , &
                                       .true. , .true. , .true. , .false., &
                                       .false., .false., .true. , .false., &
                                       .true. , .false., .true. , .true. , &
                                       .true. , .false., .true. , .true. , &
                                       .false., .true. , .false., .true. , &
                                       .false., .true. , .false., .false., &
                                       .false., .true. , .true. , .true. , &
                                       .false., .true. , .false., .true.  /)
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
  integer, parameter :: data1m(*) = (/ 1, 2, 1, 1,  &
                                       1, 3, 2, 3,  &
                                       1, 1, 1, 2,  &
                                       3, 1, 1, 3,  &
                                       2, 3, 1, 1  /)
  integer, parameter :: data2m(*) = (/ 4, 4, 0,  &
                                       1, 1, 2,  &
                                       1, 2, 2,  &
                                       2, 3, 1,  &
                                       3, 3, 2  /)
  integer, parameter :: data3m(*) = (/ 3, 2, 4,  &
                                       4, 3, 2,  &
                                       5, 4, 0,  &
                                       1, 1, 2  /)
  call check_int_const_shape_rank_3
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_3
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_empty_4
  call check_lower_bounds
  call check_dependencies
contains
  subroutine check_int_const_shape_rank_3()
    integer :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    m = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 11
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 12
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 13
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 14
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 15
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 16
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 61
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 62
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 63
    if (any(r /= 0)) stop 64
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 65
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 71
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 72
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 73
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 74
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 75
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 76
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical:: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 101
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 102
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 103
    if (any(r /= 0)) stop 104
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 105
  end subroutine
  subroutine check_real_const_shape_rank_3()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 111
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 112
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 113
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 114
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 115
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 116
  end subroutine
  subroutine check_real_const_shape_empty_4()
    real :: a(9,3,0,7)
    logical :: m(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ real:: /), shape(a))
    m = reshape((/ logical:: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 161
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 162
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 163
    if (any(r /= 0)) stop 164
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 165
  end subroutine
  subroutine check_real_alloc_rank_3()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5), m(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    m(:,:,:) = reshape(mask60, shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 171
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 172
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 173
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 174
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 175
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 176
  end subroutine
  subroutine check_real_alloc_empty_4()
    real, allocatable :: a(:,:,:,:)
    logical, allocatable :: m(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7), m(9,3,0,7))
    a(:,:,:,:) = reshape((/ real:: /), shape(a))
    m(:,:,:,:) = reshape((/ logical :: /), shape(m))
    r = minloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 201
    r = minloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 202
    r = minloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 203
    if (any(r /= 0)) stop 204
    r = minloc(a, dim = 4, mask = m)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 205
  end subroutine
end subroutine
