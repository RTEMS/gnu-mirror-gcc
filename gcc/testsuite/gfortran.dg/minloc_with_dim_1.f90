! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline minloc implementation,
! when the dim argument is present.

program p
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
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 11
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 12
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 13
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 14
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 15
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 16
  end subroutine
  subroutine check_int_const_shape_empty_4()
    integer :: a(9,3,0,7)
    integer, allocatable :: r(:,:,:)
    a = reshape((/ integer:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 21
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 22
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 23
    if (any(r /= 0)) stop 24
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 25
  end subroutine
  subroutine check_int_alloc_rank_3()
    integer, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 31
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 32
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 33
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 34
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 35
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 36
  end subroutine
  subroutine check_int_alloc_empty_4()
    integer, allocatable :: a(:,:,:,:)
    integer, allocatable :: r(:,:,:)
    allocate(a(9,3,0,7))
    a(:,:,:,:) = reshape((/ integer:: /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 3, 0, 7 /))) stop 41
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 9, 0, 7 /))) stop 42
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 9, 3, 7 /))) stop 43
    if (any(r /= 0)) stop 44
    r = minloc(a, dim=4)
    if (any(shape(r) /= (/ 9, 3, 0 /))) stop 45
  end subroutine
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
  subroutine check_lower_bounds()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3:5,-1:2,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = minloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 91
    if (any(lbound(r) /= 1)) stop 92
    if (any(ubound(r) /= (/ 4, 5 /))) stop 93
    r = minloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 94
    if (any(lbound(r) /= 1)) stop 95
    if (any(ubound(r) /= (/ 3, 5 /))) stop 96
    r = minloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 97
    if (any(lbound(r) /= 1)) stop 98
    if (any(ubound(r) /= (/ 3, 4 /))) stop 99
  end subroutine
  elemental subroutine set(o, i)
    integer, intent(out) :: o
    integer, intent(in)  :: i
    o = i
  end subroutine
  subroutine check_dependencies()
    integer, allocatable :: a(:,:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape(data60, shape(a))
    a(1,:,:) = minloc(a, dim=1)
    if (any(a(1,:,:) /= reshape(data1, (/ 4, 5 /)))) stop 111
    a(:,:,:) = reshape(data60, shape(a))
    a(:,2,:) = minloc(a, dim=2)
    if (any(a(:,2,:) /= reshape(data2, (/ 3, 5 /)))) stop 112
    a(:,:,:) = reshape(data60, shape(a))
    a(:,:,5) = minloc(a, dim=3)
    if (any(a(:,:,5) /= reshape(data3, (/ 3, 4 /)))) stop 113
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(1,:,:), minloc(a, dim=1))
    if (any(a(1,:,:) /= reshape(data1, (/ 4, 5 /)))) stop 114
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(:,2,:), minloc(a, dim=2))
    if (any(a(:,2,:) /= reshape(data2, (/ 3, 5 /)))) stop 115
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(:,:,5), minloc(a, dim=3))
    if (any(a(:,:,5) /= reshape(data3, (/ 3, 4 /)))) stop 116
  end subroutine check_dependencies
end program p
