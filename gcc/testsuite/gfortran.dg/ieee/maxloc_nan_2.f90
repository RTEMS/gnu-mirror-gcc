! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline maxloc implementation,
! when the dim argument is present.

program p
  implicit none
  call check_without_mask
  call check_with_mask
contains
  subroutine check_without_mask()
    use, intrinsic :: ieee_arithmetic
    real, allocatable :: a(:,:,:)
    real :: nan
    integer, allocatable :: r(:,:)
    if (.not. ieee_support_nan(nan)) return
    nan = ieee_value(nan, ieee_quiet_nan)
    allocate(a(3,4,5), source = nan)
    r = maxloc(a, dim=1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 21
    if (any(r /= 1)) stop 22
    r = maxloc(a, dim=2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 23
    if (any(r /= 1)) stop 24
    r = maxloc(a, dim=3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 25
    if (any(r /= 1)) stop 26
  end subroutine
  subroutine check_with_mask()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    real :: nan
    integer, allocatable :: r(:,:)
    if (.not. ieee_support_nan(nan)) return
    nan = ieee_value(nan, ieee_quiet_nan)
    allocate(a(2,3,4), source = nan)
    allocate(m(2,3,4))
    m(:,:,:) = reshape((/ .false., .false., .true. , .true. ,  &
                          .false., .true. , .false., .false.,  &
                          .false., .true. , .true. , .false.,  &
                          .true. , .true. , .true. , .false.,  &
                          .false., .true. , .true. , .false.,  &
                          .false., .true. , .false., .false.  /), shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 51
    if (any(r /= reshape((/ 0, 1, 2,  &
                            0, 2, 1,  &
                            1, 1, 2,  &
                            1, 2, 0  /), (/ 3, 4 /)))) stop 52
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 2, 4 /))) stop 53
    if (any(r /= reshape((/ 2, 2,  &
                            3, 2,  &
                            1, 1,  &
                            1, 2  /), (/ 2, 4 /)))) stop 54
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 2, 3 /))) stop 55
    if (any(r /= reshape((/ 3, 3,  &
                            1, 1,  &
                            2, 1  /), (/ 2, 3 /)))) stop 56
  end subroutine
end program p

