! { dg-do compile }
!
! PR fortran/118896
! Check that optimizations devirtualize all the calls to the internal _copy
! typebound subroutine, and no reference to the virtual table remains.
!
! { dg-additional-options {-O2 -fdump-tree-original -fdump-tree-optimized} }
! { dg-final { scan-tree-dump {__vtab} {original} } }
! { dg-final { scan-tree-dump-not {__vtab} {optimized} } }

module m
  implicit none
  type :: t1
    integer :: i
  end type
end module m

subroutine test_t1
  use m
  implicit none

  class(t1), dimension(:), allocatable :: x, y

  x = [t1(3), t1(2), t1(1)]

  x = realloc_t1 (x)
  if (.not.check_t1 (x, [2,3,1], 1) ) stop 3

contains

  function realloc_t1 (arg) result (res)
    class(t1), dimension(:), allocatable :: arg
    class(t1), dimension(:), allocatable :: res
    select type (arg)
      type is (t1)
        allocate (res, source = [t1 (arg(2)%i), t1 (arg(1)%i), t1 (arg(3)%i)])
    end select
  end function realloc_t1

  logical function check_t1 (arg, array, t, array2)
    class(t1) :: arg(:)
    integer :: array (:), t
    integer, optional :: array2(:)
    check_t1 = .true.
    select type (arg)
    type is (t1)
      if (any (arg%i .ne. array)) check_t1 = .false.
      if (t .eq. 2) check_t1 = .false.
    class default
      check_t1 = .false.
    end select
  end function check_t1

end subroutine test_t1

  call test_t1
end
