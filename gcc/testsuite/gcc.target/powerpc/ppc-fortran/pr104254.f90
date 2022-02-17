! { dg-do compile }
! { dg-require-effective-target powerpc_p9vector_ok }
! { dg-options "-mdejagnu-cpu=power9 -O1 -fnon-call-exceptions" }

! PR target/104254.  GCC would raise an assertion error if this program was
! compiled with -O1 and -fnon-call-exceptions on a power9 or higher.  The issue
! occurs because at this optimization level, the compiler is trying to make
! a conditional move to store integers using a 32-bit floating point compare.
! It wants to use UNLE, which is not supported for integer modes.
  
  real :: a(2), nan
  real, allocatable :: c(:)
  integer :: ia(1)

  nan = 0.0
  nan = 0.0/nan

  a(:) = nan
  ia = maxloc (a)
  if (ia(1).ne.1) STOP 1

  allocate (c(1))
  c(:) = nan
  deallocate (c)
end
