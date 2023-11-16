! { dg-do run }
!
! PR fortran/90608
! Check the correct behaviour of the inline maxloc implementation,
! when the dim and mask argument are present.

program p
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
  call check_int_const_shape_rank_3_true_mask
  call check_int_const_shape_rank_3_false_mask
  call check_int_const_shape_rank_3_optional_mask_present
  call check_int_const_shape_rank_3_optional_mask_absent
  call check_int_const_shape_empty_4
  call check_int_alloc_rank_3
  call check_int_alloc_rank_3_true_mask
  call check_int_alloc_rank_3_false_mask
  call check_int_alloc_empty_4
  call check_real_const_shape_rank_3
  call check_real_const_shape_rank_3_true_mask
  call check_real_const_shape_rank_3_false_mask
  call check_real_const_shape_rank_3_optional_mask_present
  call check_real_const_shape_rank_3_optional_mask_absent
  call check_real_const_shape_empty_4
  call check_real_alloc_rank_3
  call check_real_alloc_rank_3_true_mask
  call check_real_alloc_rank_3_false_mask
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
  subroutine call_maxloc_int(r, a, d, m)
    integer :: a(:,:,:)
    integer :: d
    logical, optional :: m(:,:,:)
    integer, allocatable :: r(:,:)
    r = maxloc(a, dim = d, mask = m)
  end subroutine
  subroutine check_int_const_shape_rank_3_optional_mask_present()
    integer :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    m = reshape(mask60, shape(m))
    call call_maxloc_int(r, a, 1, m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 41
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 42
    call call_maxloc_int(r, a, 2, m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 43
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 44
    call call_maxloc_int(r, a, 3, m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 45
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 46
  end subroutine
  subroutine check_int_const_shape_rank_3_optional_mask_absent()
    integer :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape(data60, shape(a))
    call call_maxloc_int(r, a, 1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 51
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 52
    call call_maxloc_int(r, a, 2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 53
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 54
    call call_maxloc_int(r, a, 3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 55
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 56
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
  subroutine check_real_const_shape_rank_3_true_mask()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim = 1, mask = .true.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 121
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 122
    r = maxloc(a, dim = 2, mask = .true.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 123
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 124
    r = maxloc(a, dim = 3, mask = .true.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 125
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 126
  end subroutine
  subroutine check_real_const_shape_rank_3_false_mask()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim = 1, mask = .false.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 131
    if (any(r /= 0)) stop 132
    r = maxloc(a, dim = 2, mask = .false.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 133
    if (any(r /= 0)) stop 134
    r = maxloc(a, dim = 3, mask = .false.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 135
    if (any(r /= 0)) stop 136
  end subroutine
  subroutine call_maxloc_real(r, a, d, m)
    real :: a(:,:,:)
    integer :: d
    logical, optional  :: m(:,:,:)
    integer, allocatable :: r(:,:)
    r = maxloc(a, dim = d, mask = m)
  end subroutine
  subroutine check_real_const_shape_rank_3_optional_mask_present()
    real :: a(3,4,5)
    logical :: m(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    call call_maxloc_real(r, a, 1, m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 141
    if (any(r /= reshape(data1m, (/ 4, 5 /)))) stop 142
    call call_maxloc_real(r, a, 2, m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 143
    if (any(r /= reshape(data2m, (/ 3, 5 /)))) stop 144
    call call_maxloc_real(r, a, 3, m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 145
    if (any(r /= reshape(data3m, (/ 3, 4 /)))) stop 146
  end subroutine
  subroutine check_real_const_shape_rank_3_optional_mask_absent()
    real :: a(3,4,5)
    integer, allocatable :: r(:,:)
    a = reshape((/ real:: data60 /), shape(a))
    call call_maxloc_real(r, a, 1)
    if (any(shape(r) /= (/ 4, 5 /))) stop 151
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 152
    call call_maxloc_real(r, a, 2)
    if (any(shape(r) /= (/ 3, 5 /))) stop 153
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 154
    call call_maxloc_real(r, a, 3)
    if (any(shape(r) /= (/ 3, 4 /))) stop 155
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 156
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
  subroutine check_real_alloc_rank_3_true_mask()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim = 1, mask = .true.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 181
    if (any(r /= reshape(data1, (/ 4, 5 /)))) stop 182
    r = maxloc(a, dim = 2, mask = .true.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 183
    if (any(r /= reshape(data2, (/ 3, 5 /)))) stop 184
    r = maxloc(a, dim = 3, mask = .true.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 185
    if (any(r /= reshape(data3, (/ 3, 4 /)))) stop 186
  end subroutine
  subroutine check_real_alloc_rank_3_false_mask()
    real, allocatable :: a(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3,4,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    r = maxloc(a, dim = 1, mask = .false.)
    if (any(shape(r) /= (/ 4, 5 /))) stop 191
    if (any(r /= 0)) stop 192
    r = maxloc(a, dim = 2, mask = .false.)
    if (any(shape(r) /= (/ 3, 5 /))) stop 193
    if (any(r /= 0)) stop 194
    r = maxloc(a, dim = 3, mask = .false.)
    if (any(shape(r) /= (/ 3, 4 /))) stop 195
    if (any(r /= 0)) stop 196
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
  subroutine check_lower_bounds()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    integer, allocatable :: r(:,:)
    allocate(a(3:5,-1:2,5), m(3:5,-1:2,5))
    a(:,:,:) = reshape((/ real:: data60 /), shape(a))
    m = reshape(mask60, shape(m))
    r = maxloc(a, dim = 1, mask = m)
    if (any(shape(r) /= (/ 4, 5 /))) stop 211
    if (any(lbound(r) /= 1)) stop 212
    if (any(ubound(r) /= (/ 4, 5 /))) stop 213
    r = maxloc(a, dim = 2, mask = m)
    if (any(shape(r) /= (/ 3, 5 /))) stop 214
    if (any(lbound(r) /= 1)) stop 215
    if (any(ubound(r) /= (/ 3, 5 /))) stop 216
    r = maxloc(a, dim = 3, mask = m)
    if (any(shape(r) /= (/ 3, 4 /))) stop 217
    if (any(lbound(r) /= 1)) stop 218
    if (any(ubound(r) /= (/ 3, 4 /))) stop 219
  end subroutine
  elemental subroutine set(o, i)
    real, intent(out) :: o
    integer, intent(in)  :: i
    o = i
  end subroutine
  subroutine check_dependencies()
    real, allocatable :: a(:,:,:)
    logical, allocatable :: m(:,:,:)
    allocate(a(3,4,5),m(3,4,5))
    m(:,:,:) = reshape(mask60, shape(m))
    a(:,:,:) = reshape(data60, shape(a))
    a(1,:,:) = maxloc(a, dim = 1, mask = m)
    if (any(a(1,:,:) /= reshape(data1m, (/ 4, 5 /)))) stop 231
    a(:,:,:) = reshape(data60, shape(a))
    a(:,2,:) = maxloc(a, dim = 2, mask = m)
    if (any(a(:,2,:) /= reshape(data2m, (/ 3, 5 /)))) stop 232
    a(:,:,:) = reshape(data60, shape(a))
    a(:,:,5) = maxloc(a, dim = 3, mask = m)
    if (any(a(:,:,5) /= reshape(data3m, (/ 3, 4 /)))) stop 233
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(1,:,:), maxloc(a, dim = 1, mask = m))
    if (any(a(1,:,:) /= reshape(data1m, (/ 4, 5 /)))) stop 234
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(:,2,:), maxloc(a, dim = 2, mask = m))
    if (any(a(:,2,:) /= reshape(data2m, (/ 3, 5 /)))) stop 235
    a(:,:,:) = reshape(data60, shape(a))
    call set(a(:,:,5), maxloc(a, dim = 3, mask = m))
    if (any(a(:,:,5) /= reshape(data3m, (/ 3, 4 /)))) stop 236
  end subroutine
end program p
