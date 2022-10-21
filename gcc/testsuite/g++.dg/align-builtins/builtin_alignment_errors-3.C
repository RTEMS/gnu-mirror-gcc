/* { dg-do compile } */

int
down_bad_type () {
    return __builtin_align_down ((badtype)128, 16); /* { dg-error ".badtype. was not declared" } */
}

int
down_no_alignment () {
    return __builtin_align_down ((int)128); /* { dg-error "too few arguments" } */
}

int
down_bad_alignment () {
    return __builtin_align_down ((int)128, 13); /* { dg-warning "requested alignment is not a power of 2" } */
}

int
down_no_arguments () {
    return __builtin_align_down(); /* { dg-error "too few arguments to function" } */
}

int
up_bad_type () {
    return __builtin_align_up ((badtype)128, 16); /* { dg-error ".badtype. was not declared" } */
}

int
up_no_alignment () {
    return __builtin_align_up ((int)128); /* { dg-error "too few arguments" } */
}

int
up_bad_alignment () {
    return __builtin_align_up ((int)128, 13); /* { dg-warning "requested alignment is not a power of 2" } */
}

int
up_no_arguments () {
    return __builtin_align_up(); /* { dg-error "too few arguments" } */
}

int
check_bad_type () {
    return __builtin_is_aligned ((badtype)128, 16); /* { dg-error ".badtype. was not declared" } */
}

int
check_no_alignment () {
    return __builtin_is_aligned ((int)128); /* { dg-error "too few arguments" } */
}

int
check_bad_alignment () {
    return __builtin_is_aligned ((int)128, 13); /* { dg-warning "requested alignment is not a power of 2" } */
}

int
check_no_arguments () {
    return __builtin_is_aligned(); /* { dg-error "too few arguments" } */
}

#if __cplusplus >= 201103L
constexpr const int aligndown = __builtin_align_down (128, 13);  /* { dg-error "alignment must be power of 2" "" { target c++11 } } */
/* { dg-warning "requested alignment is not a power of 2" "" { target c++11 } .-1 } */
constexpr const int alignup = __builtin_align_up (128, 13);  /* { dg-error "alignment must be power of 2" "" { target c++11 } } */
/* { dg-warning "requested alignment is not a power of 2" "" { target c++11 } .-1 } */
constexpr const int aligned = __builtin_is_aligned (128, 13);  /* { dg-error "alignment must be power of 2" "" { target c++11 } } */
/* { dg-warning "requested alignment is not a power of 2" "" { target c++11 } .-1 } */
constexpr const int aligndown_zero = __builtin_align_down (128, 0);  /* { dg-error "alignment must be nonzero" "" { target c++11 } } */
/* { dg-warning "requested alignment must be nonzero" "" { target c++11 } .-1 } */
constexpr const int alignup_zero = __builtin_align_up (128, 0);  /* { dg-error "alignment must be nonzero" "" { target c++11 } } */
/* { dg-warning "requested alignment must be nonzero" "" { target c++11 } .-1 } */
constexpr const int aligned_zero = __builtin_is_aligned (128, 0);  /* { dg-error "alignment must be nonzero" "" { target c++11 } } */
/* { dg-warning "requested alignment must be nonzero" "" { target c++11 } .-1 } */

int
down_constexpr_bad_alignment () {
    constexpr int x = __builtin_align_down ((int)128, 13);  /* { dg-error "alignment must be power of 2" "" { target c++11 } } */
    /* { dg-warning "requested alignment is not a power of 2" "" { target c++11 } .-1 } */
    return x;
}

int
down_constexpr_zero_alignment () {
    constexpr int x = __builtin_align_down ((int)128, 0);  /* { dg-error "alignment must be nonzero" "" { target c++11 } } */
    /* { dg-warning "requested alignment must be nonzero" "" { target c++11 } .-1 } */
    return x;
}

int
up_constexpr_bad_alignment () {
    constexpr int x = __builtin_align_up ((int)128, 13);  /* { dg-error "alignment must be power of 2" "" { target c++11 } } */
    /* { dg-warning "requested alignment is not a power of 2" "" { target c++11 } .-1 } */
    return x;
}

int
up_constexpr_zero_alignment () {
    constexpr int x = __builtin_align_up ((int)128, 0);  /* { dg-error "alignment must be nonzero" "" { target c++11 } } */
    /* { dg-warning "requested alignment must be nonzero" "" { target c++11 } .-1 } */
    return x;
}

int
check_constexpr_bad_alignment () {
    constexpr int x = __builtin_is_aligned ((int)128, 13);  /* { dg-error "alignment must be power of 2" "" { target c++11 } } */
    /* { dg-warning "requested alignment is not a power of 2" "" { target c++11 } .-1 } */
    return x;
}

int
check_constexpr_zero_alignment () {
    constexpr int x = __builtin_is_aligned ((int)128, 0);  /* { dg-error "alignment must be nonzero" "" { target c++11 } } */
    /* { dg-warning "requested alignment must be nonzero" "" { target c++11 } .-1 } */
    return x;
}
#endif
