/* { dg-do compile } */

int
down_bad_type () {
    return __builtin_align_down ((badtype)128, 16); /* { dg-error ".badtype. undeclared" } */
    /* { dg-error "before numeric constant" "" { target *-*-* } .-1 } */
    /* { dg-error "too few arguments" "" { target *-*-* } .-2 } */
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
    return __builtin_align_up ((badtype)128, 16); /* { dg-error ".badtype. undeclared" } */
    /* { dg-error "before numeric constant" "" { target *-*-* } .-1 } */
    /* { dg-error "too few arguments" "" { target *-*-* } .-2 } */
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
    return __builtin_is_aligned ((badtype)128, 16); /* { dg-error ".badtype. undeclared" } */
    /* { dg-error "before numeric constant" "" { target *-*-* } .-1 } */
    /* { dg-error "too few arguments" "" { target *-*-* } .-2 } */
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
