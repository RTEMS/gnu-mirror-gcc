typedef unsigned long size_t;
struct tempstruct {
    unsigned short x;
    unsigned int y;
};
unsigned
down (struct tempstruct x, size_t alignment) {
    return __builtin_align_down (x, alignment); /* { dg-error "incompatible type for argument 1" } */
}
unsigned
up (struct tempstruct x, size_t alignment) {
    return __builtin_align_up (x, alignment); /* { dg-error "incompatible type for argument 1" } */
}
unsigned
aligned (struct tempstruct x, size_t alignment) {
    return __builtin_is_aligned (x, alignment); /* { dg-error "incompatible type for argument 1" } */
}
