// PR preprocessor/80005
// Implementation backwards compatibility
// { dg-do preprocess }

#undef __has_include
#ifdef __has_include
#error "__has_include still exists"
#endif

#undef vector
#define vector NOPE

#if !__has_include__ (<vector>)
#error "Header 'vector' could not be found"
#endif

#if __has_include__ ("not an escape! \")	// comment
#endif

// This #define of __has_include is broken!
#define __has_include(X) __has_include__ (X)
#if !__has_include (<vector>)
#error "Header 'vector' could not be found" // { dg-error "not be found" }
#endif

#undef __has_include
// And this one is ok
#define __has_include __has_include__
#if !__has_include (<vector>)
#error "Header 'vector' could not be found"
#endif
