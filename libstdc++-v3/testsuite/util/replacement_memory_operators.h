//
// Copyright (C) 2007-2022 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along
// with this library; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#include <exception>
#include <stdexcept>
#include <cstdlib>
#include <cstdio>
#include <testsuite_hooks.h>

namespace __gnu_test
{
  struct counter_error : public std::exception { };

  struct counter
  {
    std::size_t _M_count;
    bool	_M_throw;

    counter() : _M_count(0), _M_throw(true) { }

    ~counter() THROW (counter_error)
    {
      if (_M_throw && _M_count != 0)
	throw counter_error();
    }

    static void
    increment() { get()._M_count++; }

    static void
    decrement() { get()._M_count--; }

    static counter&
    get()
    {
      static counter g;
      return g;
    }

    static std::size_t
    count() { return get()._M_count; }

    static void
    exceptions(bool __b) { get()._M_throw = __b; }
  };

  template<typename Alloc, bool uses_global_new>
    bool
    check_new(Alloc a = Alloc())
    {
      __gnu_test::counter::exceptions(false);
      (void) a.allocate(10);
      const bool __b((__gnu_test::counter::count() > 0) == uses_global_new);
      if (!__b)
	throw std::logic_error("counter not incremented");
      return __b;
    }

  template<typename Alloc, bool uses_global_delete>
    bool
    check_delete(Alloc a = Alloc())
    {
      __gnu_test::counter::exceptions(false);
#if __cplusplus >= 201103L
      auto p = a.allocate(10);
#else
      typename Alloc::pointer p = a.allocate(10);
#endif
      const std::size_t count1 = __gnu_test::counter::count();
      a.deallocate(p, 10);
      const std::size_t count2 = __gnu_test::counter::count();
      const bool __b((count2 < count1) == uses_global_delete);
      if (!__b)
	throw std::logic_error("counter not decremented");
      return __b;
    }

  template<typename Alloc, bool uses_global_delete>
    typename Alloc::value_type
    check_reallocate_and_read(Alloc a = Alloc())
      {
	typename Alloc::pointer p, p2;
	typename Alloc::value_type ret;
	__gnu_test::counter::exceptions(false);
#define ALLOC_AND_ADD(NUM_OBJS) \
	{ \
	  p = a.allocate((NUM_OBJS)); \
	  p2 = a.allocate((NUM_OBJS)); \
	  ret += *(p  + NUM_OBJS - 1); \
	  ret += *(p2 + NUM_OBJS - 1); \
	  a.deallocate(p, (NUM_OBJS)); \
	  a.deallocate(p2, (NUM_OBJS)); \
	}

	// Allocate and add a few times for an allocation that should use
	// operator new (i.e. a large allocation).
	ALLOC_AND_ADD (100);
	ALLOC_AND_ADD (100);
	ALLOC_AND_ADD (100);

	// Now allocate and add a few times for an allocation that might use
	// operator new.
	ALLOC_AND_ADD (10);
	ALLOC_AND_ADD (10);
	ALLOC_AND_ADD (10);

	// Now allocate and add a few times for an allocation that probably
	// won't use operator new.
	ALLOC_AND_ADD (1);
	ALLOC_AND_ADD (1);
	ALLOC_AND_ADD (1);

	// N.b. we are not trying to check anything about the values and
	// pointers, just trying to check that the reads are allowed and that
	// deallocating and reallocating doesn't crash.
	// We want to ensure that the reads are not optimised out, hence we
	// return some combination of the reads.
	// In order to run this test the value_type must be something which has
	// an operator+ on it.
	return ret;
      }

  template<typename Alloc, bool uses_global_delete>
    typename Alloc::value_type
    check_read_out_of_bounds(Alloc a = Alloc())
      {
	// N.b. we choose the size quite carefully for our tests.
	// Unfortunately this is based on implementation details.
	// Our memory allocators which use this test both round up the bounds
	// of the allocations they return in order to simplify the
	// implementation details.  Hence in order to produce a working test we
	// use a number of allocations which result in tight bounds despite
	// this rounding up.
	__gnu_test::counter::exceptions(false);
#if __cplusplus >= 201103L
	auto p = a.allocate(8);
	auto val = *(p+8);
	a.deallocate(p, 8);
#else
	typename Alloc::pointer p = a.allocate(8);
	typename Alloc::value_type val = *(p+8);
	a.deallocate(p, 8);
#endif
	// N.b. we are not trying to check anything about the values and
	// pointers, just trying to check that the read above is not OK.
	return val;
      }

  template<typename Alloc, bool uses_global_delete>
    typename Alloc::value_type
    check_read_end_of_bounds(Alloc a = Alloc())
      {
	// Allocate a large-ish sized allocation, then reclaim it, then
	// allocate again.  Often this will result in being given the same
	// original allocation back.
	// This is useful to test CHERI bounded pointers to ensure that any
	// bounds applied do not restrict the access of any future allocations.
	__gnu_test::counter::exceptions(false);
	typename Alloc::pointer p;
	typename Alloc::value_type val;
	p = a.allocate(9);
	val = *(p+8);
	a.deallocate(p, 9);
	p = a.allocate(10);
	val += *(p+9);
	a.deallocate(p, 10);
	// N.b. we are not trying to check anything about the values and
	// pointers, just trying to check that the read above is not OK.
	return val;
      }
} // namespace __gnu_test

void* operator new(std::size_t size) THROW(std::bad_alloc)
{
  std::printf("operator new is called \n");
  void* p = std::malloc(size);
  if (!p)
    throw std::bad_alloc();
  __gnu_test::counter::increment();
  return p;
}

void operator delete(void* p) throw()
{
  std::printf("operator delete is called \n");
  if (p)
    {
      std::free(p);
      __gnu_test::counter::decrement();

      std::size_t count = __gnu_test::counter::count();
      if (count == 0)
	std::printf("All memory released \n");
      else
	std::printf("%lu allocations to be released \n", (unsigned long)count);
    }
}
