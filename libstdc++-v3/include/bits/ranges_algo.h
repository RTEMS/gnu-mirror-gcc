// Core algorithmic facilities -*- C++ -*-

// Copyright (C) 2019 Free Software Foundation, Inc.
//
// This file is part of the GNU ISO C++ Library.  This library is free
// software; you can redistribute it and/or modify it under the
// terms of the GNU General Public License as published by the
// Free Software Foundation; either version 3, or (at your option)
// any later version.

// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// Under Section 7 of GPL version 3, you are granted additional
// permissions described in the GCC Runtime Library Exception, version
// 3.1, as published by the Free Software Foundation.

// You should have received a copy of the GNU General Public License and
// a copy of the GCC Runtime Library Exception along with this program;
// see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
// <http://www.gnu.org/licenses/>.

/** @file bits/ranges_algo.h
 *  This is an internal header file, included by other library headers.
 *  Do not attempt to use it directly. @headername{algorithm}
 */

#ifndef _RANGES_ALGO_H
#define _RANGES_ALGO_H 1

#if __cplusplus > 201703L

#include <compare>
#include <iterator>
// #include <bits/range_concepts.h>
#include <ranges>
#include <bits/invoke.h>
#include <bits/cpp_type_traits.h> // __is_byte

#if __cpp_lib_concepts
namespace std _GLIBCXX_VISIBILITY(default)
{
_GLIBCXX_BEGIN_NAMESPACE_VERSION
namespace ranges
{
  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr bool
    all_of(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (!std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  return false;
      return true;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr bool
    all_of(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::all_of(ranges::begin(__r), ranges::end(__r),
			    std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr bool
    any_of(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  return true;
      return false;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr bool
    any_of(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::any_of(ranges::begin(__r), ranges::end(__r),
			    std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr bool
    none_of(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  return false;
      return true;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr bool
    none_of(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::none_of(ranges::begin(__r), ranges::end(__r),
			    std::move(__pred), std::move(__proj));
    }

  template<typename _Iter, typename _F>
    struct for_each_result
    {
      [[no_unique_address]] _Iter in;
      [[no_unique_address]] _F fun;

      template<typename _Iter2, typename _F2>
	requires convertible_to<const _Iter&, _Iter2>
	  && convertible_to<const _F&, _F2>
	operator for_each_result<_Iter2, _F2>() const &
	{
	  return {in, fun};
	}

      template<typename _Iter2, typename _F2>
	requires convertible_to<_Iter, _Iter2> && convertible_to<_F, _F2>
	operator for_each_result<_Iter2, _F2>() &&
	{
	  return {std::move(in), std::move(fun)};
	}
    };

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirectly_unary_invocable<projected<_Iter, _Proj>> _Fun>
    constexpr for_each_result<_Iter, _Fun>
    for_each(_Iter __first, _Sent __last, _Fun __f, _Proj __proj = {})
    {
      for (; __first != __last; ++__first)
	std::__invoke(__f, std::__invoke(__proj, *__first));
      return { __first, std::move(__f) };
    }

  template<input_range _Range, typename _Proj = identity,
	   indirectly_unary_invocable<projected<iterator_t<_Range>, _Proj>>
	     _Fun>
    constexpr for_each_result<safe_iterator_t<_Range>, _Fun>
    for_each(_Range&& __r, _Fun __f, _Proj __proj = {})
    {
      return ranges::for_each(ranges::begin(__r), ranges::end(__r),
			      std::move(__f), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent, typename _Tp,
	   typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<_Iter, _Proj>, const _Tp*>
    constexpr _Iter
    find(_Iter __first, _Sent __last, const _Tp& __value, _Proj __proj = {})
    {
      while (__first != __last
	  && !(std::__invoke(__proj, *__first) == __value))
	++__first;
      return __first;
    }

  template<input_range _Range, typename _Tp, typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<iterator_t<_Range>, _Proj>,
				       const _Tp*>
    constexpr safe_iterator_t<_Range>
    find(_Range&& __r, const _Tp& __value, _Proj __proj = {})
    {
      return ranges::find(ranges::begin(__r), ranges::end(__r), __value,
			  std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr _Iter
    find_if(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      while (__first != __last
	  && !(bool)std::__invoke(__pred, std::__invoke(__proj, *__first)))
	++__first;
      return __first;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>>
	     _Pred>
    constexpr safe_iterator_t<_Range>
    find_if(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::find_if(ranges::begin(__r), ranges::end(__r),
			     std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr _Iter
    find_if_not(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      while (__first != __last
	  && (bool)std::__invoke(__pred, std::__invoke(__proj, *__first)))
	++__first;
      return __first;
    }

  template<input_range _Range, typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>>
	     _Pred>
    constexpr safe_iterator_t<_Range>
    find_if_not(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::find_if_not(ranges::begin(__r), ranges::end(__r),
				 std::move(__pred), std::move(__proj));
    }

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr _Iter1
    find_first_of(_Iter1 __first1, _Sent1 __last1,
		  _Iter2 __first2, _Sent2 __last2,
		  _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      for (; __first1 != __last1; ++__first1)
	for (auto __iter = __first2; __iter != __last2; ++__iter)
	  if (std::__invoke(__pred,
			    std::__invoke(__proj1, *__first1),
			    std::__invoke(__proj2, *__iter)))
	    return __first1;
      return __first1;
    }

  template<input_range _Range1, forward_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr safe_iterator_t<_Range1>
    find_first_of(_Range1&& __r1, _Range2&& __r2,
		  _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::find_first_of(ranges::begin(__r1), ranges::end(__r1),
				   ranges::begin(__r2), ranges::end(__r2),
				   std::move(__pred),
				   std::move(__proj1), std::move(__proj2));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Tp, typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<_Iter, _Proj>,
				       const _Tp*>
    constexpr iter_difference_t<_Iter>
    count(_Iter __first, _Sent __last, const _Tp& __value, _Proj __proj = {})
    {
      iter_difference_t<_Iter> __n = 0;
      for (; __first != __last; ++__first)
	if (std::__invoke(__proj, *__first) == __value)
	  ++__n;
      return __n;
    }

  template<input_range _Range, typename _Tp, typename _Proj = identity>
    requires indirect_binary_predicate<ranges::equal_to,
				       projected<iterator_t<_Range>, _Proj>,
				       const _Tp*>
    constexpr range_difference_t<_Range>
    count(_Range&& __r, const _Tp& __value, _Proj __proj = {})
    {
      return ranges::count(ranges::begin(__r), ranges::end(__r),
			   __value, std::move(__proj));
    }

  template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
    constexpr iter_difference_t<_Iter>
    count_if(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
    {
      iter_difference_t<_Iter> __n = 0;
      for (; __first != __last; ++__first)
	if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	  ++__n;
      return __n;
    }

  template<input_range _Range,
	   typename _Proj = identity,
	   indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
    constexpr range_difference_t<_Range>
    count_if(_Range&& __r, _Pred __pred, _Proj __proj = {})
    {
      return ranges::count_if(ranges::begin(__r), ranges::end(__r),
			      std::move(__pred), std::move(__proj));
    }

  template<typename _Iter1, typename _Iter2>
    struct mismatch_result
    {
      [[no_unique_address]] _Iter1 in1;
      [[no_unique_address]] _Iter2 in2;

      template<typename _IIter1, typename _IIter2>
	requires convertible_to<const _Iter1&, _IIter1>
	  && convertible_to<const _Iter2&, _IIter2>
	operator mismatch_result<_IIter1, _IIter2>() const &
	{
	  return {in1, in2};
	}

      template<typename _IIter1, typename _IIter2>
	requires convertible_to<_Iter1, _IIter1>
	  && convertible_to<_Iter2, _IIter2>
	operator mismatch_result<_IIter1, _IIter2>() &&
	{
	  return {std::move(in1), std::move(in2)};
	}
    };

  template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr mismatch_result<_Iter1, _Iter2>
    mismatch(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      while (__first1 != __last1 && __first2 != __last2
	     && (bool)std::__invoke(__pred,
				    std::__invoke(__proj1, *__first1),
				    std::__invoke(__proj2, *__first2)))
      {
	++__first1;
	++__first2;
      }
      return { __first1, __first2 };
    }

  template<input_range _Range1, input_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr mismatch_result<iterator_t<_Range1>, iterator_t<_Range2>>
    mismatch(_Range1&& __r1, _Range2&& __r2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::mismatch(ranges::begin(__r1), ranges::end(__r1),
			      ranges::begin(__r2), ranges::end(__r2),
			      std::move(__pred),
			      std::move(__proj1), std::move(__proj2));
    }

  template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr subrange<_Iter1>
    search(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	   _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      if (__first1 == __last1 || __first2 == __last2)
	return {__first1, __first1};

      for (;;)
	{
	  for (;;)
	    {
	      if (__first1 == __last1)
		return {__first1, __first1};
	      if (std::__invoke(__pred,
				std::__invoke(__proj1, *__first1),
				std::__invoke(__proj2, *__first2)))
		break;
	      ++__first1;
	    }
	  auto __cur1 = __first1;
	  auto __cur2 = __first2;
	  for (;;)
	    {
	      if (++__cur2 == __last2)
		return {__first1, ++__cur1};
	      if (++__cur1 == __last1)
		return {__cur1, __cur1};
	      if (!std::__invoke(__pred,
				 std::__invoke(__proj1, *__cur1),
				 std::__invoke(__proj2, *__cur2)))
		{
		  ++__first1;
		  break;
		}
	    }
	}
    }

  template<forward_range _Range1, forward_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr safe_subrange_t<_Range1>
    search(_Range1&& __r1, _Range2&& __r2,
	   _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::search(ranges::begin(__r1), ranges::end(__r1),
			    ranges::begin(__r2), ranges::end(__r2),
			    std::move(__pred),
			    std::move(__proj1), std::move(__proj2));
    }

  template<forward_iterator _Iter, sentinel_for<_Iter> _Sent, typename _Tp,
	   typename _Pred = ranges::equal_to, typename _Proj = identity>
    requires indirectly_comparable<_Iter, const _Tp*, _Pred, _Proj>
    constexpr subrange<_Iter>
    search_n(_Iter __first, _Sent __last, iter_difference_t<_Iter> __count,
	     const _Tp& __value, _Pred __pred = {}, _Proj __proj = {})
    {
      if (__count <= 0)
	return {__first, __first};

      auto __value_comp = [&] <typename _Rp> (_Rp&& __arg) {
	  return std::__invoke(__pred, std::forward<_Rp>(__arg), __value);
      };
      if (__count == 1)
	{
	  __first = ranges::find_if(std::move(__first), __last,
				    std::move(__value_comp), std::move(__proj));
	  if (__first == __last)
	    return {__first, __first};
	  else
	    {
	      auto __end = __first;
	      return {__first, ++__end};
	    }
	}

      if constexpr (sized_sentinel_for<_Sent, _Iter>)
	{
	  auto __tail_size = __last - __first;
	  auto __remainder = __count;

	  while (__remainder <= __tail_size)
	    {
	      __first += __remainder;
	      __tail_size -= __remainder;
	      auto __backtrack = __first;
	      while (__value_comp(std::__invoke(__proj, *--__backtrack)))
		{
		  if (--__remainder == 0)
		    return {__first - __count, __first};
		}
	    }
	  auto __i = __first + __tail_size;
	  return {__i, __i};
	}
      else
	{
	  __first = ranges::find_if(__first, __last, __value_comp, __proj);
	  while (__first != __last)
	    {
	      auto __n = __count;
	      auto __i = __first;
	      ++__i;
	      while (__i != __last && __n != 1
		     && __value_comp(std::__invoke(__proj, *__i)))
		{
		  ++__i;
		  --__n;
		}
	      if (__n == 1)
		return {__first, __i};
	      if (__i == __last)
		return {__i, __i};
	      __first = ranges::find_if(++__i, __last, __value_comp, __proj);
	    }
	  return {__first, __first};
	}
    }

  template<forward_range _Range, typename _Tp,
	   typename _Pred = ranges::equal_to, typename _Proj = identity>
    requires indirectly_comparable<iterator_t<_Range>, const _Tp*, _Pred, _Proj>
    constexpr safe_subrange_t<_Range>
    search_n(_Range&& __r, range_difference_t<_Range> __count,
	     const _Tp& __value, _Pred __pred = {}, _Proj __proj = {})
    {
      return ranges::search_n(ranges::begin(__r), ranges::end(__r),
			      std::move(__count), __value,
			      std::move(__pred), std::move(__proj));
    }

  template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr subrange<_Iter1>
    __find_end(_Iter1 __first1, _Sent1 __last1,
	       _Iter2 __first2, _Sent2 __last2,
	       _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      auto __i = ranges::next(__first1, __last1);
      if (__first2 == __last2)
	return {__i, __i};

      auto __res_first = __i, __res_last = __i;
      for (;;)
	{
	  auto __new_range = ranges::search(__first1, __last1,
					    __first2, __last2,
					    __pred, __proj1, __proj2);
	  auto __new_res_first = ranges::begin(__new_range);
	  auto __new_res_last = ranges::end(__new_range);
	  if (__new_res_first == __last1)
	    return {__res_first, __res_last};
	  else
	    {
	      __res_first = __new_res_first;
	      __res_last = __new_res_last;
	      __first1 = __res_first;
	      ++__first1;
	    }
	}
    }

  template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	   forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
    constexpr subrange<_Iter1>
    find_end(_Iter1 __first1, _Sent1 __last1,
	     _Iter2 __first2, _Sent2 __last2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      if constexpr (bidirectional_iterator<_Iter1>
		    && bidirectional_iterator<_Iter2>)
	{
	  auto __i1 = ranges::next(__first1, __last1);
	  auto __i2 = ranges::next(__first2, __last2);
	  auto __rresult
	    = ranges::search(reverse_iterator<_Iter1>{__i1},
			     reverse_iterator<_Iter1>{__first1},
			     reverse_iterator<_Iter2>{__i2},
			     reverse_iterator<_Iter2>{__first2},
			     std::move(__pred),
			     std::move(__proj1), std::move(__proj2));
	  auto __result_first = ranges::end(__rresult).base();
	  auto __result_last = ranges::begin(__rresult).base();
	  if (__result_last == __first1)
	    return {__i1, __i1};
	  else
	    return {__result_first, __result_last};
	}
      else
	return ranges::__find_end(__first1, __last1, __first2, __last2,
				  std::move(__pred),
				  std::move(__proj1), std::move(__proj2));
    }

  template<forward_range _Range1, forward_range _Range2,
	   typename _Pred = ranges::equal_to,
	   typename _Proj1 = identity, typename _Proj2 = identity>
    requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				   _Pred, _Proj1, _Proj2>
    constexpr safe_subrange_t<_Range1>
    find_end(_Range1&& __r1, _Range2&& __r2,
	     _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
    {
      return ranges::find_end(ranges::begin(__r1), ranges::end(__r1),
			      ranges::begin(__r2), ranges::end(__r2),
			      std::move(__pred),
			      std::move(__proj1), std::move(__proj2));
    }

    template<forward_iterator _Iter, sentinel_for<_Iter> _Sent,
	     typename _Proj = identity,
	     indirect_binary_predicate<projected<_Iter, _Proj>,
				       projected<_Iter, _Proj>> _Pred
	       = ranges::equal_to>
      constexpr _Iter
      adjacent_find(_Iter __first, _Sent __last,
		    _Pred __pred = {}, _Proj __proj = {})
      {
	if (__first == __last)
	  return __first;
	auto __next = __first;
	auto __proj_first = std::__invoke(__proj, *__first);
	while (++__next != __last)
	  {
	    auto __proj_next = std::__invoke(__proj, *__next);
	    if (std::__invoke(__pred, std::move(__proj_first), __proj_next))
	      return __first;
	    __first = __next;
	    __proj_first = std::move(__proj_next);
	  }
	return __next;
      }

    template<forward_range _Range, typename _Proj = identity,
	     indirect_binary_predicate<
	       projected<iterator_t<_Range>, _Proj>,
	       projected<iterator_t<_Range>, _Proj>> _Pred = ranges::equal_to>
      constexpr safe_iterator_t<_Range>
      adjacent_find(_Range&& __r, _Pred __pred = {}, _Proj __proj = {})
      {
	return ranges::adjacent_find(ranges::begin(__r), ranges::end(__r),
				     std::move(__pred), std::move(__proj));
      }

    template<forward_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	     forward_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	     typename _Proj1 = identity, typename _Proj2 = identity,
	     indirect_equivalence_relation<projected<_Iter1, _Proj1>,
					   projected<_Iter2, _Proj2>> _Pred
	       = ranges::equal_to>
      constexpr bool
      is_permutation(_Iter1 __first1, _Sent1 __last1,
		     _Iter2 __first2, _Sent2 __last2, _Pred __pred = {},
		     _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
      {
	constexpr bool __sized_iters
	  = (sized_sentinel_for<_Sent1, _Iter1>
	     && sized_sentinel_for<_Sent2, _Iter2>);
	if constexpr (__sized_iters)
	  {
	    auto __d1 = ranges::distance(__first1, __last1);
	    auto __d2 = ranges::distance(__first2, __last2);
	    if (__d1 != __d2)
	      return false;
	  }

	// Efficiently compare identical prefixes:  O(N) if sequences
	// have the same elements in the same order.
	for (; __first1 != __last1 && __first2 != __last2;
	     ++__first1, (void)++__first2)
	  if (!std::__invoke(__pred,
			     std::__invoke(__proj1, *__first1),
			     std::__invoke(__proj2, *__first2)))
	      break;

	if constexpr (__sized_iters)
	  {
	    if (__first1 == __last1)
	      return true;
	  }
	else
	  {
	    auto __d1 = ranges::distance(__first1, __last1);
	    auto __d2 = ranges::distance(__first2, __last2);
	    if (__d1 == 0 && __d2 == 0)
	      return true;
	    if (__d1 != __d2)
	      return false;
	  }

	for (auto __scan = __first1; __scan != __last1; ++__scan)
	  {
	    auto __proj_scan = std::__invoke(__proj1, *__scan);
	    auto __comp_scan = [&] <typename _Tp> (_Tp&& __arg) {
	      return std::__invoke(__pred, __proj_scan,
				   std::forward<_Tp>(__arg));
	    };
	    if (__scan != ranges::find_if(__first1, __scan,
					  __comp_scan, __proj1))
	      continue; // We've seen this one before.

	    auto __matches = ranges::count_if(__first2, __last2,
					      __comp_scan, __proj2);
	    if (__matches == 0
		|| ranges::count_if(__scan, __last1,
				    __comp_scan, __proj1) != __matches)
	      return false;
	  }
	return true;
      }

    template<forward_range _Range1, forward_range _Range2,
	     typename _Proj1 = identity, typename _Proj2 = identity,
	     indirect_equivalence_relation<
	       projected<iterator_t<_Range1>, _Proj1>,
	       projected<iterator_t<_Range2>, _Proj2>> _Pred = ranges::equal_to>
      constexpr bool
      is_permutation(_Range1&& __r1, _Range2&& __r2, _Pred __pred = {},
		     _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
      {
	return ranges::is_permutation(ranges::begin(__r1), ranges::end(__r1),
				      ranges::begin(__r2), ranges::end(__r2),
				      std::move(__pred),
				      std::move(__proj1), std::move(__proj2));
      }

    template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	     input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	     typename _Pred, typename _Proj1, typename _Proj2>
      requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
      constexpr bool
      __equal(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	      _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
      {
	// TODO: implement more specializations to at least have parity with
	// std::equal.
	constexpr bool __sized_iters
	  = (sized_sentinel_for<_Sent1, _Iter1>
	     && sized_sentinel_for<_Sent2, _Iter2>);
	if constexpr (__sized_iters)
	  {
	    auto __d1 = std::distance(__first1, __last1);
	    auto __d2 = std::distance(__first2, __last2);
	    if (__d1 != __d2)
	      return false;

	    using _ValueType1 = iterator_traits<_Iter1>::value_type;
	    using _ValueType2 = iterator_traits<_Iter2>::value_type;
	    constexpr bool __use_memcmp
	      = ((is_integral_v<_ValueType1> || is_pointer_v<_ValueType1>)
		 && is_same_v<_ValueType1, _ValueType2>
		 && is_pointer_v<_Iter1>
		 && is_pointer_v<_Iter2>
		 && is_same_v<_Pred, ranges::equal_to>
		 && is_same_v<_Proj1, identity>
		 && is_same_v<_Proj2, identity>);
	    if constexpr (__use_memcmp)
	      {
		if (const size_t __len = (__last1 - __first1))
		  return !std::__memcmp(__first1, __first2, __len);
		return true;
	      }
	    else
	      {
		for (; __first1 != __last1; ++__first1, (void)++__first2)
		  if (!std::__invoke(__pred,
				     std::__invoke(__proj1, *__first1),
				     std::__invoke(__proj2, *__first2)))
		    return false;
		return true;
	      }
	  }
	else
	  {
	    for (; __first1 != __last1 && __first2 != __last2;
		 ++__first1, (void)++__first2)
	      if (!std::__invoke(__pred,
				 std::__invoke(__proj1, *__first1),
				 std::__invoke(__proj2, *__first2)))
		return false;
	    return __first1 == __last1 && __first2 == __last2;
	  }
      }

    template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	     input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	     typename _Pred = ranges::equal_to,
	     typename _Proj1 = identity, typename _Proj2 = identity>
      requires indirectly_comparable<_Iter1, _Iter2, _Pred, _Proj1, _Proj2>
      constexpr bool
      equal(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
	    _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
      {
	return ranges::__equal(std::__niter_base(__first1),
			       std::__niter_base(__last1),
			       std::__niter_base(__first2),
			       std::__niter_base(__last2),
			       std::move(__pred),
			       std::move(__proj1), std::move(__proj2));
      }

    template<input_range _Range1, input_range _Range2,
	     typename _Pred = ranges::equal_to,
	     typename _Proj1 = identity, typename _Proj2 = identity>
      requires indirectly_comparable<iterator_t<_Range1>, iterator_t<_Range2>,
				     _Pred, _Proj1, _Proj2>
      constexpr bool
      equal(_Range1&& __r1, _Range2&& __r2,
	    _Pred __pred = {}, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
      {
	return ranges::equal(ranges::begin(__r1), ranges::end(__r1),
			     ranges::begin(__r2), ranges::end(__r2),
			     std::move(__pred),
			     std::move(__proj1), std::move(__proj2));
      }

    template<typename _Iter, typename _Out>
    struct copy_result
    {
      [[no_unique_address]] _Iter in;
      [[no_unique_address]] _Out out;

      template<typename _Iter2, typename _Out2>
	requires convertible_to<const _Iter&, _Iter2>
	  && convertible_to<const _Out&, _Out2>
	operator copy_result<_Iter2, _Out2>() const &
	{
	  return {in, out};
	}

      template<typename _Iter2, typename _Out2>
	requires convertible_to<_Iter, _Iter2>
	  && convertible_to<_Out, _Out2>
	operator copy_result<_Iter2, _Out2>() &&
	{
	  return {std::move(in), std::move(out)};
	}
    };

    template<typename _Iter, typename _Out>
    using move_result = copy_result<_Iter, _Out>;

    template<bool _IsMove,
	     input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     weakly_incrementable _Out>
      requires (_IsMove
		? indirectly_movable<_Iter, _Out>
		: indirectly_copyable<_Iter, _Out>)
      constexpr conditional_t<_IsMove,
			      move_result<_Iter, _Out>,
			      copy_result<_Iter, _Out>>
      __copy_or_move(_Iter __first, _Sent __last, _Out __result)
      {
	// TODO: implement more specializations to be at least on par with
	// std::copy/std::move.
	if constexpr (sized_sentinel_for<_Sent, _Iter>)
	  {
	    using _ValueTypeI = iterator_traits<_Iter>::value_type;
	    using _ValueTypeO = iterator_traits<_Out>::value_type;
	    constexpr bool __use_memmove
	      = (is_trivially_copyable_v<_ValueTypeI>
		 && is_same_v<_ValueTypeI, _ValueTypeO>
		 && is_pointer_v<_Iter>
		 && is_pointer_v<_Out>);

	    if constexpr (__use_memmove)
	      {
		static_assert(_IsMove
			      ? is_move_assignable_v<_ValueTypeI>
			      : is_copy_assignable_v<_ValueTypeI>);
		auto __num = __last - __first;
		if (__num)
		  std::__memmove<_IsMove>(__result, __first, __num);
		return {__first + __num, __result + __num};
	      }
	    else
	      {
		for (auto __n = __last - __first; __n > 0; --__n)
		  {
		    if constexpr (_IsMove)
		      *__result = std::move(*__first);
		    else
		      *__result = *__first;
		    __first++;
		    __result++;
		  }
		return {__first, __result};
	      }
	  }
	else
	  {
	    while (__first != __last)
	      {
		if constexpr (_IsMove)
		  *__result = std::move(*__first);
		else
		  *__result = *__first;
		__first++;
		__result++;
	      }
	    return {__first, __result};
	  }
      }

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     weakly_incrementable _Out>
      requires indirectly_copyable<_Iter, _Out>
      constexpr copy_result<_Iter, _Out>
      copy(_Iter __first, _Sent __last, _Out __result)
      {
	constexpr bool __move_iterator_p = __is_move_iterator<_Iter>::__value;
	if constexpr (__move_iterator_p)
	  {
	    auto __first_base = __first.base();
	    auto __last_base = __last.base();
	    auto [__in,__out]
	      = ranges::__copy_or_move<true>(std::__niter_base(__first_base),
					     std::__niter_base(__last_base),
					     std::__niter_base(__result));
	    auto __wrapped_in = std::__niter_wrap(__first_base, __in);
	    auto __wrapped_out = std::__niter_wrap(__result, __out);
	    return {move_iterator{__wrapped_in}, __wrapped_out};
	  }
	else
	  {
	    auto [__in,__out]
	      = ranges::__copy_or_move<false>(std::__niter_base(__first),
					      std::__niter_base(__last),
					      std::__niter_base(__result));
	    return {std::__niter_wrap(__first, __in),
		    std::__niter_wrap(__result, __out)};
	  }
      }

    template<input_range _Range, weakly_incrementable _Out>
      requires indirectly_copyable<iterator_t<_Range>, _Out>
      constexpr copy_result<safe_iterator_t<_Range>, _Out>
      copy(_Range&& __r, _Out __result)
      {
	return ranges::copy(ranges::begin(__r), ranges::end(__r),
			    std::move(__result));
      }

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     weakly_incrementable _Out>
      requires indirectly_movable<_Iter, _Out>
      constexpr move_result<_Iter, _Out>
      move(_Iter __first, _Sent __last, _Out __result)
      {
	if constexpr (__is_move_iterator<_Iter>::__value)
	  return ranges::copy(__first, __last, __result);
	else
	  {
	    auto [__in, __out]
	      = ranges::copy(move_iterator<_Iter>{__first},
			     move_sentinel<_Sent>{__last}, __result);
	    return {__in.base(), __out};
	  }
      }

    template<input_range _Range, weakly_incrementable _Out>
      requires indirectly_movable<iterator_t<_Range>, _Out>
      constexpr move_result<safe_iterator_t<_Range>, _Out>
      move(_Range&& __r, _Out __result)
      {
	return ranges::move(ranges::begin(__r), ranges::end(__r),
			    std::move(__result));
      }

    template<typename _Iter1, typename _Iter2>
    using swap_ranges_result = mismatch_result<_Iter1, _Iter2>;

    template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	     input_iterator _Iter2, sentinel_for<_Iter2> _Sent2>
      requires indirectly_swappable<_Iter1, _Iter2>
      constexpr swap_ranges_result<_Iter1, _Iter2>
      swap_ranges(_Iter1 __first1, _Sent1 __last1,
		  _Iter2 __first2, _Sent2 __last2)
      {
	for (; __first1 != __last1 && __first2 != __last2;
	     ++__first1, (void)++__first2)
	  ranges::iter_swap(__first1, __first2);
	return {__first1, __first2};
      }

    template<input_range _Range1, input_range _Range2>
      requires indirectly_swappable<iterator_t<_Range1>, iterator_t<_Range2>>
      constexpr swap_ranges_result<safe_iterator_t<_Range1>,
				   safe_iterator_t<_Range2>>
      swap_ranges(_Range1&& __r1, _Range2&& __r2)
      {
	return ranges::swap_ranges(ranges::begin(__r1), ranges::end(__r1),
				   ranges::begin(__r2), ranges::end(__r2));
      }

    template<typename _Iter, typename _Out>
    using unary_transform_result = copy_result<_Iter, _Out>;

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     weakly_incrementable _Out,
	     copy_constructible _F, typename _Proj = identity>
      requires writable<_Out, indirect_result_t<_F&, projected<_Iter, _Proj>>>
      constexpr unary_transform_result<_Iter, _Out>
      transform(_Iter __first1, _Sent __last1, _Out __result,
		_F __op, _Proj __proj = {})
      {
	for (; __first1 != __last1; ++__first1, (void)++__result)
	  *__result = std::__invoke(__op, std::__invoke(__proj, *__first1));
	return {__first1, __result};
      }

    template<input_range _Range, weakly_incrementable _Out,
	     copy_constructible _F, typename _Proj = identity>
      requires writable<_Out,
			indirect_result_t<_F&, projected<iterator_t<_Range>,
							_Proj>>>
      constexpr unary_transform_result<safe_iterator_t<_Range>, _Out>
      transform(_Range&& __r, _Out __result, _F __op, _Proj __proj = {})
      {
	return ranges::transform(ranges::begin(__r), ranges::end(__r),
				 std::move(__result),
				 std::move(__op), std::move(__proj));
      }

    template<typename _Iter1, typename _Iter2, typename _Out>
    struct binary_transform_result {
      [[no_unique_address]] _Iter1 in1;
      [[no_unique_address]] _Iter2 in2;
      [[no_unique_address]] _Out  out;

      template<typename _IIter1, typename _IIter2, typename _OOut>
	requires convertible_to<const _Iter1&, _IIter1> &&
	  && convertible_to<const _Iter2&, _IIter2>
	  && convertible_to<const _Out&, _OOut>
	operator binary_transform_result<_IIter1, _IIter2, _OOut>() const & {
	  return {in1, in2, out};
	}

      template<typename _IIter1, typename _IIter2, typename _OOut>
	requires convertible_to<_Iter1, _IIter1>
	  && convertible_to<_Iter2, _IIter2>
	  && convertible_to<_Out, _OOut>
	operator binary_transform_result<_IIter1, _IIter2, _OOut>() && {
	  return {std::move(in1), std::move(in2), std::move(out)};
	}
    };

    template<input_iterator _Iter1, sentinel_for<_Iter1> _Sent1,
	     input_iterator _Iter2, sentinel_for<_Iter2> _Sent2,
	     weakly_incrementable _Out, copy_constructible _F,
	     typename _Proj1 = identity, typename _Proj2 = identity>
      requires writable<_Out, indirect_result_t<_F&, projected<_Iter1, _Proj1>,
					     projected<_Iter2, _Proj2>>>
      constexpr binary_transform_result<_Iter1, _Iter2, _Out>
      transform(_Iter1 __first1, _Sent1 __last1, _Iter2 __first2, _Sent2 __last2,
		_Out __result, _F __binary_op,
		_Proj1 __proj1 = {}, _Proj2 __proj2 = {})
      {
	for (; __first1 != __last1 && __first2 != __last2;
	     ++__first1, (void)++__first2, ++__result)
	  *__result = std::__invoke(__binary_op,
				    std::__invoke(__proj1, *__first1),
				    std::__invoke(__proj2, *__first2));
	return {__first1, __first2, __result};
      }

    template<input_range _Range1, input_range _Range2,
	     weakly_incrementable _Out, copy_constructible _F,
	     typename _Proj1 = identity, typename _Proj2 = identity>
      requires writable<_Out, indirect_result_t<_F&,
						projected<iterator_t<_Range1>,
							  _Proj1>,
						projected<iterator_t<_Range2>,
							  _Proj2>>>
      constexpr binary_transform_result<safe_iterator_t<_Range1>,
					safe_iterator_t<_Range2>, _Out>
      transform(_Range1&& __r1, _Range2&& __r2, _Out __result,
		_F __binary_op, _Proj1 __proj1 = {}, _Proj2 __proj2 = {})
      {
	return ranges::transform(ranges::begin(__r1), ranges::end(__r1),
				 ranges::begin(__r2), ranges::end(__r2),
				 std::move(__result), std::move(__binary_op),
				 std::move(__proj1), std::move(__proj2));
      }

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     typename _Tp1, typename _Tp2, typename _Proj = identity>
      requires writable<_Iter, const _Tp2&> &&
	       indirect_binary_predicate<ranges::equal_to,
					 projected<_Iter, _Proj>, const _Tp1*>
      constexpr _Iter
      replace(_Iter __first, _Sent __last,
	      const _Tp1& __old_value, const _Tp2& __new_value,
	      _Proj __proj = {})
      {
	for (; __first != __last; ++__first)
	  if (std::__invoke(__proj, *__first) == __old_value)
	    *__first = __new_value;
	return __first;
      }

    template<input_range _Range,
	     typename _Tp1, typename _Tp2, typename _Proj = identity>
      requires writable<iterator_t<_Range>, const _Tp2&> &&
	       indirect_binary_predicate<ranges::equal_to,
					 projected<iterator_t<_Range>, _Proj>,
						   const _Tp1*>
      constexpr safe_iterator_t<_Range>
      replace(_Range&& __r,
	      const _Tp1& __old_value, const _Tp2& __new_value,
	      _Proj __proj = {})
      {
	return ranges::replace(ranges::begin(__r), ranges::end(__r),
			       __old_value, __new_value, std::move(__proj));
      }

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     typename _Tp, typename _Proj = identity,
	     indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
      requires writable<_Iter, const _Tp&>
      constexpr _Iter
      replace_if(_Iter __first, _Sent __last,
		 _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
      {
	for (; __first != __last; ++__first)
	  if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	    *__first = __new_value;
	return __first;
      }

    template<input_range _Range, typename _Tp, typename _Proj = identity,
	     indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
      requires writable<iterator_t<_Range>, const _Tp&>
      constexpr safe_iterator_t<_Range>
      replace_if(_Range&& __r,
		 _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
      {
	return ranges::replace_if(ranges::begin(__r), ranges::end(__r),
				  std::move(__pred), __new_value,
				  std::move(__proj));
      }

    template<typename _Iter, typename _Out>
    using replace_copy_result = copy_result<_Iter, _Out>;

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     typename _Tp1, typename _Tp2, output_iterator<const _Tp2&> _Out,
	     typename _Proj = identity>
      requires indirectly_copyable<_Iter, _Out>
	&& indirect_binary_predicate<ranges::equal_to,
				     projected<_Iter, _Proj>, const _Tp1*>
      constexpr replace_copy_result<_Iter, _Out>
      replace_copy(_Iter __first, _Sent __last, _Out __result,
		   const _Tp1& __old_value, const _Tp2& __new_value,
		   _Proj __proj = {})
      {
	for (; __first != __last; ++__first, (void)++__result)
	  if (std::__invoke(__proj, *__first) == __old_value)
	    *__result = __new_value;
	  else
	    *__result = *__first;
	return {__first, __result};
      }

    template<input_range _Range, typename _Tp1, typename _Tp2,
	     output_iterator<const _Tp2&> _Out, typename _Proj = identity>
      requires indirectly_copyable<iterator_t<_Range>, _Out>
	&& indirect_binary_predicate<ranges::equal_to,
				     projected<iterator_t<_Range>, _Proj>,
				     const _Tp1*>
      constexpr replace_copy_result<safe_iterator_t<_Range>, _Out>
      replace_copy(_Range&& __r, _Out __result,
		   const _Tp1& __old_value, const _Tp2& __new_value,
		   _Proj __proj = {})
      {
	return ranges::replace_copy(ranges::begin(__r), ranges::end(__r),
				    std::move(__result), __old_value,
				    __new_value, std::move(__proj));
      }

    template<typename _Iter, typename _Out>
    using replace_copy_if_result = copy_result<_Iter, _Out>;

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     typename _Tp, output_iterator<const _Tp&> _Out,
	     typename _Proj = identity,
	     indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
      requires indirectly_copyable<_Iter, _Out>
      constexpr replace_copy_if_result<_Iter, _Out>
      replace_copy_if(_Iter __first, _Sent __last, _Out __result,
		      _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
      {
	for (; __first != __last; ++__first, (void)++__result)
	  if (std::__invoke(__pred, std::__invoke(__proj, *__first)))
	    *__result = __new_value;
	  else
	    *__result = *__first;
	return {__first, __result};
      }

    template<input_range _Range,
	     typename _Tp, output_iterator<const _Tp&> _Out,
	     typename _Proj = identity,
	     indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
      requires indirectly_copyable<iterator_t<_Range>, _Out>
      constexpr replace_copy_if_result<safe_iterator_t<_Range>, _Out>
      replace_copy_if(_Range&& __r, _Out __result,
		      _Pred __pred, const _Tp& __new_value, _Proj __proj = {})
      {
	return ranges::replace_copy_if(ranges::begin(__r), ranges::end(__r),
				       std::move(__result), std::move(__pred),
				       __new_value, std::move(__proj));
      }

    template<typename _Tp, output_iterator<const _Tp&> _Out>
      constexpr _Out
      fill_n(_Out __first, iter_difference_t<_Out> __n, const _Tp& __value)
      {
	// TODO: implement more specializations to be at least on par with
	// std::fill_n
	if (__n <= 0)
	  return __first;

	if constexpr (is_pointer_v<_Out> && __is_byte<_Tp>::__value)
	  {
	    __builtin_memset(__first, static_cast<unsigned char>(__value), __n);
	    return __first + __n;
	  }
	else if constexpr (is_scalar_v<_Tp>)
	  {
	    const auto __tmp = __value;
	    for (; __n > 0; --__n, (void)++__first)
	      *__first = __tmp;
	    return __first;
	  }
	else
	  {
	    for (; __n > 0; --__n, (void)++__first)
	      *__first = __value;
	    return __first;
	  }
      }

    template<typename _Tp,
	     output_iterator<const _Tp&> _Out, sentinel_for<_Out> _Sent>
      constexpr _Out
      fill(_Out __first, _Sent __last, const _Tp& __value)
      {
	// TODO: implement more specializations to be at least on par with
	// std::fill
	if constexpr (sized_sentinel_for<_Sent, _Out>)
	  {
	    const auto __len = __last - __first;
	    return ranges::fill_n(__first, __len, __value);
	  }
	else if constexpr (is_scalar_v<_Tp>)
	  {
	    const auto __tmp = __value;
	    for (; __first != __last; ++__first)
	      *__first = __tmp;
	    return __first;
	  }
	else
	  {
	    for (; __first != __last; ++__first)
	      *__first = __value;
	    return __first;
	  }
      }

    template<typename _Tp, output_range<const _Tp&> _Range>
      constexpr safe_iterator_t<_Range>
      fill(_Range&& __r, const _Tp& __value)
      {
	return ranges::fill(ranges::begin(__r), ranges::end(__r), __value);
      }

    template<input_or_output_iterator _Out, copy_constructible _F>
      requires invocable<_F&> && writable<_Out, invoke_result_t<_F&>>
      constexpr _Out
      generate_n(_Out __first, iter_difference_t<_Out> __n, _F __gen)
      {
	for (; __n > 0; --__n, (void)++__first)
	  *__first = std::__invoke(__gen);
	return __first;
      }

    template<input_or_output_iterator _Out, sentinel_for<_Out> _Sent,
	     copy_constructible _F>
      requires invocable<_F&> && writable<_Out, invoke_result_t<_F&>>
      constexpr _Out
      generate(_Out __first, _Sent __last, _F __gen)
      {
	for (; __first != __last; ++__first)
	  *__first = std::__invoke(__gen);
	return __first;
      }

    template<typename _Range, copy_constructible _F>
      requires invocable<_F&> && output_range<_Range, invoke_result_t<_F&>>
      constexpr safe_iterator_t<_Range>
      generate(_Range&& __r, _F __gen)
      {
	return ranges::generate(ranges::begin(__r), ranges::end(__r),
				std::move(__gen));
      }

    template<permutable _Iter, sentinel_for<_Iter> _Sent, class _Proj = identity,
	     indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
      constexpr subrange<_Iter>
      remove_if(_Iter __first, _Sent __last, _Pred __pred, _Proj __proj = {})
      {
	__first = ranges::find_if(__first, __last, __pred, __proj);
	if (__first == __last)
	  return {__first, __first};

	auto __result = __first;
	++__first;
	for (; __first != __last; ++__first)
	  if (!std::__invoke(__pred, std::__invoke(__proj, *__first)))
	    {
	      *__result = std::move(*__first);
	      ++__result;
	    }

	return {__result, __first};
      }

    template<forward_range _Range, class _Proj = identity,
	     indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
      requires permutable<iterator_t<_Range>>
      constexpr safe_subrange_t<_Range>
      remove_if(_Range&& __r, _Pred __pred, _Proj __proj = {})
      {
	return ranges::remove_if(ranges::begin(__r), ranges::end(__r),
				 std::move(__pred), std::move(__proj));
      }

    template<permutable _Iter, sentinel_for<_Iter> _Sent,
	     class _Tp, class _Proj = identity>
      requires indirect_binary_predicate<ranges::equal_to,
					 projected<_Iter, _Proj>,
					 const _Tp*>
      constexpr subrange<_Iter>
      remove(_Iter __first, _Sent __last, const _Tp& __value, _Proj __proj = {})
      {
	auto __pred = [&] (auto&& __arg) {
	  return std::forward<decltype(__arg)>(__arg) == __value;
	};
	return ranges::remove_if(__first, __last,
				 std::move(__pred), std::move(__proj));
      }

    template<forward_range _Range, class _Tp, class _Proj = identity>
      requires permutable<iterator_t<_Range>> &&
	       indirect_binary_predicate<ranges::equal_to,
					 projected<iterator_t<_Range>, _Proj>,
					 const _Tp*>
      constexpr safe_subrange_t<_Range>
      remove(_Range&& __r, const _Tp& __value, _Proj __proj = {})
      {
	return ranges::remove(ranges::begin(__r), ranges::end(__r),
			      __value, std::move(__proj));
      }

    template<class _Iter, class _Out>
    using remove_copy_if_result = copy_result<_Iter, _Out>;

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     weakly_incrementable _Out, class _Proj = identity,
	     indirect_unary_predicate<projected<_Iter, _Proj>> _Pred>
      requires indirectly_copyable<_Iter, _Out>
      constexpr remove_copy_if_result<_Iter, _Out>
      remove_copy_if(_Iter __first, _Sent __last, _Out __result,
		     _Pred __pred, _Proj __proj = {})
      {
	for (; __first != __last; ++__first)
	  if (!std::__invoke(__pred, std::__invoke(__proj, *__first)))
	    {
	      *__result = *__first;
	      ++__result;
	    }
	return {__first, __result};
      }

    template<input_range _Range, weakly_incrementable _Out,
	     class _Proj = identity,
	     indirect_unary_predicate<projected<iterator_t<_Range>, _Proj>> _Pred>
      requires indirectly_copyable<iterator_t<_Range>, _Out>
      constexpr remove_copy_if_result<safe_iterator_t<_Range>, _Out>
      remove_copy_if(_Range&& __r, _Out __result,
		     _Pred __pred, _Proj __proj = {})
      {
	return ranges::remove_copy_if(ranges::begin(__r), ranges::end(__r),
				      __result,
				      std::move(__pred), std::move(__proj));
      }

    template<class _Iter, class _Out>
    using remove_copy_result = copy_result<_Iter, _Out>;

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     weakly_incrementable _Out, class _Tp, class _Proj = identity>
      requires indirectly_copyable<_Iter, _Out>
	&& indirect_binary_predicate<ranges::equal_to,
				     projected<_Iter, _Proj>,
				     const _Tp*>
      constexpr remove_copy_result<_Iter, _Out>
      remove_copy(_Iter __first, _Sent __last, _Out __result,
		  const _Tp& __value, _Proj __proj = {})
      {
	for (; __first != __last; ++__first)
	  if (!(std::__invoke(__proj, *__first) == __value))
	    {
	      *__result = *__first;
	      ++__result;
	    }
	return {__first, __result};
      }

    template<input_range _Range, weakly_incrementable _Out,
	     class _Tp, class _Proj = identity>
      requires indirectly_copyable<iterator_t<_Range>, _Out>
	&& indirect_binary_predicate<ranges::equal_to,
				     projected<iterator_t<_Range>, _Proj>,
				     const _Tp*>
      constexpr remove_copy_result<safe_iterator_t<_Range>, _Out>
      remove_copy(_Range&& __r, _Out __result,
		  const _Tp& __value, _Proj __proj = {})
      {
	return ranges::remove_copy(ranges::begin(__r), ranges::end(__r),
				   __result, __value, std::move(__proj));

      }

    template<permutable _Iter, sentinel_for<_Iter> _Sent, class _Proj = identity,
	     indirect_equivalence_relation<
	       projected<_Iter, _Proj>> _Comp = ranges::equal_to>
      constexpr subrange<_Iter>
      unique(_Iter __first, _Sent __last, _Comp __comp = {}, _Proj __proj = {})
      {
	__first = ranges::adjacent_find(__first, __last, __comp, __proj);
	if (__first == __last)
	  return {__first, __first};

	auto __dest = __first;
	++__first;
	while (++__first != __last)
	  if (!std::__invoke(__comp,
			     std::__invoke(__proj, *__dest),
			     std::__invoke(__proj, *__first)))
	    *++__dest = std::move(*__first);
	return {++__dest, __first};
      }

    template<forward_range _Range, class _Proj = identity,
	     indirect_equivalence_relation<
	       projected<iterator_t<_Range>, _Proj>> _Comp = ranges::equal_to>
      requires permutable<iterator_t<_Range>>
      constexpr safe_subrange_t<_Range>
      unique(_Range&& __r, _Comp __comp = {}, _Proj __proj = {})
      {
	return ranges::unique(ranges::begin(__r), ranges::end(__r),
			      std::move(__comp), std::move(__proj));
      }

    template<class _Iter, class _Out>
    using unique_copy_result = copy_result<_Iter, _Out>;

    template<input_iterator _Iter, sentinel_for<_Iter> _Sent,
	     weakly_incrementable _Out, class _Proj = identity,
	     indirect_equivalence_relation<
	       projected<_Iter, _Proj>> _Comp = ranges::equal_to>
      requires indirectly_copyable<_Iter, _Out>
	&& (forward_iterator<_Iter>
	    || (input_iterator<_Out>
		&& same_as<iter_value_t<_Iter>, iter_value_t<_Out>>)
	    || indirectly_copyable_storable<_Iter, _Out>)
      constexpr unique_copy_result<_Iter, _Out>
      unique_copy(_Iter __first, _Sent __last, _Out __result,
		  _Comp __comp = {}, _Proj __proj = {})
      {
	if (__first == __last)
	  return {__first, __result};

	if constexpr (forward_iterator<_Iter>)
	  {
	    auto __next = __first;
	    *__result = *__next;
	    while (++__next != __last)
	      if (!std::__invoke(__comp,
				 std::__invoke(__proj, *__first),
				 std::__invoke(__proj, *__next)))
		{
		  __first = __next;
		  *++__result = *__first;
		}
	    return {__next, ++__result};
	  }
	else if constexpr (input_iterator<_Out>
			   && same_as<iter_value_t<_Iter>, iter_value_t<_Out>>)
	  {
	    *__result = *__first;
	    while (++__first != __last)
	      if (!std::__invoke(__comp,
				 std::__invoke(__proj, *__result),
				 std::__invoke(__proj, *__first)))
		  *++__result = *__first;
	    return {__first, ++__result};
	  }
	else // indirectly_copyable_storable<_Iter, _Out>
	  {
	    auto __value = *__first;
	    *__result = __value;
	    while (++__first != __last)
	      {
		if (!std::__invoke(__comp,
				   std::__invoke(__proj, *__first),
				   std::__invoke(__proj, __value)))
		  {
		    __value = *__first;
		    *++__result = __value;
		  }
	      }
	    return {__first, ++__result};
	  }
      }

    template<input_range _Range,
	     weakly_incrementable _Out, class _Proj = identity,
	     indirect_equivalence_relation<
	       projected<iterator_t<_Range>, _Proj>> _Comp = ranges::equal_to>
      requires indirectly_copyable<iterator_t<_Range>, _Out>
	&& (forward_iterator<iterator_t<_Range>>
	    || (input_iterator<_Out>
		&& same_as<range_value_t<_Range>, iter_value_t<_Out>>)
	    || indirectly_copyable_storable<iterator_t<_Range>, _Out>)
      constexpr unique_copy_result<safe_iterator_t<_Range>, _Out>
      unique_copy(_Range&& __r, _Out __result,
		  _Comp __comp = {}, _Proj __proj = {})
      {
	return ranges::unique_copy(ranges::begin(__r), ranges::end(__r),
				   std::move(__result),
				   std::move(__comp), std::move(__proj));
      }

} // namespace ranges
_GLIBCXX_END_NAMESPACE_VERSION
} // namespace std
#endif // concepts
#endif // C++20
#endif // _RANGES_ALGO_H

