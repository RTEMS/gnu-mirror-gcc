/* Dynamic testing for abstract is-a relationships.
   Copyright (C) 2012-2025 Free Software Foundation, Inc.
   Contributed by Lawrence Crowl.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* This header generic type query and conversion functions.


USING THE GENERIC TYPE FACILITY


The user functions are:

bool is_a <TYPE> (pointer)

    Tests whether the pointer actually points to a more derived TYPE.

    Suppose you have a symtab_node *ptr, AKA symtab_node *ptr.  You can test
    whether it points to a 'derived' cgraph_node as follows.

      if (is_a <cgraph_node *> (ptr))
        ....


TYPE as_a <TYPE> (pointer)

    Converts pointer to a TYPE.

    You can just assume that it is such a node.

      do_something_with (as_a <cgraph_node *> *ptr);

TYPE safe_as_a <TYPE> (pointer)

    Like as_a <TYPE> (pointer), but where pointer could be NULL.  This
    adds a check against NULL where the regular is_a_helper hook for TYPE
    assumes non-NULL.

      do_something_with (safe_as_a <cgraph_node *> *ptr);

TYPE dyn_cast <TYPE> (pointer)

    Converts pointer to TYPE if and only if "is_a <TYPE> pointer".  Otherwise,
    returns NULL.  This function is essentially a checked down cast.

    This functions reduce compile time and increase type safety when treating a
    generic item as a more specific item.

    You can test and obtain a pointer to the 'derived' type in one indivisible
    operation.

      if (cgraph_node *cptr = dyn_cast <cgraph_node *> (ptr))
        ....

    As an example, the code change is from

      if (symtab_function_p (node))
        {
          struct cgraph_node *cnode = cgraph (node);
          ....
        }

    to

      if (cgraph_node *cnode = dyn_cast <cgraph_node *> (node))
        {
          ....
        }

    The necessary conditional test defines a variable that holds a known good
    pointer to the specific item and avoids subsequent conversion calls and
    the assertion checks that may come with them.

    When, the property test is embedded within a larger condition, the
    variable declaration gets pulled out of the condition.  (This approach
    leaves some room for using the variable inappropriately.)

      if (symtab_variable_p (node) && varpool (node)->finalized)
        varpool_analyze_node (varpool (node));

    becomes

      varpool_node *vnode = dyn_cast <varpool_node *> (node);
      if (vnode && vnode->finalized)
        varpool_analyze_node (vnode);

    Note that we have converted two sets of assertions in the calls to varpool
    into safe and efficient use of a variable.

TYPE safe_dyn_cast <TYPE> (pointer)

    Like dyn_cast <TYPE> (pointer), except that it accepts null pointers
    and returns null results for them.


If you use these functions and get a 'inline function not defined' or a
'missing symbol' error message for 'is_a_helper<....>::test', it means that
the connection between the types has not been made.  See below.


EXTENDING THE GENERIC TYPE FACILITY

Method 1
--------

If DERIVED is derived from BASE, and if BASE contains enough information
to determine whether an object is actually an instance of DERIVED,
then you can make the above routines work for DERIVED by defining
a specialization of is_a_helper such as:

  template<>
  struct is_a_helper<DERIVED *> : static_is_a_helper<DERIVED *>
  {
    static inline bool test (const BASE *p) { return ...; }
  };

This test function should return true if P is an instanced of DERIVED.
This on its own is enough; the comments below for method 2 do not apply.

Method 2
--------

Alternatively, if two types are connected in ways other than C++
inheritance, each connection between them must be made by defining a
specialization of the template member function 'test' of the template
class 'is_a_helper'.  For example,

  template <>
  template <>
  inline bool
  is_a_helper <cgraph_node *>::test (symtab_node *p)
  {
    return p->type == SYMTAB_FUNCTION;
  }

If a simple reinterpret_cast between the pointer types is incorrect, then you
must also specialize the template member function 'cast'.  Failure to do so
when needed may result in a crash.  For example,

  template <>
  template <>
  inline bool
  is_a_helper <cgraph_node *>::cast (symtab_node *p)
  {
    return &p->x_function;
  }

*/

#ifndef GCC_IS_A_H
#define GCC_IS_A_H

/* A base class that specializations of is_a_helper can use if casting
   U * to T is simply a reinterpret_cast.  */

template <typename T>
struct reinterpret_is_a_helper
{
  template <typename U>
  static inline T cast (U *p) { return reinterpret_cast <T> (p); }
};

/* A base class that specializations of is_a_helper can use if casting
   U * to T is simply a static_cast.  This is more type-safe than
   reinterpret_is_a_helper.  */

template <typename T>
struct static_is_a_helper
{
  template <typename U>
  static inline T cast (U *p) { return static_cast <T> (p); }
};

/* A generic type conversion internal helper class.  */

template <typename T>
struct is_a_helper : reinterpret_is_a_helper<T>
{
  template <typename U>
  static inline bool test (U *p);
};

/* Reuse the definition of is_a_helper<T *> to implement
   is_a_helper<const T *>.  */

template <typename T>
struct is_a_helper<const T *>
{
  template <typename U>
  static inline const T *cast (const U *p)
  {
    return is_a_helper<T *>::cast (const_cast <U *> (p));
  }
  template <typename U>
  static inline bool test (const U *p)
  {
    return is_a_helper<T *>::test (p);
  }
};

/* Note that we deliberately do not define the 'test' member template.  Not
   doing so will result in a build-time error for type relationships that have
   not been defined, rather than a run-time error.  See the discussion above
   for when to define this member.  */

/* The public interface.  */

/* A generic test for a type relationship.  See the discussion above for when
   to use this function.  The question answered is "Is type T a derived type of
   type U?".  */

template <typename T, typename U>
inline bool
is_a (U *p)
{
  return is_a_helper<T>::test (p);
}

/* Similar to is_a<>, but where the pointer can be NULL, even if
   is_a_helper<T> doesn't check for NULL.  */

template <typename T, typename U>
inline bool
safe_is_a (U *p)
{
  if (p)
    return is_a_helper <T>::test (p);
  else
    return false;
}

/* A generic conversion from a base type U to a derived type T.  See the
   discussion above for when to use this function.  */

template <typename T, typename U>
inline T
as_a (U *p)
{
  gcc_checking_assert (is_a <T> (p));
  return is_a_helper <T>::cast (p);
}

/* Similar to as_a<>, but where the pointer can be NULL, even if
   is_a_helper<T> doesn't check for NULL.  */

template <typename T, typename U>
inline T
safe_as_a (U *p)
{
  if (p)
    {
      gcc_checking_assert (is_a <T> (p));
      return is_a_helper <T>::cast (p);
    }
  else
    return NULL;
}

/* A generic checked conversion from a base type U to a derived type T.  See
   the discussion above for when to use this function.  */

template <typename T, typename U>
inline T
dyn_cast (U *p)
{
  if (is_a <T> (p))
    return is_a_helper <T>::cast (p);
  else
    return static_cast <T> (0);
}

/* Similar to dyn_cast, except that the pointer may be null.  */

template <typename T, typename U>
inline T
safe_dyn_cast (U *p)
{
  return p ? dyn_cast <T> (p) : 0;
}

#endif  /* GCC_IS_A_H  */
