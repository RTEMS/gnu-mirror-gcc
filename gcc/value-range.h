/* Support routines for value ranges.
   Copyright (C) 2019-2020 Free Software Foundation, Inc.
   Contributed by Aldy Hernandez <aldyh@redhat.com>.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef GCC_VALUE_RANGE_H
#define GCC_VALUE_RANGE_H

// Types of value ranges.
enum value_range_kind
{
  /* Empty range.  */
  VR_UNDEFINED,
  /* Range spans the entire domain.  */
  VR_VARYING,
  /* Range is [MIN, MAX].  */
  VR_RANGE,
  /* Range is ~[MIN, MAX].  */
  VR_ANTI_RANGE,
  /* Range is a nice guy.  */
  VR_LAST
};

// Helper for is_a<> to distinguish between irange and widest_irange.
// We could do this with virtuals, but this enum fits in the free bits
// of the irange.  This saves us from having to use virtuals, and the
// vtable pointer associated with them.

enum irange_discriminator
{
  IRANGE_KIND_UNKNOWN,
  IRANGE_KIND_INT,
  IRANGE_KIND_WIDEST_INT
};

// Range of values that can be associated with an SSA_NAME.
//
// This is the base class without any storage.

class irange_base
{
public:
  // In-place setters.
  void set (tree, tree, value_range_kind = VR_RANGE);
  void set_nonzero (tree);
  void set_zero (tree);
  void set_varying (tree type);
  void set_undefined ();

  // Iteration over sub-ranges.
  unsigned num_pairs () const;
  wide_int lower_bound (unsigned = 0) const;
  wide_int upper_bound (unsigned) const;
  wide_int upper_bound () const;

  // Range types.
  static bool supports_type_p (tree);
  tree type () const;

  // Predicates.
  bool undefined_p () const;
  bool varying_p () const;
  bool zero_p () const;
  bool nonzero_p () const;
  bool singleton_p (tree *result = NULL) const;
  bool contains_p (tree) const;

  // In-place operators.
  void union_ (const irange_base &);
  void intersect (const irange_base &);
  void invert ();

  // Operator overloads.
  irange_base& operator= (const irange_base &);
  bool operator== (const irange_base &) const;
  bool operator!= (const irange_base &r) const { return !(*this == r); }

  // Misc methods.
  void dump (FILE * = stderr) const;

protected:
  void check ();
  irange_base (tree *, unsigned);
  irange_base (tree *, unsigned, const irange_base &);

private:
  void intersect_from_wide_ints (const wide_int &, const wide_int &);
protected:
  void multi_range_union (const irange_base &);
  void multi_range_intersect (const irange_base &);
  bool swap_out_of_order_endpoints (tree &min, tree &max, value_range_kind &);
  bool normalize_min_max (tree type, tree min, tree max, value_range_kind);
  tree tree_lower_bound (unsigned = 0) const;
  tree tree_upper_bound (unsigned) const;
  tree tree_upper_bound () const;

public:
  bool equal_p (const irange_base &) const;

protected:
  unsigned char m_num_ranges;
  ENUM_BITFIELD(value_range_kind) m_kind : 8;
  unsigned char m_max_ranges;
public:
  ENUM_BITFIELD(irange_discriminator) m_discriminator : 8;
protected:
  tree *m_base;
};

// This is the legacy API for irange.  It is meant to be compatible
// with the legacy value_range code.  For now, irange is a typedef for
// irange_legacy.  Once this class is no longer needed, it can be
// removed and irange_base will become irange.

class irange_legacy : public irange_base
{
public:
  // Deprecated public methods that do not exist in the clean
  // implementation of irange_base.
  enum value_range_kind kind () const;
  tree min () const;
  tree max () const;
  bool symbolic_p () const;
  void normalize_symbolics ();
  void normalize_addresses ();
  bool may_contain_p (tree) const;

  // The rest of this class are overrides for irange_base methods
  // handling awkward VR_ANTI_RANGE type ranges.

  // In-place setters.
  void set (tree, tree, value_range_kind = VR_RANGE);
  void set (tree);
  void set_nonzero (tree);
  void set_zero (tree);
  void set_varying (tree type);
  void set_undefined ();

  // Iteration over sub-ranges.
  unsigned num_pairs () const;
  wide_int lower_bound (unsigned = 0) const;
  wide_int upper_bound (unsigned) const;
  wide_int upper_bound () const;

  // Predicates.
  bool undefined_p () const;
  bool varying_p () const;
  bool singleton_p (tree *result = NULL) const;
  bool contains_p (tree) const;
  bool constant_p () const;

  // In-place operators.
  void union_ (const class irange_legacy *);
  void intersect (const irange_legacy *);
  void union_ (const irange_legacy &);
  void intersect (const irange_legacy &);
  void invert ();

  // Operator overloads.
  irange_legacy& operator= (const irange_legacy &);
  bool operator== (const irange_legacy &) const;
  bool operator!= (const irange_legacy &r) const { return !(*this == r); }

  // Misc methods.
  void dump (FILE * = stderr) const;

public:
  bool equal_p (const irange_legacy &) const;

protected:
  irange_legacy (tree *, unsigned);
  irange_legacy (tree *, unsigned, const irange_legacy &);
  void legacy_set (tree, tree, value_range_kind = VR_RANGE);
  bool simple_ranges_p () const;
  void union_helper (irange_legacy *, const irange_legacy *);
  void intersect_helper (irange_legacy *, const irange_legacy *);
  void check ();
private:
  int value_inside_range (tree) const;
  bool maybe_anti_range () const;
  void copy_simple_range (const irange_legacy &);
};

// Here we describe an irange with N pairs of ranges.  The storage for
// the pairs is embedded in the class as an array.

template<unsigned N>
class GTY((user)) int_range : public irange_legacy
{
public:
  int_range ();
  int_range (tree, tree, value_range_kind = VR_RANGE);
  int_range (tree type, const wide_int &, const wide_int &,
	     value_range_kind = VR_RANGE);
  int_range (tree type);
  int_range (const int_range &);
  int_range (const irange_legacy &);
  int_range& operator= (const int_range &);
private:
  template <unsigned X> friend void gt_ggc_mx (int_range<X> *);
  template <unsigned X> friend void gt_pch_nx (int_range<X> *);
  template <unsigned X> friend void gt_pch_nx (int_range<X> *,
					       gt_pointer_operator, void *);
  // ?? hash-traits.h has its own extern for these, which is causing
  // them to never be picked up by the templates.  For now, define
  // elsewhere.
  //template<unsigned X> friend void gt_ggc_mx (int_range<X> *&);
  //template<unsigned X> friend void gt_pch_nx (int_range<X> *&);
  friend void gt_ggc_mx (int_range<1> *&);
  friend void gt_pch_nx (int_range<1> *&);

  tree m_ranges[N*2];
};

// This is a special int_range<1> with only one pair, plus
// VR_ANTI_RANGE magic to describe slightly more than can be described
// in one pair.  It is described in the code as a "simple range" (as
// opposed to multi-ranges which have multiple sub-ranges).  It is
// provided for backward compatibility with code that has not been
// converted to multi-range irange's.
//
// There are copy operators to seamlessly copy to/fro multi-ranges.
typedef int_range<1> value_range;

// Returns true for an old-school value_range as described above.
inline bool
irange_legacy::simple_ranges_p () const
{
  return m_max_ranges == 1;
}

// An irange with "unlimited" sub-ranges.  In reality we are limited
// by the number of values that fit in an `m_num_ranges'.
//
// A widest_irange starts with a handful of sub-ranges in local
// storage and will grow into the heap as necessary.

class widest_irange : public irange_legacy
{
public:
  widest_irange ();
  widest_irange (tree, tree, value_range_kind = VR_RANGE);
  widest_irange (tree, const wide_int &, const wide_int &,
		 value_range_kind = VR_RANGE);
  widest_irange (tree type);
  widest_irange (const widest_irange &);
  widest_irange (const irange_legacy &);
  widest_irange (const irange_base &);
  ~widest_irange ();
  widest_irange& operator= (const widest_irange &);
  void resize_if_needed (unsigned);
  static void stats_dump (FILE *);

private:
  static const unsigned m_sub_ranges_in_local_storage = 5;
  void init_widest_irange ();

  // Memory usage stats.
  void stats_register_use (void);
  static int stats_used_buckets[11];

  tree *m_blob;
  tree m_ranges[m_sub_ranges_in_local_storage*2];
};

extern bool range_has_numeric_bounds_p (const irange_legacy *);
extern bool ranges_from_anti_range (const value_range *,
				    value_range *, value_range *);
extern void dump_value_range (FILE *, const irange_base *);
extern void dump_value_range (FILE *, const irange_legacy *);
extern void dump_value_range_stats (FILE *);
extern bool vrp_val_is_min (const_tree);
extern bool vrp_val_is_max (const_tree);
extern tree vrp_val_min (const_tree);
extern tree vrp_val_max (const_tree);
extern bool vrp_operand_equal_p (const_tree, const_tree);

inline value_range_kind
irange_legacy::kind () const
{
  if (undefined_p ())
    return VR_UNDEFINED;

  if (simple_ranges_p ())
    return m_kind;

  if (varying_p ())
    return VR_VARYING;

  gcc_checking_assert (m_kind == VR_RANGE);
  return VR_RANGE;
}

inline unsigned
irange_base::num_pairs () const
{
  return m_num_ranges;
}

inline tree
irange_base::type () const
{
  gcc_checking_assert (!undefined_p ());
  return TREE_TYPE (m_base[0]);
}

inline tree
irange_base::tree_lower_bound (unsigned i) const
{
  return m_base[i * 2];
}

inline tree
irange_base::tree_upper_bound (unsigned i) const
{
  return m_base[i * 2 + 1];
}

inline tree
irange_base::tree_upper_bound () const
{
  if (m_num_ranges)
    return tree_upper_bound (m_num_ranges - 1);
  return NULL;
}

inline tree
irange_legacy::min () const
{
  return tree_lower_bound (0);
}

inline tree
irange_legacy::max () const
{
  return tree_upper_bound ();
}

inline bool
irange_base::varying_p () const
{
  if (m_num_ranges != 1)
    return false;

  tree l = m_base[0];
  tree u = m_base[1];
  tree t = TREE_TYPE (l);
  if (INTEGRAL_TYPE_P (t))
    return l == TYPE_MIN_VALUE (t) && u == TYPE_MAX_VALUE (t);
  if (POINTER_TYPE_P (t))
    return wi::to_wide (l) == 0
	   && wi::to_wide (u) == wi::max_value (TYPE_PRECISION (t),
						TYPE_SIGN (t));
  return true;
}

inline bool
irange_legacy::varying_p () const
{
  if (simple_ranges_p ())
    return m_kind == VR_VARYING;

  return irange_base::varying_p ();
}

inline bool
irange_base::undefined_p () const
{
  return m_num_ranges == 0;
}

inline bool
irange_legacy::undefined_p () const
{
  if (CHECKING_P && simple_ranges_p ())
    {
      if (m_kind == VR_UNDEFINED)
	gcc_checking_assert (m_num_ranges == 0);
      else
	gcc_checking_assert (m_num_ranges != 0);
    }
  return irange_base::undefined_p ();
}

inline bool
irange_base::zero_p () const
{
  return (m_num_ranges == 1
	  && integer_zerop (tree_lower_bound (0))
	  && integer_zerop (tree_upper_bound (0)));
}

inline bool
irange_base::nonzero_p () const
{
  if (undefined_p ())
    return false;

  tree zero = build_zero_cst (type ());
  return *this == int_range<1> (zero, zero, VR_ANTI_RANGE);
}

inline bool
irange_base::supports_type_p (tree type)
{
  if (type && (INTEGRAL_TYPE_P (type) || POINTER_TYPE_P (type)))
    return type;
  return false;
}

inline bool
range_includes_zero_p (const irange_legacy *vr)
{
  if (vr->undefined_p ())
    return false;

  if (vr->varying_p ())
    return true;

  return vr->may_contain_p (build_zero_cst (vr->type ()));
}

template<unsigned N>
static inline void
gt_ggc_mx (int_range<N> *x)
{
  for (unsigned i = 0; i < N; ++i)
    {
      gt_ggc_mx (x->m_ranges[i * 2]);
      gt_ggc_mx (x->m_ranges[i * 2 + 1]);
    }
}

template<unsigned N>
static inline void
gt_pch_nx (int_range<N> *x)
{
  for (unsigned i = 0; i < N; ++i)
    {
      gt_pch_nx (x->m_ranges[i * 2]);
      gt_pch_nx (x->m_ranges[i * 2 + 1]);
    }
}

template<unsigned N>
static inline void
gt_pch_nx (int_range<N> *x, gt_pointer_operator op, void *cookie)
{
  for (unsigned i = 0; i < N; ++i)
    {
      op (&x->m_ranges[i * 2], cookie);
      op (&x->m_ranges[i * 2 + 1], cookie);
    }
}

// Constructors for irange_base.

inline
irange_base::irange_base (tree *base, unsigned nranges)
{
  m_kind = VR_UNDEFINED;
  m_discriminator = IRANGE_KIND_INT;
  m_base = base;
  m_num_ranges = 0;
  m_max_ranges = nranges;
}

inline
irange_base::irange_base (tree *base, unsigned nranges,
			  const irange_base &other)
{
  m_discriminator = IRANGE_KIND_INT;
  m_base = base;
  m_max_ranges = nranges;
  *this = other;
}

inline
irange_legacy::irange_legacy (tree *base, unsigned nranges)
  : irange_base (base, nranges)
{
  m_kind = VR_UNDEFINED;
  m_discriminator = IRANGE_KIND_INT;
  m_base = base;
  m_num_ranges = 0;
  m_max_ranges = nranges;
}

inline
irange_legacy::irange_legacy (tree *base, unsigned nranges,
			      const irange_legacy &other)
  : irange_base (base, nranges)
{
  m_num_ranges = 0;
  m_max_ranges = nranges;
  *this = other;
}

// Constructors for int_range<>.

template<unsigned N>
inline
int_range<N>::int_range ()
  : irange_legacy (m_ranges, N)
{
  m_kind = VR_UNDEFINED;
  m_num_ranges = 0;
}

template<unsigned N>
int_range<N>::int_range (const int_range &other)
  : irange_legacy (m_ranges, N, other)
{
}

template<unsigned N>
int_range<N>::int_range (tree min, tree max, value_range_kind kind)
  : irange_legacy (m_ranges, N)
{
  irange_legacy::set (min, max, kind);
}

template<unsigned N>
int_range<N>::int_range (tree type)
  : irange_legacy (m_ranges, N)
{
  set_varying (type);
}

template<unsigned N>
int_range<N>::int_range (tree type, const wide_int &wmin, const wide_int &wmax,
			 value_range_kind kind)
  : irange_legacy (m_ranges, N)
{
  tree min = wide_int_to_tree (type, wmin);
  tree max = wide_int_to_tree (type, wmax);
  set (min, max, kind);
}

template<unsigned N>
int_range<N>::int_range (const irange_legacy &other)
  : irange_legacy (m_ranges, N, other)
{
}

template<unsigned N>
int_range<N>&
int_range<N>::operator= (const int_range &src)
{
  irange_legacy::operator= (src);
  return *this;
}

inline void
irange_base::set_undefined ()
{
  m_kind = VR_RANGE;
  m_num_ranges = 0;
}

inline void
irange_legacy::set_undefined ()
{
  irange_base::set_undefined ();
  if (simple_ranges_p ())
    m_kind = VR_UNDEFINED;
}

inline void
irange_legacy::set_varying (tree type)
{
  irange_base::set_varying (type);
  if (simple_ranges_p ())
    m_kind = VR_VARYING;
}

inline
bool
irange_base::operator== (const irange_base &r) const
{
  return equal_p (r);
}

inline
bool
irange_legacy::operator== (const irange_legacy &r) const
{
  return irange_legacy::equal_p (r);
}

// Return the lower bound for a sub-range.  PAIR is the sub-range in
// question.

inline wide_int
irange_base::lower_bound (unsigned pair) const
{
  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  return wi::to_wide (tree_lower_bound (pair));
}

// Return the upper bound for a sub-range.  PAIR is the sub-range in
// question.

inline wide_int
irange_base::upper_bound (unsigned pair) const
{
  gcc_checking_assert (!undefined_p ());
  gcc_checking_assert (pair + 1 <= num_pairs ());
  return wi::to_wide (tree_upper_bound (pair));
}

// Return the highest bound in a range.

inline wide_int
irange_base::upper_bound () const
{
  unsigned pairs = num_pairs ();
  gcc_checking_assert (pairs > 0);
  return upper_bound (pairs - 1);
}

inline wide_int
irange_legacy::upper_bound () const
{
  unsigned pairs = num_pairs ();
  gcc_checking_assert (pairs > 0);
  return upper_bound (pairs - 1);
}

inline
widest_irange::~widest_irange ()
{
  if (CHECKING_P)
    stats_register_use ();
  if (m_blob)
    free (m_blob);
}


inline void
irange_legacy::union_ (const irange_legacy &r)
{
  dump_flags_t m_flags = dump_flags;
  dump_flags &= ~TDF_DETAILS;
  irange_legacy::union_ (&r);
  dump_flags = m_flags;
}

inline void
irange_legacy::intersect (const irange_legacy &r)
{
  dump_flags_t m_flags = dump_flags;
  dump_flags &= ~TDF_DETAILS;
  irange_legacy::intersect (&r);
  dump_flags = m_flags;
}

// Set value range VR to a nonzero range of type TYPE.

inline void
irange_base::set_nonzero (tree type)
{
  tree zero = build_int_cst (type, 0);
  set (zero, zero, VR_ANTI_RANGE);
}

// Set value range VR to a ZERO range of type TYPE.

inline void
irange_base::set_zero (tree type)
{
  tree z = build_int_cst (type, 0);
  set (z, z);
}

inline void
irange_legacy::set_nonzero (tree type)
{
  if (simple_ranges_p ())
    {
      tree zero = build_int_cst (type, 0);
      set (zero, zero, VR_ANTI_RANGE);
      return;
    }
  irange_base::set_nonzero (type);
}

inline void
irange_legacy::set_zero (tree type)
{
  if (simple_ranges_p ())
    {
      set (build_int_cst (type, 0));
      return;
    }
  irange_base::set_zero (type);
}

/* Normalize [MIN, MAX] into VARYING and ~[MIN, MAX] into UNDEFINED.

   Avoid using TYPE_{MIN,MAX}_VALUE because -fstrict-enums can
   restrict those to a subset of what actually fits in the type.
   Instead use the extremes of the type precision which will allow
   compare_range_with_value() to check if a value is inside a range,
   whereas if we used TYPE_*_VAL, said function would just punt
   upon seeing a VARYING.  */

inline bool
irange_base::normalize_min_max (tree type, tree min, tree max,
				value_range_kind kind)
{
  unsigned prec = TYPE_PRECISION (type);
  signop sign = TYPE_SIGN (type);
  if (wi::eq_p (wi::to_wide (min), wi::min_value (prec, sign))
      && wi::eq_p (wi::to_wide (max), wi::max_value (prec, sign)))
    {
      if (kind == VR_RANGE)
	set_varying (type);
      else if (kind == VR_ANTI_RANGE)
	set_undefined ();
      else
	gcc_unreachable ();
      return true;
    }
  return false;
}

// When irange_legacy is removed, irange_base will become irange.
typedef irange_legacy irange;

#endif // GCC_VALUE_RANGE_H
