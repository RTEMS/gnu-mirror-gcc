// Common infrastructure for Load/Store Pair fusion optimization pass.
// Copyright (C) 2023-2024 Free Software Foundation, Inc.
//
// This file is part of GCC.
//
// GCC is free software; you can redistribute it and/or modify it
// under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 3, or (at your option)
// any later version.
//
// GCC is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with GCC; see the file COPYING3.  If not see
// <http://www.gnu.org/licenses/>.

#define INCLUDE_ALGORITHM
#define INCLUDE_FUNCTIONAL
#define INCLUDE_LIST
#define INCLUDE_TYPE_TRAITS
#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "df.h"
#include "rtl-iter.h"
#include "rtl-ssa.h"
#include "cfgcleanup.h"
#include "tree-pass.h"
#include "ordered-hash-map.h"
#include "tree-dfa.h"
#include "fold-const.h"
#include "tree-hash-traits.h"
#include "print-tree.h"
#include "insn-attr.h"

using namespace rtl_ssa;

static constexpr HOST_WIDE_INT LDP_IMM_BITS = 7;
static constexpr HOST_WIDE_INT LDP_IMM_SIGN_BIT = (1 << (LDP_IMM_BITS - 1));
static constexpr HOST_WIDE_INT LDP_MAX_IMM = LDP_IMM_SIGN_BIT - 1;
static constexpr HOST_WIDE_INT LDP_MIN_IMM = -LDP_MAX_IMM - 1;

// We pack these fields (load_p, fpsimd_p, and size) into an integer
// (LFS) which we use as part of the key into the main hash tables.
//
// The idea is that we group candidates together only if they agree on
// the fields below.  Candidates that disagree on any of these
// properties shouldn't be merged together.
struct lfs_fields
{
  bool load_p;
  bool fpsimd_p;
  unsigned size;
};

using insn_list_t = std::list<insn_info *>;
using insn_iter_t = insn_list_t::iterator;

// Information about the accesses at a given offset from a particular
// base.  Stored in an access_group, see below.
struct access_record
{
  poly_int64 offset;
  std::list<insn_info *> cand_insns;
  std::list<access_record>::iterator place;

  access_record (poly_int64 off) : offset (off) {}
};

// A group of accesses where adjacent accesses could be ldp/stp
// or lxvp/stxvp candidates.  The splay tree supports efficient
// insertion, while the list supports efficient iteration.
struct access_group
{
  splay_tree<access_record *> tree;
  std::list<access_record> list;

  template<typename Alloc>
  inline void track (Alloc node_alloc, poly_int64 offset, insn_info *insn);
};

// Information about a potential base candidate, used in try_fuse_pair.
// There may be zero, one, or two viable RTL bases for a given pair.
struct base_cand
{
  // DEF is the def of the base register to be used by the pair.
  def_info *def;

  // FROM_INSN is -1 if the base candidate is already shared by both
  // candidate insns.  Otherwise it holds the index of the insn from
  // which the base originated.
  //
  // In the case that the base is shared, either DEF is already used
  // by both candidate accesses, or both accesses see different versions
  // of the same regno, in which case DEF is the def consumed by the
  // first candidate access.
  int from_insn;

  // To form a pair, we do so by moving the first access down and the second
  // access up.  To determine where to form the pair, and whether or not
  // it is safe to form the pair, we track instructions which cannot be
  // re-ordered past due to either dataflow or alias hazards.
  //
  // Since we allow changing the base used by an access, the choice of
  // base can change which instructions act as re-ordering hazards for
  // this pair (due to different dataflow).  We store the initial
  // dataflow hazards for this choice of base candidate in HAZARDS.
  //
  // These hazards act as re-ordering barriers to each candidate insn
  // respectively, in program order.
  //
  // Later on, when we take alias analysis into account, we narrow
  // HAZARDS accordingly.
  insn_info *hazards[2];

  base_cand (def_info *def, int insn)
    : def (def), from_insn (insn), hazards {nullptr, nullptr} {}

  base_cand (def_info *def) : base_cand (def, -1) {}

  // Test if this base candidate is viable according to HAZARDS.
  bool viable () const
  {
    return !hazards[0] || !hazards[1] || (*hazards[0] > *hazards[1]);
  }
};

// Information about an alternate base.  For a def_info D, it may
// instead be expressed as D = BASE + OFFSET.
struct alt_base
{
  def_info *base;
  poly_int64 offset;
};

// State used by the pass for a given basic block.
struct fusion_bb_info
{
  using def_hash = nofree_ptr_hash<def_info>;
  using expr_key_t = pair_hash<tree_operand_hash, int_hash<int, -1, -2>>;
  using def_key_t = pair_hash<def_hash, int_hash<int, -1, -2>>;

  // Map of <tree base, LFS> -> access_group.
  ordered_hash_map<expr_key_t, access_group> expr_map;

  // Map of <RTL-SSA def_info *, LFS> -> access_group.
  ordered_hash_map<def_key_t, access_group> def_map;

  // Given the def_info for an RTL base register, express it as an offset from
  // some canonical base instead.
  //
  // Canonicalizing bases in this way allows us to identify adjacent accesses
  // even if they see different base register defs.
  hash_map<def_hash, alt_base> canon_base_map;

  static const size_t obstack_alignment = sizeof (void *);
  bb_info *m_bb;

  fusion_bb_info (bb_info *bb) : m_bb (bb), m_emitted_tombstone (false)
  {
    obstack_specify_allocation (&m_obstack, OBSTACK_CHUNK_SIZE,
				obstack_alignment, obstack_chunk_alloc,
				obstack_chunk_free);
  }
  ~fusion_bb_info ()
  {
    obstack_free (&m_obstack, nullptr);

    if (m_emitted_tombstone)
      {
	bitmap_release (&m_tombstone_bitmap);
	bitmap_obstack_release (&m_bitmap_obstack);
      }
  }

  inline void track_access (insn_info *, bool load, rtx mem);
  inline void transform ();
  inline void cleanup_tombstones ();

private:
  obstack m_obstack;

  // State for keeping track of tombstone insns emitted for this BB.
  bitmap_obstack m_bitmap_obstack;
  bitmap_head m_tombstone_bitmap;
  bool m_emitted_tombstone;

  inline splay_tree_node<access_record *> *node_alloc (access_record *);

  template<typename Map>
  inline void traverse_base_map (Map &map);
  inline void transform_for_base (int load_size, access_group &group);

   void merge_pairs (insn_list_t &, insn_list_t &,
			   bool load_p, unsigned access_size);

   bool try_fuse_pair (bool load_p, unsigned access_size,
			     insn_info *i1, insn_info *i2);

   bool fuse_pair (bool load_p, unsigned access_size,
			 int writeback,
			 insn_info *i1, insn_info *i2,
			 base_cand &base,
			 const insn_range_info &move_range);

  inline void track_tombstone (int uid);

  inline bool track_via_mem_expr (insn_info *, rtx mem, lfs_fields lfs);
};

splay_tree_node<access_record *> *fusion_bb_info::node_alloc (access_record *access)
{
  using T = splay_tree_node<access_record *>;
  void *addr = obstack_alloc (&m_obstack, sizeof (T));
  return new (addr) T (access);
}

// Given a mem MEM, if the address has side effects, return a MEM that accesses
// the same address but without the side effects.  Otherwise, return
// MEM unchanged.
static rtx
drop_writeback (rtx mem)
{
  rtx addr = XEXP (mem, 0);

  if (!side_effects_p (addr))
    return mem;

  switch (GET_CODE (addr))
    {
    case PRE_MODIFY:
      addr = XEXP (addr, 1);
      break;
    case POST_MODIFY:
    case POST_INC:
    case POST_DEC:
      addr = XEXP (addr, 0);
      break;
    case PRE_INC:
    case PRE_DEC:
    {
      poly_int64 adjustment = GET_MODE_SIZE (GET_MODE (mem));
      if (GET_CODE (addr) == PRE_DEC)
	adjustment *= -1;
      addr = plus_constant (GET_MODE (addr), XEXP (addr, 0), adjustment);
      break;
    }
    default:
      gcc_unreachable ();
    }

  return change_address (mem, GET_MODE (mem), addr);
}

// Convenience wrapper around strip_offset that can also look through
// RTX_AUTOINC addresses.  The interface is like strip_offset except we take a
// MEM so that we know the mode of the access.
static rtx
load_strip_offset (rtx mem, poly_int64 *offset)
{
  rtx addr = XEXP (mem, 0);

  switch (GET_CODE (addr))
    {
    case PRE_MODIFY:
    case POST_MODIFY:
      addr = strip_offset (XEXP (addr, 1), offset);
      gcc_checking_assert (REG_P (addr));
      gcc_checking_assert (rtx_equal_p (XEXP (XEXP (mem, 0), 0), addr));
      break;
    case PRE_INC:
    case POST_INC:
      addr = XEXP (addr, 0);
      *offset = GET_MODE_SIZE (GET_MODE (mem));
      gcc_checking_assert (REG_P (addr));
      break;
    case PRE_DEC:
    case POST_DEC:
      addr = XEXP (addr, 0);
      *offset = -GET_MODE_SIZE (GET_MODE (mem));
      gcc_checking_assert (REG_P (addr));
      break;

    default:
      addr = strip_offset (addr, offset);
    }

  return addr;
}

// Return true if X is a PRE_{INC,DEC,MODIFY} rtx.
static bool
any_pre_modify_p (rtx x)
{
  const auto code = GET_CODE (x);
  return code == PRE_INC || code == PRE_DEC || code == PRE_MODIFY;
}

// Return true if X is a POST_{INC,DEC,MODIFY} rtx.
static bool
any_post_modify_p (rtx x)
{
  const auto code = GET_CODE (x);
  return code == POST_INC || code == POST_DEC || code == POST_MODIFY;
}
// Given LFS (load_p, fpsimd_p, size) fields in FIELDS, encode these
// into an integer for use as a hash table key.
static int
encode_lfs (lfs_fields fields)
{
  int size_log2 = exact_log2 (fields.size);
  //gcc_checking_assert (size_log2 >= 2 && size_log2 <= 4);
  return ((int)fields.load_p << 3)
    | ((int)fields.fpsimd_p << 2)
    | (size_log2 - 2);
}

// Inverse of encode_lfs.
static lfs_fields
decode_lfs (int lfs)
{
  bool load_p = (lfs & (1 << 3));
  bool fpsimd_p = (lfs & (1 << 2));
  unsigned size = 1U << ((lfs & 3) + 2);
  return { load_p, fpsimd_p, size };
}
// Track the access INSN at offset OFFSET in this access group.
// ALLOC_NODE is used to allocate splay tree nodes.
template<typename Alloc>
void
access_group::track (Alloc alloc_node, poly_int64 offset, insn_info *insn)
{
  auto insert_before = [&](std::list<access_record>::iterator after)
    {
      auto it = list.emplace (after, offset);
      it->cand_insns.push_back (insn);
      it->place = it;
      return &*it;
    };

  if (!list.size ())
    {
      auto access = insert_before (list.end ());
      tree.insert_max_node (alloc_node (access));
      return;
    }

  auto compare = [&](splay_tree_node<access_record *> *node)
    {
      return compare_sizes_for_sort (offset, node->value ()->offset);
    };
  auto result = tree.lookup (compare);
  splay_tree_node<access_record *> *node = tree.root ();
  if (result == 0)
    node->value ()->cand_insns.push_back (insn);
  else
    {
      auto it = node->value ()->place;
      auto after = (result > 0) ? std::next (it) : it;
      auto access = insert_before (after);
      tree.insert_child (node, result > 0, alloc_node (access));
    }
}
// Given a candidate access INSN (with mem MEM), see if it has a suitable
// MEM_EXPR base (i.e. a tree decl) relative to which we can track the access.
// LFS is used as part of the key to the hash table, see track_access.
bool
fusion_bb_info::track_via_mem_expr (insn_info *insn, rtx mem, lfs_fields lfs)
{
  if (!MEM_EXPR (mem) || !MEM_OFFSET_KNOWN_P (mem))
    return false;

  poly_int64 offset;
  tree base_expr = get_addr_base_and_unit_offset (MEM_EXPR (mem),
						  &offset);
  if (!base_expr || !DECL_P (base_expr))
    return false;

  offset += MEM_OFFSET (mem);

  const machine_mode mem_mode = GET_MODE (mem);
  const HOST_WIDE_INT mem_size = GET_MODE_SIZE (mem_mode);//.to_constant ();

  // Punt on misaligned offsets.  LDP/STP instructions require offsets to be a
  // multiple of the access size, and we believe that misaligned offsets on
  // MEM_EXPR bases are likely to lead to misaligned offsets w.r.t. RTL bases.
  if (!multiple_p (offset, mem_size))
    return false;

  const auto key = std::make_pair (base_expr, encode_lfs (lfs));
  access_group &group = expr_map.get_or_insert (key, NULL);
  auto alloc = [&](access_record *access) { return node_alloc (access); };
  group.track (alloc, offset, insn);

  if (dump_file)
    {
      fprintf (dump_file, "[bb %u] tracking insn %d via ",
	       m_bb->index (), insn->uid ());
      print_node_brief (dump_file, "mem expr", base_expr, 0);
      fprintf (dump_file, " [L=%d FP=%d, %smode, off=",
	       lfs.load_p, lfs.fpsimd_p, mode_name[mem_mode]);
      print_dec (offset, dump_file);
      fprintf (dump_file, "]\n");
    }

  return true;
}

// Dummy predicate that never ignores any insns.
static bool no_ignore (insn_info *) { return false; }

// Return the latest dataflow hazard before INSN.
//
// If IGNORE is non-NULL, this points to a sub-rtx which we should ignore for
// dataflow purposes.  This is needed when considering changing the RTL base of
// an access discovered through a MEM_EXPR base.
//
// If IGNORE_INSN is non-NULL, we should further ignore any hazards arising
// from that insn.
//
// N.B. we ignore any defs/uses of memory here as we deal with that separately,
// making use of alias disambiguation.
static insn_info *
latest_hazard_before (insn_info *insn, rtx *ignore,
		      insn_info *ignore_insn = nullptr)
{
  insn_info *result = nullptr;

  // If the insn can throw then it is at the end of a BB and we can't
  // move it, model this by recording a hazard in the previous insn
  // which will prevent moving the insn up.
  if (cfun->can_throw_non_call_exceptions
      && find_reg_note (insn->rtl (), REG_EH_REGION, NULL_RTX))
    return insn->prev_nondebug_insn ();

  // Return true if we registered the hazard.
  auto hazard = [&](insn_info *h) -> bool
    {
      gcc_checking_assert (*h < *insn);
      if (h == ignore_insn)
	return false;

      if (!result || *h > *result)
	result = h;

      return true;
    };

  rtx pat = PATTERN (insn->rtl ());
  auto ignore_use = [&](use_info *u)
    {
      if (u->is_mem ())
	return true;

      return !refers_to_regno_p (u->regno (), u->regno () + 1, pat, ignore);
    };

  // Find defs of uses in INSN (RaW).
  for (auto use : insn->uses ())
    if (!ignore_use (use) && use->def ())
      hazard (use->def ()->insn ());

  // Find previous defs (WaW) or previous uses (WaR) of defs in INSN.
  for (auto def : insn->defs ())
    {
      if (def->is_mem ())
	continue;

      if (def->prev_def ())
	{
	  hazard (def->prev_def ()->insn ()); // WaW

	  auto set = dyn_cast<set_info *> (def->prev_def ());
	  if (set && set->has_nondebug_insn_uses ())
	    for (auto use : set->reverse_nondebug_insn_uses ())
	      if (use->insn () != insn && hazard (use->insn ())) // WaR
		break;
	}

      if (!HARD_REGISTER_NUM_P (def->regno ()))
	continue;

      // Also need to check backwards for call clobbers (WaW).
      for (auto call_group : def->ebb ()->call_clobbers ())
	{
	  if (!call_group->clobbers (def->resource ()))
	    continue;

	  auto clobber_insn = prev_call_clobbers_ignoring (*call_group,
							   def->insn (),
							   no_ignore);
	  if (clobber_insn)
	    hazard (clobber_insn);
	}

    }

  return result;
}

// Return the first dataflow hazard after INSN.
//
// If IGNORE is non-NULL, this points to a sub-rtx which we should ignore for
// dataflow purposes.  This is needed when considering changing the RTL base of
// an access discovered through a MEM_EXPR base.
//
// N.B. we ignore any defs/uses of memory here as we deal with that separately,
// making use of alias disambiguation.
static insn_info *
first_hazard_after (insn_info *insn, rtx *ignore)
{
  insn_info *result = nullptr;
  auto hazard = [insn, &result](insn_info *h)
    {
      gcc_checking_assert (*h > *insn);
      if (!result || *h < *result)
	result = h;
    };

  rtx pat = PATTERN (insn->rtl ());
  auto ignore_use = [&](use_info *u)
    {
      if (u->is_mem ())
	return true;

      return !refers_to_regno_p (u->regno (), u->regno () + 1, pat, ignore);
    };

  for (auto def : insn->defs ())
    {
      if (def->is_mem ())
	continue;

      if (def->next_def ())
	hazard (def->next_def ()->insn ()); // WaW

      auto set = dyn_cast<set_info *> (def);
      if (set && set->has_nondebug_insn_uses ())
	hazard (set->first_nondebug_insn_use ()->insn ()); // RaW

      if (!HARD_REGISTER_NUM_P (def->regno ()))
	continue;

      // Also check for call clobbers of this def (WaW).
      for (auto call_group : def->ebb ()->call_clobbers ())
	{
	  if (!call_group->clobbers (def->resource ()))
	    continue;

	  auto clobber_insn = next_call_clobbers_ignoring (*call_group,
							   def->insn (),
							   no_ignore);
	  if (clobber_insn)
	    hazard (clobber_insn);
	}
    }

  // Find any subsequent defs of uses in INSN (WaR).
  for (auto use : insn->uses ())
    {
      if (ignore_use (use))
	continue;

      if (use->def ())
	{
	  auto def = use->def ()->next_def ();
	  if (def && def->insn () == insn)
	    def = def->next_def ();

	  if (def)
	    hazard (def->insn ());
	}

      if (!HARD_REGISTER_NUM_P (use->regno ()))
	continue;

      // Also need to handle call clobbers of our uses (again WaR).
      //
      // See restrict_movement_for_uses_ignoring for why we don't
      // need to check backwards for call clobbers.
      for (auto call_group : use->ebb ()->call_clobbers ())
	{
	  if (!call_group->clobbers (use->resource ()))
	    continue;

	  auto clobber_insn = next_call_clobbers_ignoring (*call_group,
							   use->insn (),
							   no_ignore);
	  if (clobber_insn)
	    hazard (clobber_insn);
	}
    }

  return result;
}

// Return true iff R1 and R2 overlap.
static bool
ranges_overlap_p (const insn_range_info &r1, const insn_range_info &r2)
{
  // If either range is empty, then their intersection is empty.
  if (!r1 || !r2)
    return false;

  // When do they not overlap? When one range finishes before the other
  // starts, i.e. (*r1.last < *r2.first || *r2.last < *r1.first).
  // Inverting this, we get the below.
  return *r1.last >= *r2.first && *r2.last >= *r1.first;
}

// Get the range of insns that def feeds.
static insn_range_info get_def_range (def_info *def)
{
  insn_info *last = def->next_def ()->insn ()->prev_nondebug_insn ();
  return { def->insn (), last };
}

// Given a def (of memory), return the downwards range within which we
// can safely move this def.
static insn_range_info
def_downwards_move_range (def_info *def)
{
  auto range = get_def_range (def);

  auto set = dyn_cast<set_info *> (def);
  if (!set || !set->has_any_uses ())
    return range;

  auto use = set->first_nondebug_insn_use ();
  if (use)
    range = move_earlier_than (range, use->insn ());

  return range;
}

// Given a def (of memory), return the upwards range within which we can
// safely move this def.
static insn_range_info
def_upwards_move_range (def_info *def)
{
  def_info *prev = def->prev_def ();
  insn_range_info range { prev->insn (), def->insn () };

  auto set = dyn_cast<set_info *> (prev);
  if (!set || !set->has_any_uses ())
    return range;

  auto use = set->last_nondebug_insn_use ();
  if (use)
    range = move_later_than (range, use->insn ());

  return range;
}

// Given candidate store insns FIRST and SECOND, see if we can re-purpose one
// of them (together with its def of memory) for the stp/stxvp insn. If so,
//  return that insn.  Otherwise, return null.
static insn_info *
decide_store_strategy (insn_info *first,
		     insn_info *second,
		     const insn_range_info &move_range)
{
  def_info * const defs[2] = {
    memory_access (first->defs ()),
    memory_access (second->defs ())
  };

  if (move_range.includes (first)
      || ranges_overlap_p (move_range, def_downwards_move_range (defs[0])))
    return first;

  if (move_range.includes (second)
      || ranges_overlap_p (move_range, def_upwards_move_range (defs[1])))
    return second;

  return nullptr;
}

// Generate the RTL pattern for a "tombstone"; used temporarily during this pass
// to replace stores that are marked for deletion where we can't immediately
// delete the store (since there are uses of mem hanging off the store).
//
// These are deleted at the end of the pass and uses re-parented appropriately
// at this point.
static rtx
gen_tombstone (void)
{
  return gen_rtx_CLOBBER (VOIDmode,
			  gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (Pmode)));
}
// Go through the reg notes rooted at NOTE, dropping those that we should drop,
// and preserving those that we want to keep by prepending them to (and
// returning) RESULT.  EH_REGION is used to make sure we have at most one
// REG_EH_REGION note in the resulting list.  FR_EXPR is used to return any
// REG_FRAME_RELATED_EXPR note we find, as these can need special handling in
// combine_reg_notes.
static rtx
filter_notes (rtx note, rtx result, bool *eh_region, rtx *fr_expr)
{
  for (; note; note = XEXP (note, 1))
    {
      switch (REG_NOTE_KIND (note))
	{
	case REG_DEAD:
	  // REG_DEAD notes aren't required to be maintained.
	case REG_EQUAL:
	case REG_EQUIV:
	case REG_UNUSED:
	case REG_NOALIAS:
	  // These can all be dropped.  For REG_EQU{AL,IV} they cannot apply to
	  // non-single_set insns, and REG_UNUSED is re-computed by RTl-SSA, see
	  // rtl-ssa/changes.cc:update_notes.
	  //
	  // Similarly, REG_NOALIAS cannot apply to a parallel.
	case REG_INC:
	  // When we form the pair insn, the reg update is implemented
	  // as just another SET in the parallel, so isn't really an
	  // auto-increment in the RTL sense, hence we drop the note.
	  break;
	case REG_EH_REGION:
	  gcc_assert (!*eh_region);
	  *eh_region = true;
	  result = alloc_reg_note (REG_EH_REGION, XEXP (note, 0), result);
	  break;
	case REG_CFA_DEF_CFA:
	case REG_CFA_OFFSET:
	case REG_CFA_RESTORE:
	  result = alloc_reg_note (REG_NOTE_KIND (note),
				   copy_rtx (XEXP (note, 0)),
				   result);
	  break;
	case REG_FRAME_RELATED_EXPR:
	  gcc_assert (!*fr_expr);
	  *fr_expr = copy_rtx (XEXP (note, 0));
	  break;
	default:
	  // Unexpected REG_NOTE kind.
	  gcc_unreachable ();
	}
    }

  return result;
}

// Return the notes that should be attached to a combination of I1 and I2, where
// *I1 < *I2.  LOAD_P is true for loads.
static rtx
combine_reg_notes (insn_info *i1, insn_info *i2, bool load_p)
{
  // Temporary storage for REG_FRAME_RELATED_EXPR notes.
  rtx fr_expr[2] = {};

  bool found_eh_region = false;
  rtx result = NULL_RTX;
  result = filter_notes (REG_NOTES (i2->rtl ()), result,
			 &found_eh_region, fr_expr);
  result = filter_notes (REG_NOTES (i1->rtl ()), result,
			 &found_eh_region, fr_expr + 1);

  if (!load_p)
    {
      // Simple frame-related sp-relative saves don't need CFI notes, but when
      // we combine them into an stp/stxvp we will need a CFI note as dwarf2cfi
      //  can't interpret the unspec pair representation directly.
      if (RTX_FRAME_RELATED_P (i1->rtl ()) && !fr_expr[0])
	fr_expr[0] = copy_rtx (PATTERN (i1->rtl ()));
      if (RTX_FRAME_RELATED_P (i2->rtl ()) && !fr_expr[1])
	fr_expr[1] = copy_rtx (PATTERN (i2->rtl ()));
    }

  rtx fr_pat = NULL_RTX;
  if (fr_expr[0] && fr_expr[1])
    {
      // Combining two frame-related insns, need to construct
      // a REG_FRAME_RELATED_EXPR note which represents the combined
      // operation.
      RTX_FRAME_RELATED_P (fr_expr[1]) = 1;
      fr_pat = gen_rtx_PARALLEL (VOIDmode,
				 gen_rtvec (2, fr_expr[0], fr_expr[1]));
    }
  else
    fr_pat = fr_expr[0] ? fr_expr[0] : fr_expr[1];

  if (fr_pat)
    result = alloc_reg_note (REG_FRAME_RELATED_EXPR,
			     fr_pat, result);

  return result;
}

// Given two memory accesses in PATS, at least one of which is of a
// writeback form, extract two non-writeback memory accesses addressed
// relative to the initial value of the base register, and output these
// in PATS.  Return an rtx that represents the overall change to the
// base register.
static rtx
extract_writebacks (bool load_p, rtx pats[2], int changed)
{
  rtx base_reg = NULL_RTX;
  poly_int64 current_offset = 0;

  poly_int64 offsets[2];

  for (int i = 0; i < 2; i++)
    {
      rtx mem = XEXP (pats[i], load_p);
      rtx reg = XEXP (pats[i], !load_p);

      rtx addr = XEXP (mem, 0);
      const bool autoinc_p = GET_RTX_CLASS (GET_CODE (addr)) == RTX_AUTOINC;

      poly_int64 offset;
      rtx this_base = load_strip_offset (mem, &offset);
      gcc_assert (REG_P (this_base));
      if (base_reg)
	gcc_assert (rtx_equal_p (base_reg, this_base));
      else
	base_reg = this_base;

      // If we changed base for the current insn, then we already
      // derived the correct mem for this insn from the effective
      // address of the other access.
      if (i == changed)
	{
	  gcc_checking_assert (!autoinc_p);
	  offsets[i] = offset;
	  continue;
	}

      if (autoinc_p && any_pre_modify_p (addr))
	current_offset += offset;

      poly_int64 this_off = current_offset;
      if (!autoinc_p)
	this_off += offset;

      offsets[i] = this_off;
      rtx new_mem = change_address (mem, GET_MODE (mem),
				    plus_constant (GET_MODE (base_reg),
						   base_reg, this_off));
      pats[i] = load_p
	? gen_rtx_SET (reg, new_mem)
	: gen_rtx_SET (new_mem, reg);

      if (autoinc_p && any_post_modify_p (addr))
	current_offset += offset;
    }

  if (known_eq (current_offset, 0))
    return NULL_RTX;

  return gen_rtx_SET (base_reg, plus_constant (GET_MODE (base_reg),
					       base_reg, current_offset));
}

// We just emitted a tombstone with uid UID, track it in a bitmap for
// this BB so we can easily identify it later when cleaning up tombstones.
void
fusion_bb_info::track_tombstone (int uid)
{
  if (!m_emitted_tombstone)
    {
      // Lazily initialize the bitmap for tracking tombstone insns.
      bitmap_obstack_initialize (&m_bitmap_obstack);
      bitmap_initialize (&m_tombstone_bitmap, &m_bitmap_obstack);
      m_emitted_tombstone = true;
    }

  if (!bitmap_set_bit (&m_tombstone_bitmap, uid))
    gcc_unreachable (); // Bit should have changed.
}

// Virtual base class for load/store walkers used in alias analysis.
struct alias_walker
{
  virtual bool conflict_p (int &budget) const = 0;
  virtual insn_info *insn () const = 0;
  virtual bool valid () const  = 0;
  virtual void advance () = 0;
};

// Implement some common functionality used by both store_walker
// and load_walker.
template<bool reverse>
class def_walker : public alias_walker
{
protected:
  using def_iter_t = typename std::conditional<reverse,
	reverse_def_iterator, def_iterator>::type;

  static use_info *start_use_chain (def_iter_t &def_iter)
  {
    set_info *set = nullptr;
    for (; *def_iter; def_iter++)
      {
	set = dyn_cast<set_info *> (*def_iter);
	if (!set)
	  continue;

	use_info *use = reverse
	  ? set->last_nondebug_insn_use ()
	  : set->first_nondebug_insn_use ();

	if (use)
	  return use;
      }

    return nullptr;
  }

  def_iter_t def_iter;
  insn_info *limit;
  def_walker (def_info *def, insn_info *limit) :
    def_iter (def), limit (limit) {}

  virtual bool iter_valid () const { return *def_iter; }

public:
  insn_info *insn () const override { return (*def_iter)->insn (); }
  void advance () override { def_iter++; }
  bool valid () const override final
  {
    if (!iter_valid ())
      return false;

    if (reverse)
      return *(insn ()) > *limit;
    else
      return *(insn ()) < *limit;
  }
};

// alias_walker that iterates over stores.
template<bool reverse, typename InsnPredicate>
class store_walker : public def_walker<reverse>
{
  rtx cand_mem;
  InsnPredicate tombstone_p;

public:
  store_walker (def_info *mem_def, rtx mem, insn_info *limit_insn,
		InsnPredicate tombstone_fn) :
    def_walker<reverse> (mem_def, limit_insn),
    cand_mem (mem), tombstone_p (tombstone_fn) {}

  bool conflict_p (int &budget) const override final
  {
    if (tombstone_p (this->insn ()))
      return false;

    return store_modifies_mem_p (cand_mem, this->insn (), budget);
  }
};

// alias_walker that iterates over loads.
template<bool reverse>
class load_walker : public def_walker<reverse>
{
  using Base = def_walker<reverse>;
  using use_iter_t = typename std::conditional<reverse,
	reverse_use_iterator, nondebug_insn_use_iterator>::type;

  use_iter_t use_iter;
  insn_info *cand_store;

  bool iter_valid () const override final { return *use_iter; }

public:
  void advance () override final
  {
    use_iter++;
    if (*use_iter)
      return;
    this->def_iter++;
    use_iter = Base::start_use_chain (this->def_iter);
  }

  insn_info *insn () const override final
  {
    return (*use_iter)->insn ();
  }

  bool conflict_p (int &budget) const override final
  {
    return load_modified_by_store_p (insn (), cand_store, budget);
  }

  load_walker (def_info *def, insn_info *store, insn_info *limit_insn)
    : Base (def, limit_insn),
      use_iter (Base::start_use_chain (this->def_iter)),
      cand_store (store) {}
};

// Given INSNS (in program order) which are known to be adjacent, look
// to see if either insn has a suitable RTL (register) base that we can
// use to form a pair.  Push these to BASE_CANDS if we find any.  CAND_MEMs
// gives the relevant mems from the candidate insns, ACCESS_SIZE gives the
// size of a single candidate access, and REVERSED says whether the accesses
// are inverted in offset order.
//
// Returns an integer where bit (1 << i) is set if INSNS[i] uses writeback
// addressing.
static int
get_viable_bases (insn_info *insns[2],
		  vec<base_cand> &base_cands,
		  rtx cand_mems[2],
		  unsigned access_size,
		  bool reversed)
{
  // We discovered this pair through a common base.  Need to ensure that
  // we have a common base register that is live at both locations.
  def_info *base_defs[2] = {};
  int writeback = 0;
  for (int i = 0; i < 2; i++)
    {
      const bool is_lower = (i == reversed);
      poly_int64 poly_off;
      rtx base = load_strip_offset (cand_mems[i], &poly_off);
      if (GET_RTX_CLASS (GET_CODE (XEXP (cand_mems[i], 0))) == RTX_AUTOINC)
	writeback |= (1 << i);

      if (!REG_P (base) || !poly_off.is_constant ())
	continue;

      // Punt on accesses relative to eliminable regs.  See the comment in
      // fusion_bb_info::track_access for a detailed explanation of this.
      if (!reload_completed
	  && (REGNO (base) == FRAME_POINTER_REGNUM
	      || REGNO (base) == ARG_POINTER_REGNUM))
	continue;

      HOST_WIDE_INT base_off = poly_off.to_constant ();

      // It should be unlikely that we ever punt here, since MEM_EXPR offset
      // alignment should be a good proxy for register offset alignment.
      if (base_off % access_size != 0)
	{
	  if (dump_file)
	    fprintf (dump_file,
		     "base not viable, offset misaligned (insn %d)\n",
		     insns[i]->uid ());
	  continue;
	}

      base_off /= access_size;

      if (!is_lower)
	base_off--;

      if (base_off < LDP_MIN_IMM || base_off > LDP_MAX_IMM)
	continue;

      use_info *use = find_access (insns[i]->uses (), REGNO (base));
      gcc_assert (use);
      base_defs[i] = use->def ();
    }

  if (!base_defs[0] && !base_defs[1])
    {
      if (dump_file)
	fprintf (dump_file, "no viable base register for pair (%d,%d)\n",
		 insns[0]->uid (), insns[1]->uid ());
      return writeback;
    }

  for (int i = 0; i < 2; i++)
    if ((writeback & (1 << i)) && !base_defs[i])
      {
	if (dump_file)
	  fprintf (dump_file, "insn %d has writeback but base isn't viable\n",
		   insns[i]->uid ());
	return writeback;
      }

  if (writeback == 3
      && base_defs[0]->regno () != base_defs[1]->regno ())
    {
      if (dump_file)
	fprintf (dump_file,
		 "pair (%d,%d): double writeback with distinct regs (%d,%d): "
		 "punting\n",
		 insns[0]->uid (), insns[1]->uid (),
		 base_defs[0]->regno (), base_defs[1]->regno ());
      return writeback;
    }

  if (base_defs[0] && base_defs[1]
      && base_defs[0]->regno () == base_defs[1]->regno ())
    {
      // Easy case: insns already share the same base reg.
      base_cands.quick_push (base_defs[0]);
      return writeback;
    }

  // Otherwise, we know that one of the bases must change.
  //
  // Note that if there is writeback we must use the writeback base
  // (we know now there is exactly one).
  for (int i = 0; i < 2; i++)
    if (base_defs[i] && (!writeback || (writeback & (1 << i))))
      base_cands.quick_push (base_cand { base_defs[i], i });

  return writeback;
}

static void
dump_insn_list (FILE *f, const insn_list_t &l)
{
  fprintf (f, "(");

  auto i = l.begin ();
  auto end = l.end ();

  if (i != end)
    fprintf (f, "%d", (*i)->uid ());
  i++;

  for (; i != end; i++)
    fprintf (f, ", %d", (*i)->uid ());

  fprintf (f, ")");
}

DEBUG_FUNCTION void
debug (const insn_list_t &l)
{
  dump_insn_list (stderr, l);
  fprintf (stderr, "\n");
}

// If we emitted tombstone insns for this BB, iterate through the BB
// and remove all the tombstone insns, being sure to reparent any uses
// of mem to previous defs when we do this.
void
fusion_bb_info::cleanup_tombstones ()
{
  // No need to do anything if we didn't emit a tombstone insn for this BB.
  if (!m_emitted_tombstone)
    return;

  insn_info *insn = m_bb->head_insn ();
  while (insn)
    {
      insn_info *next = insn->next_nondebug_insn ();
      if (!insn->is_real ()
	  || !bitmap_bit_p (&m_tombstone_bitmap, insn->uid ()))
	{
	  insn = next;
	  continue;
	}

      auto def = memory_access (insn->defs ());
      auto set = dyn_cast<set_info *> (def);
      if (set && set->has_any_uses ())
	{
	  def_info *prev_def = def->prev_def ();
	  auto prev_set = dyn_cast<set_info *> (prev_def);
	  if (!prev_set)
	    gcc_unreachable ();

	  while (set->first_use ())
	    crtl->ssa->reparent_use (set->first_use (), prev_set);
	}

      // Now set has no uses, we can delete it.
      insn_change change (insn, insn_change::DELETE);
      //df_insn_rescan (insn->rtl());
      crtl->ssa->change_insn (change);
      insn = next;
    }
}

template<typename Map>
void
fusion_bb_info::traverse_base_map (Map &map)
{
  for (auto kv : map)
    {
      const auto &key = kv.first;
      auto &value = kv.second;
      transform_for_base (key.second, value);
    }
}

void
fusion_bb_info::transform ()
{
  traverse_base_map (expr_map);
  traverse_base_map (def_map);
}
