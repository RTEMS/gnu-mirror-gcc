/* Communication between registering jump thread requests and
   updating the SSA/CFG for jump threading.
   Copyright (C) 2013-2020 Free Software Foundation, Inc.

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

#ifndef _TREE_SSA_THREADUPDATE_H
#define _TREE_SSA_THREADUPDATE_H 1

/* In tree-ssa-threadupdate.c.  */
enum jump_thread_edge_type
{
  EDGE_START_JUMP_THREAD,
  EDGE_FSM_THREAD,
  EDGE_COPY_SRC_BLOCK,
  EDGE_COPY_SRC_JOINER_BLOCK,
  EDGE_NO_COPY_SRC_BLOCK
};

class jump_thread_edge
{
public:
  jump_thread_edge (edge e, enum jump_thread_edge_type type)
    : e (e), type (type) {}

  edge e;
  enum jump_thread_edge_type type;
};

/* Rather than search all the edges in jump thread paths each time
   DOM is able to simply if control statement, we build a hash table
   with the deleted edges.  We only care about the address of the edge,
   not its contents.  */
struct removed_edges : nofree_ptr_hash<edge_def>
{
  static hashval_t hash (edge e) { return htab_hash_pointer (e); }
  static bool equal (edge e1, edge e2) { return e1 == e2; }
};

/*
class jump_thread_path
{
public:
  jump_thread_path ();
  void add (jump_thread_edge);	 // safe_push
  jump_thread_edge &operator[] (int i);
  void operator delete (void *); // delete_jump_thread_path

private:
  vec<jump_thread_edge *> m_path;
};
*/

class jump_thread_path_registry
{
public:
  jump_thread_path_registry ();
  ~jump_thread_path_registry ();
  bool thread_through_all_blocks (bool);
  void register_jump_thread (vec <class jump_thread_edge *> *);
  void remove_jump_threads_including (edge);
  void debug_paths ();

private:
  void debug_path (FILE *, int pathno);
  void mark_threaded_blocks (bitmap threaded_blocks);
  bool rewire_first_differing_edge (unsigned path_num, unsigned edge_num);
  void adjust_paths_after_duplication (unsigned curr_path_num);
  bool duplicate_thread_path (edge entry,
			      edge exit,
			      basic_block *region,
			      unsigned n_region,
			      unsigned current_path_no);

  // We keep the registered jump threading opportunities in this
  // vector as edge pairs (original_edge, target_edge).
  vec<vec<jump_thread_edge *> *> paths;

  hash_table<removed_edges> *m_removed_edges;
};

extern void delete_jump_thread_path (vec <class jump_thread_edge *> *);
extern unsigned int estimate_threading_killed_stmts (basic_block);

enum bb_dom_status
{
  /* BB does not dominate latch of the LOOP.  */
  DOMST_NONDOMINATING,
  /* The LOOP is broken (there is no path from the header to its latch.  */
  DOMST_LOOP_BROKEN,
  /* BB dominates the latch of the LOOP.  */
  DOMST_DOMINATING
};

enum bb_dom_status determine_bb_domination_status (class loop *, basic_block);

// In tree-ssa-dom.c
extern void free_dom_edge_info (edge);

#endif
