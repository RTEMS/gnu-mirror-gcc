#!/usr/bin/env python3

# Copyright (C) 2020 Free Software Foundation, Inc.
#
# This file is part of GCC.
#
# GCC is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.
#
# GCC is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with GCC; see the file COPYING.  If not, write to
# the Free Software Foundation, 51 Franklin Street, Fifth Floor,
# Boston, MA 02110-1301, USA.

# This script parses a .diff file generated with 'diff -up' or 'diff -cp'
# and adds a skeleton ChangeLog file to the file. It does not try to be
# too smart when parsing function names, but it produces a reasonable
# approximation.
#
# Author: Martin Liska <mliska@suse.cz>

import unittest

from mklog_ng import generate_changelog

PATCH1 = '''\
diff --git a/gcc/config/riscv/riscv.h b/gcc/config/riscv/riscv.h
index 567c23380fe..e6209ede9d6 100644
--- a/gcc/config/riscv/riscv.h
+++ b/gcc/config/riscv/riscv.h
@@ -920,6 +920,7 @@ extern unsigned riscv_stack_boundary;
 #define SHIFT_RS1 15
 #define SHIFT_IMM 20
 #define IMM_BITS 12
+#define C_S_BITS 5
 #define C_SxSP_BITS 6
 
 #define IMM_REACH (1LL << IMM_BITS)
@@ -929,6 +930,10 @@ extern unsigned riscv_stack_boundary;
 #define SWSP_REACH (4LL << C_SxSP_BITS)
 #define SDSP_REACH (8LL << C_SxSP_BITS)
 
+/* This is the maximum value that can be represented in a compressed load/store
+   offset (an unsigned 5-bit value scaled by 4).  */
+#define CSW_MAX_OFFSET ((4LL << C_S_BITS) - 1) & ~3
+
 /* Called from RISCV_REORG, this is defined in riscv-sr.c.  */
 
 extern void riscv_remove_unneeded_save_restore_calls (void);

'''

EXPECTED1 = '''\
gcc/ChangeLog:

	* config/riscv/riscv.h (C_S_BITS):
	(CSW_MAX_OFFSET):
'''

PATCH2 = '''\
diff --git a/gcc/targhooks.h b/gcc/targhooks.h
index 9704d23f1db..b572a36e8cf 100644
--- a/gcc/targhooks.h
+++ b/gcc/targhooks.h
@@ -120,7 +120,7 @@ extern bool default_empty_mask_is_expensive (unsigned);
 extern void *default_init_cost (class loop *);
 extern unsigned default_add_stmt_cost (class vec_info *, void *, int,
 				       enum vect_cost_for_stmt,
-				       class _stmt_vec_info *, int,
+				       class _stmt_vec_info *, tree, int,
 				       enum vect_cost_model_location);
 extern void default_finish_cost (void *, unsigned *, unsigned *, unsigned *);
 extern void default_destroy_cost_data (void *);
@@ -186,6 +186,7 @@ extern tree default_emutls_var_init (tree, tree, tree);
 extern unsigned int default_hard_regno_nregs (unsigned int, machine_mode);
 extern bool default_hard_regno_scratch_ok (unsigned int);
 extern bool default_mode_dependent_address_p (const_rtx, addr_space_t);
+extern bool default_new_address_profitable_p (rtx, rtx_insn *, rtx);
 extern bool default_target_option_valid_attribute_p (tree, tree, tree, int);
 extern bool default_target_option_pragma_parse (tree, tree);
 extern bool default_target_can_inline_p (tree, tree);

'''

EXPECTED2 = '''\
gcc/ChangeLog:

	* targhooks.h (default_add_stmt_cost):
	(default_new_address_profitable_p):
'''

PATCH3 = '''\
diff --git a/libcpp/include/cpplib.h b/libcpp/include/cpplib.h
index 2b1e33f94ae..7f47402f9b9 100644
--- a/libcpp/include/cpplib.h
+++ b/libcpp/include/cpplib.h
@@ -173,7 +173,7 @@ enum c_lang {CLK_GNUC89 = 0, CLK_GNUC99, CLK_GNUC11, CLK_GNUC17, CLK_GNUC2X,
 	     CLK_STDC2X,
 	     CLK_GNUCXX, CLK_CXX98, CLK_GNUCXX11, CLK_CXX11,
 	     CLK_GNUCXX14, CLK_CXX14, CLK_GNUCXX17, CLK_CXX17,
-	     CLK_GNUCXX2A, CLK_CXX2A, CLK_ASM};
+	     CLK_GNUCXX20, CLK_CXX20, CLK_ASM};
 
 /* Payload of a NUMBER, STRING, CHAR or COMMENT token.  */
 struct GTY(()) cpp_string {
@@ -484,7 +484,7 @@ struct cpp_options
   /* Nonzero for C2X decimal floating-point constants.  */
   unsigned char dfp_constants;
 
-  /* Nonzero for C++2a __VA_OPT__ feature.  */
+  /* Nonzero for C++20 __VA_OPT__ feature.  */
   unsigned char va_opt;
 
   /* Nonzero for the '::' token.  */

'''

EXPECTED3 = '''\
libcpp/ChangeLog:

	* include/cpplib.h (enum c_lang):
	(struct cpp_options):
'''

class TestMklog(unittest.TestCase):
    def test_macro_definition(self):
        changelog = generate_changelog(PATCH1)
        assert changelog == EXPECTED1

    def test_changed_argument(self):
        changelog = generate_changelog(PATCH2)
        assert changelog == EXPECTED2

    def test_enum_and_struct(self):
        changelog = generate_changelog(PATCH3)
        assert changelog == EXPECTED3
