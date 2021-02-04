/* Command line option handling.  Code involving global state that
   should not be shared with the driver.
   Copyright (C) 2002-2021 Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "rtl.h"
#include "tree.h"
#include "tree-pass.h"
#include "diagnostic.h"
#include "opts.h"
#include "flags.h"
#include "langhooks.h"
#include "dbgcnt.h"
#include "debug.h"
#include "output.h"
#include "plugin.h"
#include "toplev.h"
#include "context.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "file-prefix-map.h" /* add_*_prefix_map()  */
#include "target.h"

typedef const char *const_char_p; /* For DEF_VEC_P.  */

static vec<const_char_p> ignored_options;

/* Input file names.  */
const char **in_fnames;
unsigned num_in_fnames;

/* Return a malloced slash-separated list of languages in MASK.  */

char *
write_langs (unsigned int mask)
{
  unsigned int n = 0, len = 0;
  const char *lang_name;
  char *result;

  for (n = 0; (lang_name = lang_names[n]) != 0; n++)
    if (mask & (1U << n))
      len += strlen (lang_name) + 1;

  result = XNEWVEC (char, len);
  len = 0;
  for (n = 0; (lang_name = lang_names[n]) != 0; n++)
    if (mask & (1U << n))
      {
	if (len)
	  result[len++] = '/';
	strcpy (result + len, lang_name);
	len += strlen (lang_name);
      }

  result[len] = 0;

  return result;
}

/* Complain that switch DECODED does not apply to this front end (mask
   LANG_MASK).  */

static void
complain_wrong_lang (const struct cl_decoded_option *decoded,
		     unsigned int lang_mask)
{
  const struct cl_option *option = &cl_options[decoded->opt_index];
  const char *text = decoded->orig_option_with_args_text;
  char *ok_langs = NULL, *bad_lang = NULL;
  unsigned int opt_flags = option->flags;

  if (!lang_hooks.complain_wrong_lang_p (option))
    return;

  opt_flags &= ((1U << cl_lang_count) - 1) | CL_DRIVER;
  if (opt_flags != CL_DRIVER)
    ok_langs = write_langs (opt_flags);
  if (lang_mask != CL_DRIVER)
    bad_lang = write_langs (lang_mask);

  if (opt_flags == CL_DRIVER)
    error ("command-line option %qs is valid for the driver but not for %s",
	   text, bad_lang);
  else if (lang_mask == CL_DRIVER)
    gcc_unreachable ();
  else if (ok_langs[0] != '\0')
    /* Eventually this should become a hard error IMO.  */
    warning (0, "command-line option %qs is valid for %s but not for %s",
	     text, ok_langs, bad_lang);
  else
    /* Happens for -Werror=warning_name.  */
    warning (0, "%<-Werror=%> argument %qs is not valid for %s",
	     text, bad_lang);

  free (ok_langs);
  free (bad_lang);
}

/* Buffer the unknown option described by the string OPT.  Currently,
   we only complain about unknown -Wno-* options if they may have
   prevented a diagnostic. Otherwise, we just ignore them.  Note that
   if we do complain, it is only as a warning, not an error; passing
   the compiler an unrecognized -Wno-* option should never change
   whether the compilation succeeds or fails.  */

static void
postpone_unknown_option_warning (const char *opt)
{
  ignored_options.safe_push (opt);
}

/* Produce a warning for each option previously buffered.  */

void
print_ignored_options (void)
{
  while (!ignored_options.is_empty ())
    {
      const char *opt;

      opt = ignored_options.pop ();
      /* Use inform, not warning_at, to avoid promoting these to errors.  */
      inform (UNKNOWN_LOCATION,
	      "unrecognized command-line option %qs may have been intended "
	      "to silence earlier diagnostics", opt);
    }
}

/* Handle an unknown option DECODED, returning true if an error should
   be given.  */

static bool
unknown_option_callback (const struct cl_decoded_option *decoded)
{
  const char *opt = decoded->arg;

  if (opt[1] == 'W' && opt[2] == 'n' && opt[3] == 'o' && opt[4] == '-'
      && !(decoded->errors & CL_ERR_NEGATIVE))
    {
      /* We don't generate warnings for unknown -Wno-* options unless
	 we issue diagnostics.  */
      postpone_unknown_option_warning (opt);
      return false;
    }
  else
    return true;
}

/* Handle a front-end option; arguments and return value as for
   handle_option.  */

static bool
lang_handle_option (struct gcc_options *opts,
		    struct gcc_options *opts_set,
		    const struct cl_decoded_option *decoded,
		    unsigned int lang_mask ATTRIBUTE_UNUSED, int kind,
		    location_t loc,
		    const struct cl_option_handlers *handlers,
		    diagnostic_context *dc,
		    void (*) (void))
{
  gcc_assert (opts == &global_options);
  gcc_assert (opts_set == &global_options_set);
  gcc_assert (dc == global_dc);
  gcc_assert (decoded->canonical_option_num_elements <= 2);
  return lang_hooks.handle_option (decoded->opt_index, decoded->arg,
				   decoded->value, kind, loc, handlers);
}

/* Handle FILENAME from the command line.  */

static void
add_input_filename (const char *filename)
{
  num_in_fnames++;
  in_fnames = XRESIZEVEC (const char *, in_fnames, num_in_fnames);
  in_fnames[num_in_fnames - 1] = filename;
}

/* Handle the vector of command line options (located at LOC), storing
   the results of processing DECODED_OPTIONS and DECODED_OPTIONS_COUNT
   in OPTS and OPTS_SET and using DC for diagnostic state.  LANG_MASK
   contains has a single bit set representing the current language.
   HANDLERS describes what functions to call for the options.  */

static void
read_cmdline_options (struct gcc_options *opts, struct gcc_options *opts_set,
		      struct cl_decoded_option *decoded_options,
		      unsigned int decoded_options_count,
		      location_t loc,
		      unsigned int lang_mask,
		      const struct cl_option_handlers *handlers,
		      diagnostic_context *dc)
{
  unsigned int i;

  for (i = 1; i < decoded_options_count; i++)
    {
      if (decoded_options[i].opt_index == OPT_SPECIAL_input_file)
	{
	  /* Input files should only ever appear on the main command
	     line.  */
	  gcc_assert (opts == &global_options);
	  gcc_assert (opts_set == &global_options_set);

	  if (opts->x_main_input_filename == NULL)
	    {
	      opts->x_main_input_filename = decoded_options[i].arg;
	      opts->x_main_input_baselength
		= base_of_path (opts->x_main_input_filename,
				&opts->x_main_input_basename);
	    }
	  add_input_filename (decoded_options[i].arg);
	  continue;
	}

      read_cmdline_option (opts, opts_set,
			   decoded_options + i, loc, lang_mask, handlers,
			   dc);
    }
}

/* Language mask determined at initialization.  */
static unsigned int initial_lang_mask;

/* Initialize global options-related settings at start-up.  */

void
init_options_once (void)
{
  /* Perform language-specific options initialization.  */
  initial_lang_mask = lang_hooks.option_lang_mask ();

  lang_hooks.initialize_diagnostics (global_dc);
  /* ??? Ideally, we should do this earlier and the FEs will override
     it if desired (none do it so far).  However, the way the FEs
     construct their pretty-printers means that all previous settings
     are overriden.  */
  diagnostic_color_init (global_dc);
  diagnostic_urls_init (global_dc);
}

/* Decode command-line options to an array, like
   decode_cmdline_options_to_array and with the same arguments but
   using the default lang_mask.  */

void
decode_cmdline_options_to_array_default_mask (unsigned int argc,
					      const char **argv, 
					      struct cl_decoded_option **decoded_options,
					      unsigned int *decoded_options_count)
{
  decode_cmdline_options_to_array (argc, argv,
				   initial_lang_mask | CL_COMMON | CL_TARGET,
				   decoded_options, decoded_options_count);
}

/* Set *HANDLERS to the default set of option handlers for use in the
   compilers proper (not the driver).  */
void
set_default_handlers (struct cl_option_handlers *handlers,
		      void (*target_option_override_hook) (void))
{
  handlers->unknown_option_callback = unknown_option_callback;
  handlers->wrong_lang_callback = complain_wrong_lang;
  handlers->target_option_override_hook = target_option_override_hook;
  handlers->num_handlers = 3;
  handlers->handlers[0].handler = lang_handle_option;
  handlers->handlers[0].mask = initial_lang_mask;
  handlers->handlers[1].handler = common_handle_option;
  handlers->handlers[1].mask = CL_COMMON;
  handlers->handlers[2].handler = target_handle_option;
  handlers->handlers[2].mask = CL_TARGET;
}

/* Parse command line options and set default flag values.  Do minimal
   options processing.  The decoded options are in *DECODED_OPTIONS
   and *DECODED_OPTIONS_COUNT; settings go in OPTS, OPTS_SET and DC;
   the options are located at LOC.  */
void
decode_options (struct gcc_options *opts, struct gcc_options *opts_set,
		struct cl_decoded_option *decoded_options,
		unsigned int decoded_options_count,
		location_t loc, diagnostic_context *dc,
		void (*target_option_override_hook) (void))
{
  struct cl_option_handlers handlers;

  unsigned int lang_mask;

  lang_mask = initial_lang_mask;

  set_default_handlers (&handlers, target_option_override_hook);

  default_options_optimization (opts, opts_set,
				decoded_options, decoded_options_count,
				loc, lang_mask, &handlers, dc);

  read_cmdline_options (opts, opts_set,
			decoded_options, decoded_options_count,
			loc, lang_mask,
			&handlers, dc);

  finish_options (opts, opts_set, loc);

  /* Print --help=* if used.  */
  unsigned i;
  const char *arg;

  if (!help_option_arguments.is_empty ())
    {
      /* Make sure --help=* sees the overridden values.  */
      target_option_override_hook ();

      FOR_EACH_VEC_ELT (help_option_arguments, i, arg)
	print_help (opts, lang_mask, arg);
    }
}

/* Hold command-line options associated with stack limitation.  */
const char *opt_fstack_limit_symbol_arg = NULL;
int opt_fstack_limit_register_no = -1;

/* Process common options that have been deferred until after the
   handlers have been called for all options.  */

void
handle_common_deferred_options (void)
{
  unsigned int i;
  cl_deferred_option *opt;
  vec<cl_deferred_option> v;

  if (common_deferred_options)
    v = *((vec<cl_deferred_option> *) common_deferred_options);
  else
    v = vNULL;

  if (flag_dump_all_passed)
    enable_rtl_dump_file ();

  if (flag_opt_info)
    opt_info_switch_p (NULL);

  FOR_EACH_VEC_ELT (v, i, opt)
    {
      switch (opt->opt_index)
	{
	case OPT_fcall_used_:
	  fix_register (opt->arg, 0, 1);
	  break;

	case OPT_fcall_saved_:
	  fix_register (opt->arg, 0, 0);
	  break;

	case OPT_fdbg_cnt_:
	  dbg_cnt_process_opt (opt->arg);
	  break;

	case OPT_fdebug_prefix_map_:
	  add_debug_prefix_map (opt->arg);
	  break;

	case OPT_ffile_prefix_map_:
	  add_file_prefix_map (opt->arg);
	  break;

	case OPT_fdump_:
	  g->get_dumps ()->dump_switch_p (opt->arg);
	  break;

        case OPT_fopt_info_:
	  if (!opt_info_switch_p (opt->arg))
	    error ("unrecognized command-line option %<-fopt-info-%s%>",
                   opt->arg);
          break;

	case OPT_fenable_:
	case OPT_fdisable_:
	  if (opt->opt_index == OPT_fenable_)
	    enable_pass (opt->arg);
          else
	    disable_pass (opt->arg);
          break;

	case OPT_ffixed_:
	  /* Deferred.  */
	  fix_register (opt->arg, 1, 1);
	  break;

	case OPT_fplugin_:
#ifdef ENABLE_PLUGIN
	  add_new_plugin (opt->arg);
#else
	  error ("plugin support is disabled; configure with --enable-plugin");
#endif
	  break;

	case OPT_fplugin_arg_:
#ifdef ENABLE_PLUGIN
	  parse_plugin_arg_opt (opt->arg);
#else
	  error ("plugin support is disabled; configure with --enable-plugin");
#endif
	  break;

	case OPT_frandom_seed:
	  /* The real switch is -fno-random-seed.  */
	  if (!opt->value)
	    set_random_seed (NULL);
	  break;

	case OPT_frandom_seed_:
	  set_random_seed (opt->arg);
	  break;

	case OPT_fstack_limit:
	  /* The real switch is -fno-stack-limit.  */
	  if (!opt->value)
	    stack_limit_rtx = NULL_RTX;
	  break;

	case OPT_fstack_limit_register_:
	  {
	    int reg = decode_reg_name (opt->arg);
	    if (reg < 0)
	      error ("unrecognized register name %qs", opt->arg);
	    else
	      {
		/* Deactivate previous OPT_fstack_limit_symbol_ options.  */
		opt_fstack_limit_symbol_arg = NULL;
		opt_fstack_limit_register_no = reg;
	      }
	  }
	  break;

	case OPT_fstack_limit_symbol_:
	  /* Deactivate previous OPT_fstack_limit_register_ options.  */
	  opt_fstack_limit_register_no = -1;
	  opt_fstack_limit_symbol_arg = opt->arg;
	  break;

	case OPT_fasan_shadow_offset_:
	  if (!(flag_sanitize & SANITIZE_KERNEL_ADDRESS))
	    error ("%<-fasan-shadow-offset%> should only be used "
		   "with %<-fsanitize=kernel-address%>");
	  if (!set_asan_shadow_offset (opt->arg))
	     error ("unrecognized shadow offset %qs", opt->arg);
	  break;

	case OPT_fsanitize_sections_:
	  set_sanitized_sections (opt->arg);
	  break;

	default:
	  gcc_unreachable ();
	}
    }
}

/* Arguments being collected for optimization.  */
typedef const char *const_char_p;		/* For DEF_VEC_P.  */
static GTY(()) vec<const_char_p, va_gc> *optimize_args;


/* Inner function to convert a TREE_LIST to argv string to parse the optimize
   options in ARGS.  ATTR_P is true if this is for attribute(optimize), and
   false for #pragma GCC optimize.  */

void
parse_optimize_and_target_options (gcc_options *opts,
				   gcc_options *opts_set,
				   tree args, bool attr_p, bool is_target)
{
  unsigned opt_argc;
  unsigned i;
  const char **opt_argv;
  struct cl_decoded_option *decoded_options;
  unsigned int decoded_options_count;
  tree ap;
  const char *attr_name = is_target ? "target" : "optimize";

  /* Build up argv vector.  Just in case the string is stored away, use garbage
     collected strings.  */
  vec_safe_truncate (optimize_args, 0);
  vec_safe_push (optimize_args, (const char *) NULL);
  bool is_string = TREE_CODE (args) == STRING_CST;

  for (ap = args; ap != NULL_TREE; ap = is_string ? NULL : TREE_CHAIN (ap))
    {
      tree value = is_string ? ap : TREE_VALUE (ap);

      if (TREE_CODE (value) == INTEGER_CST)
	{
	  char buffer[20];
	  sprintf (buffer, "-O%ld", (long) TREE_INT_CST_LOW (value));
	  vec_safe_push (optimize_args, ggc_strdup (buffer));
	}

      else if (TREE_CODE (value) == STRING_CST)
	{
	  /* Split string into multiple substrings.  */
	  size_t len = TREE_STRING_LENGTH (value);
	  char *p = ASTRDUP (TREE_STRING_POINTER (value));
	  char *end = p + len;
	  char *comma;
	  char *next_p = p;

	  while (next_p != NULL)
	    {
	      size_t len2;
	      char *q, *r;

	      p = next_p;
	      comma = strchr (p, ',');
	      if (comma)
		{
		  len2 = comma - p;
		  *comma = '\0';
		  next_p = comma+1;
		}
	      else
		{
		  len2 = end - p;
		  next_p = NULL;
		}

	      /* If the user supplied -Oxxx or -fxxx, only allow -Oxxx or -fxxx
		 options.  */
	      // TODO: opt/target - distinguish
	      if (*p == '-' && p[1] != 'O' && (p[1] != 'f' && p[1] != 'm'))
		{
		  if (attr_p)
		    warning (OPT_Wattributes,
			     "bad option %qs to attribute %qs", p, attr_name);
		  else
		    warning (OPT_Wpragmas,
			     "bad option %qs to pragma %qs", p, attr_name);
		  continue;
		}

	      /* Can't use GC memory here, see PR88007.  */
	      r = q = XOBNEWVEC (&opts_obstack, char, len2 + 3);

	      if (*p != '-')
		{
		  *r++ = '-';

		  /* Assume that Ox is -Ox, a numeric value is -Ox, a s by
		     itself is -Os, and any other switch begins with a -f.  */
		  if (!is_target
		      && ((*p >= '0' && *p <= '9')
			  || (p[0] == 's' && p[1] == '\0')))
		    *r++ = 'O';
		  else if (*p != 'O')
		    *r++ = is_target ? 'm' : 'f';
		}

	      memcpy (r, p, len2);
	      r[len2] = '\0';
	      vec_safe_push (optimize_args, (const char *) q);
	    }

	}
    }

  opt_argc = optimize_args->length ();
  opt_argv = (const char **) alloca (sizeof (char *) * (opt_argc + 1));

  for (i = 1; i < opt_argc; i++)
    opt_argv[i] = (*optimize_args)[i];

  /* Now parse the options.  */
  decode_cmdline_options_to_array_default_mask (opt_argc, opt_argv,
						&decoded_options,
						&decoded_options_count);
  /* Drop non-Optimization options.  */
  unsigned j = 1;
  for (i = 1; i < decoded_options_count; ++i)
    {
      unsigned int mask = is_target ? CL_TARGET : CL_OPTIMIZATION;
      if (! (cl_options[decoded_options[i].opt_index].flags & mask))
	{
	  if (attr_p)
	    warning (OPT_Wattributes,
		     "bad option %qs to attribute %qs",
		     decoded_options[i].orig_option_with_args_text, attr_name);
	  else
	    warning (OPT_Wpragmas,
		     "bad option %qs to pragma %qs",
		     decoded_options[i].orig_option_with_args_text, attr_name);
	  continue;
	}
      if (i != j)
	decoded_options[j] = decoded_options[i];
      j++;
    }
  decoded_options_count = j;
  /* And apply them.  */
  decode_options (opts, opts_set,
		  decoded_options, decoded_options_count,
		  input_location, global_dc, NULL);
  free (decoded_options);

  targetm.override_options_after_change();

  optimize_args->truncate (0);
}

#include "gt-opts-global.h"
