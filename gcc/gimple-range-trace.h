class trace_ranger : public loop_ranger
{
public:
  trace_ranger();
  virtual bool range_of_stmt (irange &r, gimple *s, tree name = NULL_TREE);
  virtual bool range_of_expr (irange &r, tree name, gimple *s = NULL);
  virtual void range_on_edge (irange &r, edge e, tree name);
  virtual void range_on_entry (irange &r, basic_block bb, tree name);
  virtual void range_on_exit (irange &r, basic_block bb, tree name);
private:
  static const unsigned bump = 2;
  unsigned indent;
  unsigned trace_count;		// Current trace index count.

  bool dumping (unsigned counter, bool trailing = false);
  bool trailer (unsigned counter, const char *caller, bool result, tree name,
		const irange &r);
};


// trace_ranger implementation.

trace_ranger::trace_ranger ()
{
  indent = 0;
  trace_count = 0;
}

// If dumping, return true and print the prefix for the next output line.

inline bool
trace_ranger::dumping (unsigned counter, bool trailing)
{
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      // Print counter index as well as INDENT spaces.
      if (!trailing)
	fprintf (dump_file, " %-7u ", counter);
      else
	fprintf (dump_file, "         ");
      unsigned x;
      for (x = 0; x< indent; x++)
	fputc (' ', dump_file);
      return true;
    }
  return false;
}

// After calling a routine, if dumping, print the CALLER, NAME, and RESULT,
// returning RESULT.

bool
trace_ranger::trailer (unsigned counter, const char *caller, bool result,
		       tree name, const irange &r)
{
  if (dumping (counter, true))
    {
      indent -= bump;
      fputs(result ? "TRUE : " : "FALSE : ", dump_file);
      fprintf (dump_file, "(%u) ", counter);
      fputs (caller, dump_file);
      fputs (" (",dump_file);
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") ",dump_file);
      if (result)
	{
	  r.dump (dump_file);
	  fputc('\n', dump_file);
	}
      else
	fputc('\n', dump_file);
      // Marks the end of a request.
      if (indent == 0)
	fputc('\n', dump_file);
    }
  return result;
}


// Tracing version of range_on_edge.  Call it with printing wrappers.

void
trace_ranger::range_on_edge (irange &r, edge e, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_edge (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") on edge %d->%d\n", e->src->index, e->dest->index);
      indent += bump;
    }

  loop_ranger::range_on_edge (r, e, name);

  trailer (idx, "range_on_edge", true, name, r);
}

// Tracing version of range_on_entry.  Call it with printing wrappers.

void
trace_ranger::range_on_entry (irange &r, basic_block bb, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_entry (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") to BB %d\n", bb->index);
      indent += bump;
    }

  gimple_ranger::range_on_entry (r, bb, name);

  trailer (idx, "range_on_entry", true, name, r);
}

// Tracing version of range_on_exit.  Call it with printing wrappers.

void
trace_ranger::range_on_exit (irange &r, basic_block bb, tree name)
{
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_on_exit (");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fprintf (dump_file, ") from BB %d\n", bb->index);
      indent += bump;
    }

  gimple_ranger::range_on_exit (r, bb, name);

  trailer (idx, "range_on_exit", true, name, r);
}

// Tracing version of range_of_stmt.  Call it with printing wrappers.

bool
trace_ranger::range_of_stmt (irange &r, gimple *s, tree name)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_stmt (");
      if (name)
	print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (") at stmt ", dump_file);
      print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
      indent += bump;
    }

  res = loop_ranger::range_of_stmt (r, s, name);

  return trailer (idx, "range_of_stmt", res, name, r);
}

// Tracing version of range_of_expr.  Call it with printing wrappers.

bool
trace_ranger::range_of_expr (irange &r, tree name, gimple *s)
{
  bool res;
  unsigned idx = ++trace_count;
  if (dumping (idx))
    {
      fprintf (dump_file, "range_of_expr(");
      print_generic_expr (dump_file, name, TDF_SLIM);
      fputs (")", dump_file);
      if (s)
        {
	  fputs (" at stmt ", dump_file);
	  print_gimple_stmt (dump_file, s, 0, TDF_SLIM);
	}
      else
        fputs ("\n", dump_file);
      indent += bump;
    }

  res = gimple_ranger::range_of_expr (r, name, s);

  return trailer (idx, "range_of_stmt", res, name, r);
}

