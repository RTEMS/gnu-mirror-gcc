class prop_iterator
{
public:
  bool has_next = true;

  prop_iterator (prop_stmt pstmt) : pstmt (pstmt)
    {
      if (pstmt.is_phi)
	{
	  /* PHI statement.  */
	  gphi* phi = as_a<gphi *> (pstmt.stmt);
	  op_num = gimple_phi_num_args (phi);
	}
      else if (gimple_assing_copy_p (pstmt))
	{
	  /* Simple copy statement.  */
	  op_num = 1;
	}
      else
	{
	  /* This would mean that pstmt isn't a prop statement.  */
	  gcc_unreachable ();
	}
    }

  tree get_next ()
    {
      gcc_assert (has_next);
      tree result = curr_op;
      update ();
      return result;
    }

private:
  prop_stmt pstmt;
  tree curr_op;
  unsigned op_num;
  unsigned curr_i = 0;

  void update ()
    {
      has_next = false;

      if (pstmt.is_phi)
	{
	  /* PHI statement.  */
	  gphi *phi = as_a<gphi *> (pstmt.stmt);
	  while (curr_i < op_num)
	    {
	      curr_op = gimple_phi_arg_def (phi, curr_i);
	      has_next = true;
	      curr_i++;
	    }
	}
      else
	{
	  /* Simple copy statement.  */
	  if (curr_i == 0)
	    {
	      curr_op = gimple_assign_rhs1 (pstmt.stmt);
	      has_next = true;
	      curr_i++;
	    }
	}
    }
}
