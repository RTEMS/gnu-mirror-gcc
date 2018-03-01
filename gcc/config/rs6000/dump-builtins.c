/* Include this into rs6000.c */

#define RS6000_BUILTIN_0(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_1(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_2(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_3(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_A(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_D(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_H(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_P(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_Q(ENUM, NAME, MASK, ATTR, ICODE)
#define RS6000_BUILTIN_X(ENUM, NAME, MASK, ATTR, ICODE) \
  { MASK, ICODE, NAME, ENUM },

static const struct builtin_description bdesc_special[] =
{
#include "rs6000-builtin.def"
};

#undef RS6000_BUILTIN_0
#undef RS6000_BUILTIN_1
#undef RS6000_BUILTIN_2
#undef RS6000_BUILTIN_3
#undef RS6000_BUILTIN_A
#undef RS6000_BUILTIN_D
#undef RS6000_BUILTIN_H
#undef RS6000_BUILTIN_P
#undef RS6000_BUILTIN_Q
#undef RS6000_BUILTIN_X


#define KELVIN_VERBOSE_BUFFER_LEN	512

static unsigned int
strcat_length (const char **names, int num_names)
{
  int total_chars = num_names - 1; /* for single-character separators */
  for (int i = 0; i < num_names; i++)
    total_chars += strlen (names[i]);
  total_chars++;		/* for final null terminator */
  return total_chars;
}

/* Mask to string, return string is valid only until next call to same
   function.  Not thread safe.  */
static const char *
m2s (HOST_WIDE_INT mask)
{
  static char buffer [KELVIN_VERBOSE_BUFFER_LEN];
  char *s = buffer;
  static HOST_WIDE_INT flag_values[] = {
    RS6000_BTM_MODULO,
    RS6000_BTM_ALTIVEC,
    RS6000_BTM_CMPB,
    RS6000_BTM_VSX,
    RS6000_BTM_P8_VECTOR,
    RS6000_BTM_P9_VECTOR,
    RS6000_BTM_P9_MISC,
    RS6000_BTM_CRYPTO,
    RS6000_BTM_HTM,
    RS6000_BTM_PAIRED,
    RS6000_BTM_FRE,
    RS6000_BTM_FRES,
    RS6000_BTM_FRSQRTE,
    RS6000_BTM_FRSQRTES,
    RS6000_BTM_POPCNTD,
    RS6000_BTM_CELL,
    RS6000_BTM_DFP,
    RS6000_BTM_HARD_FLOAT,
    RS6000_BTM_LDBL128,
    RS6000_BTM_64BIT,
    RS6000_BTM_FLOAT128,
    RS6000_BTM_FLOAT128_HW,
  };
  static const char *flag_names[] = {
    "RS6000_BTM_MODULO",
    "RS6000_BTM_ALTIVEC",
    "RS6000_BTM_CMPB",
    "RS6000_BTM_VSX",
    "RS6000_BTM_P8_VECTOR",
    "RS6000_BTM_P9_VECTOR",
    "RS6000_BTM_P9_MISC",
    "RS6000_BTM_CRYPTO",
    "RS6000_BTM_HTM",
    "RS6000_BTM_PAIRED",
    "RS6000_BTM_FRE",
    "RS6000_BTM_FRES",
    "RS6000_BTM_FRSQRTE",
    "RS6000_BTM_FRSQRTES",
    "RS6000_BTM_POPCNTD",
    "RS6000_BTM_CELL",
    "RS6000_BTM_DFP",
    "RS6000_BTM_HARD_FLOAT",
    "RS6000_BTM_LDBL128",
    "RS6000_BTM_64BIT",
    "RS6000_BTM_FLOAT128",
    "RS6000_BTM_FLOAT128_HW",
  };
  static int max_buf =
    strcat_length (flag_names, 
		   sizeof (flag_values) / sizeof (HOST_WIDE_INT));
  gcc_assert (max_buf <= KELVIN_VERBOSE_BUFFER_LEN);

  if (mask == 0)
    strcpy (s, "RS6000_BTM_ALWAYS");
  else
    for (unsigned int i = 0;
	 i < sizeof (flag_values) / sizeof (HOST_WIDE_INT); i++) {
      if (mask & flag_values[i]) {
	if (s > buffer)
	  *s++ = '|';
	strcpy (s, flag_names[i]);
	s += strlen (flag_names[i]);
      }
    }
  return buffer;
}

static const char *
a2s (unsigned int attribute)
{
  static char buffer [KELVIN_VERBOSE_BUFFER_LEN];
  char *s = buffer;

  unsigned int type_mask = attribute & RS6000_BTC_TYPE_MASK;
  const char *flag_name;

  switch (type_mask)
    {
    case RS6000_BTC_SPECIAL:
      flag_name = "RS6000_BTC_SPECIAL"; break;
    case RS6000_BTC_UNARY:
      flag_name = "RS6000_BTC_UNARY"; break;
    case RS6000_BTC_BINARY:
      flag_name = "RS6000_BTC_BINARY"; break;
    case RS6000_BTC_TERNARY:
      flag_name = "RS6000_BTC_TERNARY"; break;
    case RS6000_BTC_PREDICATE:
      flag_name = "RS6000_BTC_PREDICATE"; break;
    case RS6000_BTC_ABS:
      flag_name = "RS6000_BTC_ABS"; break;
    case RS6000_BTC_DST:
      flag_name = "RS6000_BTC_DST"; break;
    default:
      flag_name = "RS6000_BTC_<TYPE_UNKNOWN>"; break;
    }
  strcpy (s, flag_name);
  s += strlen (flag_name);

  unsigned int attribute_info = attribute & RS6000_BTC_ATTR_MASK;
  if (attribute_info == RS6000_BTC_MISC)
    {
      *s++ = '|';
      flag_name = "RS6000_BTC_MISC";
      strcpy (s, flag_name);
      s += strlen (flag_name);
    }
  else {
    if (attribute_info & RS6000_BTC_CONST)
      {
	*s++ = '|';
	flag_name = "RS6000_BTC_CONST";
	strcpy (s, flag_name);
	s += strlen (flag_name);
      }
    if (attribute_info & RS6000_BTC_PURE)
      {
	*s++ = '|';
	flag_name = "RS6000_BTC_PURE";
	strcpy (s, flag_name);
	s += strlen (flag_name);

      }
    if (attribute_info & RS6000_BTC_FP)
      {
	*s++ = '|';
	flag_name = "RS6000_BTC_FP";
	strcpy (s, flag_name);
	s += strlen (flag_name);
      }
  }

  unsigned int misc_attribute = attribute & RS6000_BTC_MISC_MASK;
  if (misc_attribute & RS6000_BTC_SPR)
    {
      *s++ = '|';
      flag_name = "RS6000_BTC_SPR";
      strcpy (s, flag_name);
      s += strlen (flag_name);

    }
  if (misc_attribute & RS6000_BTC_VOID)
    {
      *s++ = '|';
      flag_name = "RS6000_BTC_VOID";
      strcpy (s, flag_name);
      s += strlen (flag_name);
    }
  if (misc_attribute & RS6000_BTC_CR)
    {
      *s++ = '|';
      flag_name = "RS6000_BTC_CR";
      strcpy (s, flag_name);
      s += strlen (flag_name);
    }
  if (misc_attribute & RS6000_BTC_OVERLOADED)
    {
      *s++ = '|';
      flag_name = "RS6000_BTC_OVERLOADED";
      strcpy (s, flag_name);
      s += strlen (flag_name);
    }

  return buffer;
}

static const struct rs6000_builtin_info_type *
get_type (const int code)
{
  return &rs6000_builtin_info[code];
}

HOST_WIDE_INT
rs6000_get_builtin_mask (const int code)
{
  return rs6000_builtin_info[code].mask;
}


unsigned int
rs6000_get_builtin_attributes (const int code)
{
  return rs6000_builtin_info[code].attr;
}

static const char *
t2s (machine_mode type) {
  int type_code = (int) type;
  int ptr_flag;

  if (type_code < 0) {
    ptr_flag = 1;
    type_code *= -1;
  }
  else
    ptr_flag = 0;

  switch (type_code) {

  case E_VOIDmode:
    return ptr_flag? "void *": "void";

  case E_BLKmode:
    return ptr_flag? "blk_mode *": "blk_mode";

  case E_CCmode:
    /* condition codes? */
    return ptr_flag? "cc_mode *": "cc_mode";

  case E_CCUNSmode:
    return ptr_flag? "ccuns_mode *": "ccuns_mode";

  case E_CCFPmode:
    return ptr_flag? "ccfp_mode *": "ccfp_mode";

  case E_CCEQmode:
    return ptr_flag? "cceq_mode *": "cceq_mode";

  case E_BImode:
    /* binary digit (bit) mode? */
    return ptr_flag? "bi_mode *": "bi_mode";

  case E_QImode:
    return ptr_flag? "char *": "char";

  case E_HImode:
    return ptr_flag? "short *": "short";

  case E_SImode:
    return ptr_flag? "int *": "int";

  case E_DImode:
    return ptr_flag? "long long int *": "long long int";

  case E_TImode:
    return ptr_flag? "ti_mode *": "ti_mode";

  case E_PTImode:
    return ptr_flag? "pti_mode *": "pti_mode";

  case E_QQmode:
    return ptr_flag? "qq_mode *": "qq_mode";

  case E_HQmode:
    return ptr_flag? "hq_mode *": "hq_mode";

  case E_SQmode:
    return ptr_flag? "sq_mode *": "sq_mode";

  case E_DQmode:
    return ptr_flag? "dq_mode *": "dq_mode";

  case E_TQmode:
    return ptr_flag? "tq_mode *": "tq_mode";

  case E_UQQmode:
    return ptr_flag? "unsigned char *": "unsigned char";

  case E_UHQmode:
    return ptr_flag? "unsigned short *": "unsigned short";

  case E_USQmode:
    return ptr_flag? "unsigned int *": "unsigned int";

  case E_UDQmode:
    return ptr_flag? "unsigned long long int *": "unsigned long long int";

  case E_UTQmode:
    return ptr_flag? "utq_mode *": "utq_mode";

  case E_HAmode:
    return ptr_flag? "ha_mode *": "ha_mode";

  case E_SAmode:
    return ptr_flag? "sa_mode *": "sa_mode";

  case E_DAmode:
    return ptr_flag? "da_mode *": "da_mode";

  case E_TAmode:
    return ptr_flag? "ta_mode *": "ta_mode";

  case E_UHAmode:
    return ptr_flag? "uha_mode *": "uha_mode";

  case E_USAmode:
    return ptr_flag? "usa_mode *": "usa_mode";

  case E_UDAmode:
    return ptr_flag? "uda_mode *": "uda_mode";

  case E_UTAmode:
    return ptr_flag? "uta_mode *": "uta_mode";

  case E_SFmode:
    return ptr_flag? "float *": "float";

  case E_DFmode:
    return ptr_flag? "double *": "double";

  case E_IFmode:
    return ptr_flag? "__ibm128 *": "__ibm128";

  case E_KFmode:
    return ptr_flag? "__float128 *": "__float128";

  case E_TFmode:
    return ptr_flag? "tf_mode *": "tf_mode";

  case E_SDmode:
    return ptr_flag? "sd_mode *": "sd_mode";

  case E_DDmode:
    return ptr_flag? "dd_mode *": "dd_mode";

  case E_TDmode:
    return ptr_flag? "td_mode *": "td_mode";

  case E_CQImode:
    return ptr_flag? "cqi_mode *": "cqi_mode";

  case E_CHImode:
    return ptr_flag? "chi_mode *": "chi_mode";

  case E_CSImode:
    return ptr_flag? "csi_mode *": "csi_mode";

  case E_CDImode:
    return ptr_flag? "cdi_mode *": "cdi_mode";

  case E_CTImode:
    return ptr_flag? "cti_mode *": "cti_mode";

  case E_SCmode:
    return ptr_flag? "sc_mode *": "sc_mode";

  case E_DCmode:
    return ptr_flag? "dc_mode *": "dc_mode";

  case E_ICmode:
    return ptr_flag? "ic_mode *": "ic_mode";

  case E_KCmode:
    return ptr_flag? "kc_mode *": "kc_mode";

  case E_TCmode:
    return ptr_flag? "tc_mode *": "tc_mode";

  case E_V2SImode:
    return ptr_flag? "v2si_mode *": "v2si_mode";

  case E_V16QImode:
    return ptr_flag? "vector char *": "vector char";

  case E_V8HImode:
    return ptr_flag? "vector short *": "vector short";

  case E_V4SImode:
    return ptr_flag? "vector int *": "vector int";

  case E_V2DImode:
    return ptr_flag? "vector long long int *": "vector long long int";

  case E_V1TImode:
    return ptr_flag? "vector __int128_t *": "vector __int128_t";

  case E_V32QImode:
    return ptr_flag? "v32qi_mode *": "v32qi_mode";

  case E_V16HImode:
    return ptr_flag? "v16hi_mode *": "v16hi_mode";

  case E_V8SImode:
    return ptr_flag? "v8si_mode *": "v8si_mode";

  case E_V4DImode:
    return ptr_flag? "v4di_mode *": "v4di_mode";

  case E_V2TImode:
    return ptr_flag? "v2ti_mode *": "v2ti_mode";

  case E_V2SFmode:
    return ptr_flag? "v2sf_mode *": "v2sf_mode";

  case E_V4SFmode:
    return ptr_flag? "vector float *": "vector float";

  case E_V2DFmode:
    return ptr_flag? "vector double *": "vector double";

  case E_V2IFmode:
    return ptr_flag? "v2if_mode *": "v2if_mode";

  case E_V2KFmode:
    return ptr_flag? "v2kf_mode *": "v2kf_mode";

  case E_V8SFmode:
    return ptr_flag? "v8sf_mode *": "v8sf_mode";

  case E_V4DFmode:
    return ptr_flag? "v4df_mode *": "v4df_mode";

  case E_V2TFmode:
    return ptr_flag? "v2tf_mode *": "v2tf_mode";

  case MAX_MACHINE_MODE:
    return ptr_flag? "compile_time_type *": "compile_time_type";

  case MAX_MACHINE_MODE+1:
    return ptr_flag? "compile_time_vector_type *": "compile_time_vector_type";

  default:
    return ptr_flag? "unknown_mode *": "unknown_mode";
  }
}

static void
dump_one_table (const char *title,
		const struct builtin_description *bidp, int num_entries)
{
  machine_mode tmode, mode0, mode1, mode2;

  fprintf (stderr, "%s [%d]\n", title, num_entries);
  while (num_entries--) {
    unsigned attributes = get_type (bidp->code)->attr;
    unsigned type_mask = attributes & RS6000_BTC_TYPE_MASK;
    HOST_WIDE_INT builtin_mask = bidp->mask;

    if (((builtin_mask & RS6000_BTM_PAIRED) == 0) &&
	((attributes & RS6000_BTC_OVERLOADED) == 0))
      {
	switch (type_mask) {
	case RS6000_BTC_DST:
	  /* Expect ptr operand, int length, const int configuration
	     operands, with void result.  */
	case RS6000_BTC_TERNARY:
	  tmode = insn_data[bidp->icode].operand[0].mode;
	  mode0 = insn_data[bidp->icode].operand[1].mode;
	  mode1 = insn_data[bidp->icode].operand[2].mode;
	  mode2 = insn_data[bidp->icode].operand[3].mode;
	  fprintf (stderr, "%s %s (%s, %s, %s): ", t2s (tmode),
		   bidp->name, t2s (mode0), t2s (mode1), t2s (mode2));
	  break;

	case RS6000_BTC_PREDICATE:
	  /* An implicit integer predicate code and two source values.  */
	  if (builtin_mask & RS6000_BTM_PAIRED)
	    {
	      /* Special treatment in rs6000.c: paired_init_buitins */
	      mode0 = E_V2SFmode;
	      mode1 = E_V2SFmode;
	    }
	  else
	    {
	      mode0 = insn_data[bidp->icode].operand[1].mode;
	      mode1 = insn_data[bidp->icode].operand[2].mode;
	    }
	  fprintf (stderr, "int %s (int, %s, %s): ",
		   bidp->name, t2s (mode0), t2s (mode1));
	  break;

	case RS6000_BTC_BINARY:
	  tmode = insn_data[bidp->icode].operand[0].mode;
	  mode0 = insn_data[bidp->icode].operand[1].mode;
	  mode1 = insn_data[bidp->icode].operand[2].mode;
	  fprintf (stderr, "%s %s (%s, %s): ",
		   t2s (tmode), bidp->name, t2s (mode0), t2s (mode1));
	  break;

	case RS6000_BTC_ABS:	/* single argument, single result */
	case RS6000_BTC_UNARY:
	  tmode = insn_data[bidp->icode].operand[0].mode;
	  mode0 = insn_data[bidp->icode].operand[1].mode;
	  fprintf (stderr, "%s %s (%s): ",
		   t2s (tmode), bidp->name, t2s (mode0));
	  break;

	case RS6000_BTC_SPECIAL:
	  {
	    machine_mode operand_modes[4];
	    int num_operands = 0;

	    /* Built-in functions characterized with the
	       RS6000_BTC_SPECIAL have their type signatures defined
	       by special code executed during compiler startup rather
	       than by the standard tables.  */

	    switch (bidp->code) {

	      /* ignore cases associated with RS6000_BTM_PAIRED:
	       *
	       *  case PAIRED_BUILTIN_LX:
	       *    paired_init_builtins: v2sf_ftype_long_pcfloat 
	       *
	       *  case PAIRED_BUILTIN_STX:
	       *    paired_init_builtins: void_ftype_v2sf_long_pcfloat
	       *
	       *  case PAIRED_BUILTIN_STX:
	       *    from paired_expand_stv_builtin
	       *
	       *  case PAIRED_BUILTIN_LX:
	       *    from paired_expand_lv_builtin
	       */

	    case RS6000_BUILTIN_RECIP:
	      operand_modes[0] = DFmode;
	      operand_modes[1] = DFmode;
	      operand_modes[2] = DFmode;
	      num_operands = 3;
	      break;

	    case RS6000_BUILTIN_RECIPF:
	      operand_modes[0] = SFmode;
	      operand_modes[1] = SFmode;
	      operand_modes[2] = SFmode;
	      num_operands = 3;
	      break;

	    case RS6000_BUILTIN_RSQRT:
	      operand_modes[0] = DFmode;
	      operand_modes[1] = DFmode;
	      num_operands = 2;
	      break;

	    case RS6000_BUILTIN_RSQRTF:
	      operand_modes[0] = SFmode;
	      operand_modes[1] = SFmode;
	      num_operands = 2;
	      break;

	    case POWER7_BUILTIN_BPERMD:
	      /* -m32, these are SImode */
	      operand_modes[0] = DImode;
	      operand_modes[1] = DImode;
	      operand_modes[2] = DImode;
	      num_operands = 3;
	      break;

	    case RS6000_BUILTIN_GET_TB:
	      operand_modes[0] = E_USQmode;
	      num_operands = 1;
	      break;

	    case RS6000_BUILTIN_MFTB:
	      /* -m32 is SImode */
	      operand_modes[0] = E_UDQmode;
	      num_operands = 1;
	      break;

	    case RS6000_BUILTIN_MFFS:
	      operand_modes[0] = E_DFmode;
	      num_operands = 1;
	      break;

	    case RS6000_BUILTIN_MTFSF:
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[0] = E_SImode;
	      operand_modes[0] = E_DFmode;
	      num_operands = 3;
	      break;

	    case RS6000_BUILTIN_CPU_INIT:
	    case MISC_BUILTIN_SPEC_BARRIER:
	      operand_modes[0] = E_VOIDmode;
	      num_operands = 1;
	      break;

	    case RS6000_BUILTIN_CPU_IS:
	    case RS6000_BUILTIN_CPU_SUPPORTS:
	      operand_modes[0] = E_SImode;
	      operand_modes[1] = ~E_VOIDmode; /* pointer to void */
	      num_operands = 2;
	      break;

	    case ALTIVEC_BUILTIN_MTVSCR:
	      /* altivec_init_builtins: void_ftype_v4si */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_V4SImode;
	      num_operands = 2;
	      break;

	    case ALTIVEC_BUILTIN_MFVSCR:
	      /* altivec_init_builtins: v8hi_ftype_void */
	      operand_modes[0] = E_V8HImode;
	      num_operands = 1;
	      break;

	    case ALTIVEC_BUILTIN_DSSALL:
	      /* altivec_init_builtins: void_ftype_void */
	      operand_modes[0] = E_VOIDmode;
	      num_operands = 1;
	      break;

	    case ALTIVEC_BUILTIN_DSS:
	      /* altivec_init_builtins: void_ftype_int */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_SImode;
	      num_operands = 2;
	      break;

	    case ALTIVEC_BUILTIN_LVSL:
	    case ALTIVEC_BUILTIN_LVSR:
	    case ALTIVEC_BUILTIN_LVEBX:
	    case ALTIVEC_BUILTIN_LVXL_V16QI:
	    case ALTIVEC_BUILTIN_LVX_V16QI:
	    case ALTIVEC_BUILTIN_VEC_LVSL:
	    case ALTIVEC_BUILTIN_VEC_LVSR:
	    case ALTIVEC_BUILTIN_VEC_LVEBX:
	    case VSX_BUILTIN_LXVW4X_V16QI:
	    case VSX_BUILTIN_LD_ELEMREV_V16QI:
	    case ALTIVEC_BUILTIN_LVLX:
	    case ALTIVEC_BUILTIN_LVLXL:
	    case ALTIVEC_BUILTIN_LVRX:
	    case ALTIVEC_BUILTIN_LVRXL:
	    case ALTIVEC_BUILTIN_VEC_LVLX:
	    case ALTIVEC_BUILTIN_VEC_LVLXL:
	    case ALTIVEC_BUILTIN_VEC_LVRX:
	    case ALTIVEC_BUILTIN_VEC_LVRXL:
	      /* altivec_init_builtins: v16qi_ftype_long_pcvoid */
	      operand_modes[0] = E_V16QImode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_LVEHX:
	    case ALTIVEC_BUILTIN_LVXL_V8HI:
	    case ALTIVEC_BUILTIN_LVX_V8HI:
	    case ALTIVEC_BUILTIN_VEC_LVEHX:
	    case VSX_BUILTIN_LXVW4X_V8HI:
	    case VSX_BUILTIN_LD_ELEMREV_V8HI:
	      /* altivec_init_builtins: v8hi_ftype_long_pcvoid */
	      operand_modes[0] = E_V8HImode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_LVEWX:
	    case ALTIVEC_BUILTIN_LVXL:
	    case ALTIVEC_BUILTIN_LVXL_V4SI:
	    case ALTIVEC_BUILTIN_LVX:
	    case ALTIVEC_BUILTIN_LVX_V4SI:
	      /* altivec_init_builtins: v4si_ftype_long_pcvoid */
	      operand_modes[0] = E_V4SImode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_LVXL_V2DF:
	    case ALTIVEC_BUILTIN_LVX_V2DF:
	    case VSX_BUILTIN_LXVD2X_V2DF:
	    case VSX_BUILTIN_LD_ELEMREV_V2DF:
	      /* altivec_init_builtins: v2df_ftype_long_pcvoid */
	      operand_modes[0] = E_V2DFmode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_LVXL_V2DI:
	    case ALTIVEC_BUILTIN_LVX_V2DI:
	    case VSX_BUILTIN_LXVD2X_V2DI:
	    case VSX_BUILTIN_LD_ELEMREV_V2DI:
	      /* altivec_init_builtins: v2di_ftype_long_pcvoid */
	      operand_modes[0] = E_V2DImode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_LVXL_V4SF:
	    case ALTIVEC_BUILTIN_LVX_V4SF:
	    case VSX_BUILTIN_LXVW4X_V4SF:
	    case VSX_BUILTIN_LD_ELEMREV_V4SF:
	      /* altivec_init_builtins: v4sf_ftype_long_pcvoid */
	      operand_modes[0] = E_V4SFmode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_STVX:
	    case ALTIVEC_BUILTIN_STVX_V4SI:
	    case ALTIVEC_BUILTIN_STVEWX:
	    case ALTIVEC_BUILTIN_STVXL:
	    case ALTIVEC_BUILTIN_STVXL_V4SI:
	    case VSX_BUILTIN_STXVW4X_V4SI:
	    case VSX_BUILTIN_ST_ELEMREV_V4SI:
	      /* altivec_init_builtins: void_ftype_v4si_long_pvoid */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_V4SImode;
	      /* SImode on -m32 */
	      operand_modes[2] = E_DImode;
	      operand_modes[3] = ~E_VOIDmode;
	      num_operands = 4;
	      break;

	    case ALTIVEC_BUILTIN_STVX_V2DF:
	    case ALTIVEC_BUILTIN_STVXL_V2DF:
	    case VSX_BUILTIN_STXVD2X_V2DF:
	    case VSX_BUILTIN_ST_ELEMREV_V2DF:
	      /* altivec_init_builtins: void_ftype_v2df_long_pvoid */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_V2DFmode;
	      /* SImode on -m32 */
	      operand_modes[2] = E_DImode;
	      operand_modes[3] = ~E_VOIDmode;
	      num_operands = 4;
	      break;

	    case ALTIVEC_BUILTIN_STVX_V2DI:
	    case ALTIVEC_BUILTIN_STVXL_V2DI:
	    case VSX_BUILTIN_STXVD2X_V2DI:
	    case VSX_BUILTIN_ST_ELEMREV_V2DI:
	      /* altivec_init_builtins: void_ftype_v2di_long_pvoid */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_V2DFmode;
	      /* SImode on -m32 */
	      operand_modes[2] = E_DImode;
	      operand_modes[3] = ~E_VOIDmode;
	      num_operands = 4;
	      break;

	    case ALTIVEC_BUILTIN_STVX_V4SF:
	    case ALTIVEC_BUILTIN_STVXL_V4SF:
	    case VSX_BUILTIN_STXVW4X_V4SF:
	    case VSX_BUILTIN_ST_ELEMREV_V4SF:
	      /* altivec_init_builtins: void_ftype_v4sf_long_pvoid */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_V4SFmode;
	      /* SImode on -m32 */
	      operand_modes[2] = E_DImode;
	      operand_modes[3] = ~E_VOIDmode;
	      num_operands = 4;
	      break;

	    case ALTIVEC_BUILTIN_STVX_V8HI:
	    case ALTIVEC_BUILTIN_STVXL_V8HI:
	    case ALTIVEC_BUILTIN_STVEHX:
	      /* altivec_init_builtins: void_ftype_v8hi_long_pvoid */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_V8HImode;
	      /* SImode on -m32 */
	      operand_modes[2] = E_DImode;
	      operand_modes[3] = ~E_VOIDmode;
	      num_operands = 4;
	      break;

	    case ALTIVEC_BUILTIN_STVX_V16QI:
	    case ALTIVEC_BUILTIN_STVXL_V16QI:
	    case ALTIVEC_BUILTIN_STVEBX:
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = E_V16QImode;
	      /* SImode on -m32 */
	      operand_modes[2] = E_DImode;
	      operand_modes[3] = ~E_VOIDmode;
	      num_operands = 4;
	      break;

	    case ALTIVEC_BUILTIN_VEC_LD:
	    case ALTIVEC_BUILTIN_VEC_LDE:
	    case ALTIVEC_BUILTIN_VEC_LDL:
	      /* altivec_init_builtins: opaque_ftype_long_pcvoid */

	      /* opaque apparently stands for "opaque_V4SI", which is
	       * the result of making an "opaque_vector_type" out of
	       * an intSI_type_node.  I believe an opaque vector is a
	       * vector that is known to the back-end, but is not
	       * known to the target-independent portions of the GCC
	       * compiler.  Since the type is not known to the GCC
	       * compiler, the GCC compiler cannot "manipulate" this
	       * subtree.  */
	      operand_modes[0] = E_V4SImode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      /* Methinks pcvoid means "const *void".  I'll ignore
		 the const qualifier for now.  */
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_VEC_LVEWX:
	    case VSX_BUILTIN_LXVW4X_V4SI:
	    case VSX_BUILTIN_LD_ELEMREV_V4SI:
	      /* altivec_init_builtins: v4si_ftype_long_pcvoid */
	      operand_modes[0] = E_V4SImode;
	      /* SImode on -m32 */
	      operand_modes[1] = E_DImode;
	      /* Methinks pcvoid means "const *void".  I'll ignore
		 the const qualifier for now.  */
	      operand_modes[2] = ~E_VOIDmode;
	      num_operands = 3;
	      break;

	    case ALTIVEC_BUILTIN_VEC_ST:
	    case ALTIVEC_BUILTIN_VEC_STE:
	    case ALTIVEC_BUILTIN_VEC_STL:
	    case ALTIVEC_BUILTIN_VEC_STVEWX:
	    case ALTIVEC_BUILTIN_VEC_STVEBX:
	    case ALTIVEC_BUILTIN_VEC_STVEHX:
	      /* altivec_init_builtins: void_ftype_opaque_long_pvoid */

	      operand_modes[0] = E_VOIDmode;
	      /* opaque apparently stands for "opaque_V4SI", which is
	       * the result of making an "opaque_vector_type" out of
	       * an intSI_type_node.  I believe an opaque vector is a
	       * vector that is known to the back-end, but is not
	       * known to the target-independent portions of the GCC
	       * compiler.  Since the type is not known to the GCC
	       * compiler, the GCC compiler cannot "manipulate" this
	       * subtree.  */
	      operand_modes[1] = E_V4SImode;
	      /* SImode on -m32 */
	      operand_modes[2] = E_DImode;
	      /* Methinks pcvoid means "const *void".  I'll ignore
		 the const qualifier for now.  */
	      operand_modes[3] = ~E_VOIDmode;
	      num_operands = 4;
	      break;




	    case VSX_BUILTIN_STXVW4X_V8HI:
	      /* altivec_init_builtins: void_ftype_v8hi_long_pvoid, */

	    case VSX_BUILTIN_STXVW4X_V16QI:
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid, */








	    case VSX_BUILTIN_ST_ELEMREV_V1TI:
	      /* altivec_init_builtins: void_ftype_v1ti_long_pvoid, */




	    case VSX_BUILTIN_ST_ELEMREV_V8HI:
	      /* altivec_init_builtins: void_ftype_v8hi_long_pvoid, */

	    case VSX_BUILTIN_ST_ELEMREV_V16QI:
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid, */

	    case VSX_BUILTIN_VEC_LD:
	      /* altivec_init_builtins: opaque_ftype_long_pcvoid, */

	    case VSX_BUILTIN_VEC_ST:
	      /* altivec_init_builtins: void_ftype_opaque_long_pvoid */

	    case VSX_BUILTIN_VEC_XL:
	    case VSX_BUILTIN_VEC_XL_BE:
	      /* altivec_init_builtins: opaque_ftype_long_pcvoid, */
	      /* altivec_init_builtins: opaque_ftype_long_pcvoid, */


	    case VSX_BUILTIN_VEC_XST:
	    case VSX_BUILTIN_VEC_XST_BE:
	      /* altivec_init_builtins: void_ftype_opaque_long_pvoid, */
	      /* altivec_init_builtins: void_ftype_opaque_long_pvoid, */

	    case ALTIVEC_BUILTIN_VEC_STEP:
	      /* altivec_init_builtins: int_ftype_opaque */

	    case ALTIVEC_BUILTIN_VEC_SLD:
	    case ALTIVEC_BUILTIN_VEC_INSERT:
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque_int */



	    case ALTIVEC_BUILTIN_VEC_SPLATS:
	    case ALTIVEC_BUILTIN_VEC_PROMOTE:
	      /* altivec_init_builtins: opaque_ftype_opaque */
	      /* altivec_init_builtins: opaque_ftype_opaque */

	    case ALTIVEC_BUILTIN_VEC_SPLAT:
	    case ALTIVEC_BUILTIN_VEC_EXTRACT:
	    case ALTIVEC_BUILTIN_VEC_VSPLTW:
	    case ALTIVEC_BUILTIN_VEC_VSPLTH:
	    case ALTIVEC_BUILTIN_VEC_VSPLTB:
	    case ALTIVEC_BUILTIN_VEC_CTF:
	    case ALTIVEC_BUILTIN_VEC_VCFSX:
	    case ALTIVEC_BUILTIN_VEC_VCFUX:
	    case ALTIVEC_BUILTIN_VEC_CTS:
	    case ALTIVEC_BUILTIN_VEC_CTU:
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */
	      /* altivec_init_builtins: opaque_ftype_opaque_int */



	    case ALTIVEC_BUILTIN_VEC_CMPNE:
	    case ALTIVEC_BUILTIN_VEC_MUL:
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque */
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque */


	    case ALTIVEC_BUILTIN_VEC_ADDE:
	    case ALTIVEC_BUILTIN_VEC_ADDEC:
	    case ALTIVEC_BUILTIN_VEC_SUBE:
	    case ALTIVEC_BUILTIN_VEC_SUBEC:
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque_opaque */
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque_opaque */
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque_opaque */
	      /* altivec_init_builtins: opaque_ftype_opaque_opaque_opaque */







	    case P9V_BUILTIN_XST_LEN_R:
	    case P9V_BUILTIN_STXVL:
	      /* ONLY AVAILABLE ON P9 */
	    case ALTIVEC_BUILTIN_VEC_STVLX:
	    case ALTIVEC_BUILTIN_VEC_STVLXL:
	    case ALTIVEC_BUILTIN_VEC_STVRX:
	    case ALTIVEC_BUILTIN_VEC_STVRXL:
	    case ALTIVEC_BUILTIN_STVLX:
	    case ALTIVEC_BUILTIN_STVLXL:
	    case ALTIVEC_BUILTIN_STVRX:
	    case ALTIVEC_BUILTIN_STVRXL:
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_long_pvoid */
	      /* altivec_init_builtins: void_ftype_v16qi_pvoid_long */
	      /* altivec_init_builtins: void_ftype_v16qi_pvoid_long */




  /* following should all be removed */


	    case ALTIVEC_BUILTIN_ST_INTERNAL_16qi:
	      /* Replaced with CODE_FOR_vector_altivec_store_v16qi.  */
	    case ALTIVEC_BUILTIN_ST_INTERNAL_8hi:
	    case ALTIVEC_BUILTIN_ST_INTERNAL_4si:
	    case ALTIVEC_BUILTIN_ST_INTERNAL_4sf:
	    case ALTIVEC_BUILTIN_ST_INTERNAL_2df:
	    case ALTIVEC_BUILTIN_ST_INTERNAL_2di:
	    case ALTIVEC_BUILTIN_ST_INTERNAL_1ti:
	      /* Implemented by altivec_expand_st_builtin */
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = (machine_mode) (MAX_MACHINE_MODE + 1);

	      type_mask = (unsigned int) RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_LD_INTERNAL_16qi:
	    case ALTIVEC_BUILTIN_LD_INTERNAL_8hi:
	    case ALTIVEC_BUILTIN_LD_INTERNAL_4si:
	    case ALTIVEC_BUILTIN_LD_INTERNAL_4sf:
	    case ALTIVEC_BUILTIN_LD_INTERNAL_2df:
	    case ALTIVEC_BUILTIN_LD_INTERNAL_2di:
	    case ALTIVEC_BUILTIN_LD_INTERNAL_1ti:
	      /* Implemented by altivec_expand_ld_builtin */
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);

	      type_mask = (unsigned int) RS6000_BTC_UNARY;
	      break;

	    case ALTIVEC_BUILTIN_STVX_V2DF:
	    case ALTIVEC_BUILTIN_STVX_V2DI:
	    case ALTIVEC_BUILTIN_STVX_V4SF:
	    case ALTIVEC_BUILTIN_STVX:
	    case ALTIVEC_BUILTIN_STVX_V4SI:
	    case ALTIVEC_BUILTIN_STVX_V8HI:
	    case ALTIVEC_BUILTIN_STVX_V16QI:
	    case ALTIVEC_BUILTIN_STVEBX:
	    case ALTIVEC_BUILTIN_STVEHX:
	    case ALTIVEC_BUILTIN_STVEWX:
	    case ALTIVEC_BUILTIN_STVXL_V2DF:
	    case ALTIVEC_BUILTIN_STVXL_V2DI:
	    case ALTIVEC_BUILTIN_STVXL_V4SF:
	    case ALTIVEC_BUILTIN_STVXL:
	    case ALTIVEC_BUILTIN_STVXL_V4SI:
	    case ALTIVEC_BUILTIN_STVXL_V8HI:
	    case ALTIVEC_BUILTIN_STVXL_V16QI:
	    case ALTIVEC_BUILTIN_STVLX:
	    case ALTIVEC_BUILTIN_STVLXL:
	    case ALTIVEC_BUILTIN_STVRX:
	    case ALTIVEC_BUILTIN_STVRXL:
	    case VSX_BUILTIN_STXVD2X_V1TI:
	    case VSX_BUILTIN_STXVD2X_V2DF:
	    case VSX_BUILTIN_STXVD2X_V2DI:
	    case VSX_BUILTIN_STXVW4X_V4SF:
	    case VSX_BUILTIN_STXVW4X_V4SI:
	    case VSX_BUILTIN_STXVW4X_V8HI:
	    case VSX_BUILTIN_STXVW4X_V16QI:
	    case VSX_BUILTIN_ST_ELEMREV_V1TI:
	    case VSX_BUILTIN_ST_ELEMREV_V2DF:
	    case VSX_BUILTIN_ST_ELEMREV_V2DI:
	    case VSX_BUILTIN_ST_ELEMREV_V4SF:
	    case VSX_BUILTIN_ST_ELEMREV_V4SI:
	    case VSX_BUILTIN_ST_ELEMREV_V8HI:
	    case VSX_BUILTIN_ST_ELEMREV_V16QI:
	    case ALTIVEC_BUILTIN_VEC_STVLX:
	    case ALTIVEC_BUILTIN_VEC_STVLXL:
	    case ALTIVEC_BUILTIN_VEC_STVRX:
	    case ALTIVEC_BUILTIN_VEC_STVRXL:

	      /* Implemented by altivec_expand_stv_builtin */
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;
	      operand_modes[1] = insn_data[bidp->icode].operand[1].mode;

	      /* These are pointer.  Assume 64-bit target.  */
	      operand_modes[2] = E_DImode;
	      operand_modes[3] = E_DImode;

	      type_mask = (unsigned int) RS6000_BTC_TERNARY;
	      break;

	    case P9V_BUILTIN_STXVL:
	    case P9V_BUILTIN_XST_LEN_R:
	      /* Implemented by altivec_expand_stxvl_builtin */
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;
	      operand_modes[1] = insn_data[bidp->icode].operand[1].mode;
	      operand_modes[2] = insn_data[bidp->icode].operand[2].mode;

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_MFVSCR:
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;

	      type_mask = RS6000_BTC_SPECIAL;
	      break;

	    case ALTIVEC_BUILTIN_MTVSCR:
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = insn_data[bidp->icode].operand[0].mode;

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case ALTIVEC_BUILTIN_DSSALL:
	      operand_modes[0] = E_VOIDmode;

	      type_mask = RS6000_BTC_SPECIAL;
	      break;

	    case ALTIVEC_BUILTIN_DSS:
	      operand_modes[0] = E_VOIDmode;
	      /* 2-bit unsigned literal int */
	      operand_modes[1] = insn_data[bidp->icode].operand[0].mode;

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_INIT_V4SI:
	    case ALTIVEC_BUILTIN_VEC_INIT_V8HI:
	    case ALTIVEC_BUILTIN_VEC_INIT_V16QI:
	    case ALTIVEC_BUILTIN_VEC_INIT_V4SF:
	    case VSX_BUILTIN_VEC_INIT_V2DF:
	    case VSX_BUILTIN_VEC_INIT_V2DI:
	    case VSX_BUILTIN_VEC_INIT_V1TI:
	      /* The operand[0] type is computed at run-time.  */
	      operand_modes[0] = MAX_MACHINE_MODE;

	      type_mask = RS6000_BTC_SPECIAL;
	      break;

	    case ALTIVEC_BUILTIN_VEC_SET_V4SI:
	    case ALTIVEC_BUILTIN_VEC_SET_V8HI:
	    case ALTIVEC_BUILTIN_VEC_SET_V16QI:
	    case ALTIVEC_BUILTIN_VEC_SET_V4SF:
	    case VSX_BUILTIN_VEC_SET_V2DF:
	    case VSX_BUILTIN_VEC_SET_V2DI:
	    case VSX_BUILTIN_VEC_SET_V1TI:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = MAX_MACHINE_MODE;
	      operand_modes[2] = (machine_mode) (MAX_MACHINE_MODE + 1);

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_EXT_V4SI:
	    case ALTIVEC_BUILTIN_VEC_EXT_V8HI:
	    case ALTIVEC_BUILTIN_VEC_EXT_V16QI:
	    case ALTIVEC_BUILTIN_VEC_EXT_V4SF:
	    case VSX_BUILTIN_VEC_EXT_V2DF:
	    case VSX_BUILTIN_VEC_EXT_V2DI:
	    case VSX_BUILTIN_VEC_EXT_V1TI:
	      operand_modes[0] = MAX_MACHINE_MODE;
	      operand_modes[1] = MAX_MACHINE_MODE;
	      operand_modes[2] = MAX_MACHINE_MODE;

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_LVSL:
	    case ALTIVEC_BUILTIN_LVSR:
	    case ALTIVEC_BUILTIN_LVEBX:
	    case ALTIVEC_BUILTIN_LVEHX:
	    case ALTIVEC_BUILTIN_LVEWX:
	    case ALTIVEC_BUILTIN_LVXL_V2DF:
	    case ALTIVEC_BUILTIN_LVXL_V2DI:
	    case ALTIVEC_BUILTIN_LVXL_V4SF:
	    case ALTIVEC_BUILTIN_LVXL:
	    case ALTIVEC_BUILTIN_LVXL_V4SI:
	    case ALTIVEC_BUILTIN_LVXL_V8HI:
	    case ALTIVEC_BUILTIN_LVXL_V16QI:
	    case ALTIVEC_BUILTIN_LVX_V2DF:
	    case ALTIVEC_BUILTIN_LVX_V1TI:
	    case ALTIVEC_BUILTIN_LVX_V2DI:
	    case ALTIVEC_BUILTIN_LVX_V4SF:
	    case ALTIVEC_BUILTIN_LVX:
	    case ALTIVEC_BUILTIN_LVX_V4SI:
	    case ALTIVEC_BUILTIN_LVX_V8HI:
	    case ALTIVEC_BUILTIN_LVX_V16QI:
	    case ALTIVEC_BUILTIN_LVLX:
	    case ALTIVEC_BUILTIN_LVLXL:
	    case ALTIVEC_BUILTIN_LVRX:
	    case ALTIVEC_BUILTIN_LVRXL:

	    case VSX_BUILTIN_LXVD2X_V1TI:
	    case VSX_BUILTIN_LXVD2X_V2DF:
	    case VSX_BUILTIN_LXVD2X_V2DI:
	    case VSX_BUILTIN_LXVW4X_V4SF:
	    case VSX_BUILTIN_LXVW4X_V4SI:
	    case VSX_BUILTIN_LXVW4X_V8HI:
	    case VSX_BUILTIN_LXVW4X_V16QI:

	    case VSX_BUILTIN_LD_ELEMREV_V1TI:
	    case VSX_BUILTIN_LD_ELEMREV_V2DF:
	    case VSX_BUILTIN_LD_ELEMREV_V2DI:
	    case VSX_BUILTIN_LD_ELEMREV_V4SF:
	    case VSX_BUILTIN_LD_ELEMREV_V4SI:
	    case VSX_BUILTIN_LD_ELEMREV_V8HI:
	    case VSX_BUILTIN_LD_ELEMREV_V16QI:
	      /* The V1TI case is not explicitly present with the others in
		 altivec_expand_lv_builtin, so this case is apparently
		 handled in the "else" clause.  */

	    case ALTIVEC_BUILTIN_MASK_FOR_LOAD:
	    case ALTIVEC_BUILTIN_MASK_FOR_STORE:
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;
	      operand_modes[1] = insn_data[bidp->icode].operand[1].mode;

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_ADDEC:
	    case ALTIVEC_BUILTIN_VEC_ADDE:
	    case ALTIVEC_BUILTIN_VEC_SUBE:
	    case ALTIVEC_BUILTIN_VEC_SUBEC:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[3] = (machine_mode) (MAX_MACHINE_MODE + 1);

	      type_mask = RS6000_BTC_TERNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_CMPNE:
	      /* result is vector bool of same size as 2 vector args */
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = (machine_mode) (MAX_MACHINE_MODE + 1);

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_MUL:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = (machine_mode) (MAX_MACHINE_MODE + 1);

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_CTF:
	    case ALTIVEC_BUILTIN_VEC_CTS:
	    case ALTIVEC_BUILTIN_VEC_CTU:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = (machine_mode) E_SImode;

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_INSERT:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) MAX_MACHINE_MODE;
	      operand_modes[2] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[3] = (machine_mode) E_SImode;

	      type_mask = RS6000_BTC_TERNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_EXTRACT:
	      operand_modes[0] = (machine_mode) MAX_MACHINE_MODE;
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = (machine_mode) E_SImode;

	      type_mask = RS6000_BTC_TERNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_LDE:
	    case ALTIVEC_BUILTIN_VEC_LDL:
	    case ALTIVEC_BUILTIN_VEC_LVSL:
	    case ALTIVEC_BUILTIN_VEC_LVSR:
	    case ALTIVEC_BUILTIN_VEC_LVEBX:
	    case ALTIVEC_BUILTIN_VEC_LVEHX:
	    case ALTIVEC_BUILTIN_VEC_LVEWX:
	    case ALTIVEC_BUILTIN_VEC_LVLX:
	    case ALTIVEC_BUILTIN_VEC_LVLXL:
	    case ALTIVEC_BUILTIN_VEC_LVRX:
	    case ALTIVEC_BUILTIN_VEC_LVRXL:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) E_SImode;
	      operand_modes[2] = (machine_mode) E_DImode; /* Pmode */

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_PROMOTE:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) E_SImode;
	      operand_modes[2] = (machine_mode) E_SImode;

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_SPLATS:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = E_SImode;

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_STEP:
	      /* This prototype is a bit of a guess */
	      operand_modes[0] = E_SImode;
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_SLD:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[3] = E_SImode;

	      type_mask = RS6000_BTC_TERNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_SPLAT:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = E_SImode;

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_STE:
	    case ALTIVEC_BUILTIN_VEC_STL:
	    case ALTIVEC_BUILTIN_VEC_STVEWX:
	    case ALTIVEC_BUILTIN_VEC_STVEBX:
	    case ALTIVEC_BUILTIN_VEC_STVEHX:
	      operand_modes[0] = E_VOIDmode;
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = E_SImode;
	      operand_modes[3] = E_DImode; /* Pmode is DI on 64-bit */

	      type_mask = RS6000_BTC_TERNARY;
	      break;

	    case ALTIVEC_BUILTIN_VEC_VCFSX:
	    case ALTIVEC_BUILTIN_VEC_VCFUX:
	    case ALTIVEC_BUILTIN_VEC_VSPLTW:
	    case ALTIVEC_BUILTIN_VEC_VSPLTH:
	    case ALTIVEC_BUILTIN_VEC_VSPLTB:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[2] = E_SImode;

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case VSX_BUILTIN_LXSDX:
	    case VSX_BUILTIN_LXVDSX:
	    case VSX_BUILTIN_STXSDX:
	      operand_modes[0] = (machine_mode) (MAX_MACHINE_MODE + 1);
	      operand_modes[1] = E_DImode; /* 2 address registers */
	      operand_modes[2] = E_DImode;

	      /*	      name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case VSX_BUILTIN_XSABSDP:
	    case VSX_BUILTIN_XSNABSDP:
	    case VSX_BUILTIN_XSNEGDP:
	      operand_modes[0] = E_DFmode;
	      operand_modes[1] = E_DFmode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_UNARY;
	      break;


	    case VSX_BUILTIN_XSADDDP:
	    case VSX_BUILTIN_XSDIVDP:
	    case VSX_BUILTIN_XSMULDP:
	    case VSX_BUILTIN_XSSUBDP:
	      operand_modes[0] = E_DFmode;
	      operand_modes[1] = E_DFmode;
	      operand_modes[2] = E_DFmode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case VSX_BUILTIN_XSCMPODP:
	    case VSX_BUILTIN_XSCMPUDP:
	      operand_modes[0] = E_CCFPmode;
	      operand_modes[1] = E_DFmode;
	      operand_modes[2] = E_DFmode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case VSX_BUILTIN_XSCVDPSXDS:
	    case VSX_BUILTIN_XSCVDPUXDS:
	      operand_modes[0] = E_DImode;
	      operand_modes[1] = E_DFmode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case VSX_BUILTIN_XSCVDPSXWS:
	    case VSX_BUILTIN_XSCVDPUXWS:
	      operand_modes[0] = E_SImode;
	      operand_modes[1] = E_DFmode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case VSX_BUILTIN_XSCVSXDDP:
	    case VSX_BUILTIN_XSCVUXDDP:
	      operand_modes[0] = E_DFmode;
	      operand_modes[1] = E_DImode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case VSX_BUILTIN_XSMADDADP:
	    case VSX_BUILTIN_XSMADDMDP:
	    case VSX_BUILTIN_XSMSUBADP:
	    case VSX_BUILTIN_XSMSUBMDP:
	    case VSX_BUILTIN_XSNMADDADP:
	    case VSX_BUILTIN_XSNMADDMDP:
	    case VSX_BUILTIN_XSNMSUBADP:
	    case VSX_BUILTIN_XSNMSUBMDP:
	      operand_modes[0] = E_DFmode;
	      operand_modes[1] = E_DFmode;
	      operand_modes[2] = E_DFmode;
	      operand_modes[3] = E_DFmode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_TERNARY;
	      break;

	    case VSX_BUILTIN_XSMOVDP:
	      /* Can't even find this insn documented, so wild guess as
		 to intended semantics.  Maybe this is test for overflow
		 of previous floating point arithmetic?  */
	      operand_modes[0] = E_CCFPmode;

	      /* name_qualifier = "not_implemented_"; */

	      type_mask = RS6000_BTC_SPECIAL;
	      break;

	    case MISC_BUILTIN_SPEC_BARRIER:
	      operand_modes[0] = E_VOIDmode;

	      type_mask = RS6000_BTC_SPECIAL;
	      break;

	    case POWER7_BUILTIN_BPERMD:
	    case RS6000_BUILTIN_RECIP:
	    case RS6000_BUILTIN_RECIPF:
	      /* rs6000_expand_binop_builtin */
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;
	      operand_modes[1] = insn_data[bidp->icode].operand[1].mode;
	      operand_modes[2] = insn_data[bidp->icode].operand[2].mode;

	      type_mask = RS6000_BTC_BINARY;
	      break;

	    case RS6000_BUILTIN_RSQRTF:
	    case RS6000_BUILTIN_RSQRT:
	      /* rs6000_expand_unop_builtin */
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;
	      operand_modes[1] = insn_data[bidp->icode].operand[1].mode;

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case RS6000_BUILTIN_GET_TB:
	    case RS6000_BUILTIN_MFTB:
	    case RS6000_BUILTIN_MFFS:
	      /* rs6000_expand_zeroop_builtin */
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;

	      type_mask = RS6000_BTC_SPECIAL;
	      break;

	    case RS6000_BUILTIN_MTFSF:
	      /* rs6000_expand_mtfsf_builtin */
	      operand_modes[0] = insn_data[bidp->icode].operand[0].mode;
	      operand_modes[1] = insn_data[bidp->icode].operand[1].mode;

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    case RS6000_BUILTIN_CPU_INIT:
	      operand_modes[0] = E_VOIDmode;

	      type_mask = RS6000_BTC_SPECIAL;
	      break;


	    case RS6000_BUILTIN_CPU_IS:
	    case RS6000_BUILTIN_CPU_SUPPORTS:
	      /* cpu_expand_builtin */
	      operand_modes[0] = E_SImode;
	      operand_modes[1] = E_DImode; /* assume 64-bit pointer to char.  */

	      type_mask = RS6000_BTC_UNARY;
	      break;


	    case RS6000_BUILTIN_CFSTRING:
	      /* Can only find "old" documentation, that is not very
		 clear, so I'm guessing on what is intended. This may
		 have only been supported on Darwin.  */
	      operand_modes[0] = E_DImode;
	      operand_modes[1] = E_DImode;

	      type_mask = RS6000_BTC_UNARY;
	      break;

	    default:
	      fprintf (stderr,
		       "unrecognized special function: %s\n", bidp->name);
	      break;
	      /* Fall through.  */
	    }
	    attributes = type_mask;

	    switch (type_mask) {
	    case RS6000_BTC_DST:
	      /* Expect ptr operand, int length, const int configuration
		 operands, with void result.  */
	    case RS6000_BTC_TERNARY:
	      tmode = operand_modes[0];
	      mode0 = operand_modes[1];
	      mode1 = operand_modes[2];
	      mode2 = operand_modes[3];
	      /*
	      fprintf (stderr, "%s %s%s (%s, %s, %s): ", t2s (tmode),
		       name_qualifier, bidp->name,
		       t2s (mode0), t2s (mode1), t2s (mode2));
	      */
	      fprintf (stderr, "%s %s (%s, %s, %s): ", t2s (tmode),
		       bidp->name, t2s (mode0), t2s (mode1), t2s (mode2));
	      break;

	    case RS6000_BTC_PREDICATE:
	      /* An implicit integer predicate code and two source values.  */
	      tmode = operand_modes[0];
	      mode0 = operand_modes[1];
	      mode1 = operand_modes[2];
	      /*
	      fprintf (stderr, "%s %s%s (int, %s, %s): ", t2s (tmode),
		       name_qualifier, bidp->name, t2s (mode0), t2s (mode1));
	      */
	      fprintf (stderr, "%s %s (int, %s, %s): ", t2s (tmode),
		       bidp->name, t2s (mode0), t2s (mode1));
	      break;

	    case RS6000_BTC_BINARY:
	      tmode = operand_modes[0];
	      mode0 = operand_modes[1];
	      mode1 = operand_modes[2];
	      /*
	      fprintf (stderr, "%s %s%s (%s, %s): ", t2s (tmode),
		       name_qualifier, bidp->name, t2s (mode0), t2s (mode1));
	      */
	      fprintf (stderr, "%s %s (%s, %s): ", t2s (tmode),
		       bidp->name, t2s (mode0), t2s (mode1));
	      break;

	    case RS6000_BTC_ABS:	/* single argument, single result */
	    case RS6000_BTC_UNARY:
	      tmode = operand_modes[0];
	      mode0 = operand_modes[1];
	      /*
	      fprintf (stderr, "%s %s%s (%s): ",
		       t2s (tmode), name_qualifier, bidp->name, t2s (mode0));
	      */
	      fprintf (stderr, "%s %s (%s): ",
		       t2s (tmode), bidp->name, t2s (mode0));
	      break;

	    case RS6000_BTC_SPECIAL:
	      /* No args, but still a result type.  */
	      tmode = operand_modes[0];
	      /*
	      fprintf (stderr, "%s %s%s (): ", t2s (tmode),
		       name_qualifier, bidp->name);
	      */
	      fprintf (stderr, "%s %s (): ", t2s (tmode), bidp->name);
	      break;


	    }
	    /* No args, but still a result type.  */
	    tmode = insn_data[bidp->icode].operand[0].mode;
	    fprintf (stderr, "%s %s (): ", t2s (tmode), bidp->name);
	    break;
	  }
	}
	fprintf (stderr, "icode: %d, code: %d\n", bidp->icode, bidp->code);
	fprintf (stderr, "  mask: %s\n", m2s (bidp->mask));
	fprintf (stderr, "  attr: %s\n", a2s (attributes));
      }
    bidp++;
  }
  fprintf (stderr, "\n");
}

static void
dump_monomorphics ()
{
  fprintf (stderr, "\n");
  dump_one_table ("0-Argument built-in functions",
		  bdesc_0arg, ARRAY_SIZE (bdesc_0arg));
  dump_one_table ("1-Argument built-in functions",
		  bdesc_1arg, ARRAY_SIZE (bdesc_1arg));
  dump_one_table ("2-Argument built-in functions",
		  bdesc_2arg, ARRAY_SIZE (bdesc_2arg));
  dump_one_table ("3-Argument built-in functions",
		  bdesc_3arg, ARRAY_SIZE (bdesc_3arg));

  dump_one_table ("Altivec predicate built-in functions",
		  bdesc_altivec_preds, ARRAY_SIZE (bdesc_altivec_preds));
  dump_one_table ("Paired predicate built-in functions",
		  bdesc_paired_preds, ARRAY_SIZE (bdesc_paired_preds));
  dump_one_table ("Altivec/VSX Absolute Value builtin-in functions",
		  bdesc_abs, ARRAY_SIZE (bdesc_abs));
  dump_one_table ("Hardware Transactional Memory built-in functions",
		  bdesc_htm, ARRAY_SIZE (bdesc_htm));
  dump_one_table ("Data Stream Touch (cache hinting) functions",
		  bdesc_dst, ARRAY_SIZE (bdesc_dst));
  dump_one_table ("Special (non-standard) functions",
		  bdesc_special, ARRAY_SIZE (bdesc_special));
}

