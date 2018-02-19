
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
const char *
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

const char *
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

const struct rs6000_builtin_info_type *
get_type (const char *name)
{
  const struct rs6000_builtin_info_type *result;
  int i;

  /* The first entry in the table is not normal, so skip over that.  */
  result = rs6000_builtin_info + 1;
  for (i = (sizeof (rs6000_builtin_info)
	    / sizeof (struct rs6000_builtin_info_type)) - 1; i--; result++) {
    /*    fprintf (stderr, "get_type(%s), looking at { %s, %d, %lx, %x }\n",
     *	     name, result->name, result->icode, result->mask,
     *	     result->attr);
     */
    if (!strcmp(result->name, name))
      return result;
  }
  return NULL;
}

const char *
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
    return ptr_flag? "udq_mode *": "udq_mode";

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
    return ptr_flag? "sf_mode *": "sf_mode";

  case E_DFmode:
    return ptr_flag? "df_mode *": "df_mode";

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

  default:
    return ptr_flag? "unknown_mode *": "unknown_mode";
  }
}

void
dump_special_table (const char *title,
		    const struct builtin_description *bidp, int num_entries)
{
  machine_mode tmode, mode0, mode1, mode2;

  fprintf (stderr, "%s [%d]\n", title, num_entries);
  while (num_entries--) {
    unsigned attributes = get_type (bidp->name)->attr;
    unsigned type_mask = attributes & RS6000_BTC_TYPE_MASK;

bidp has an icode and a code.  which of these is the fcode?  I think it is the code.

  note that ALTIVEC_BUILTIN_VMADDFP (1) and ALTIVEC_BUILTIN_STVX_V2DF (??)
  are both enum rs6000_builtins
  it may be neither one of these

  i think it's the code.  see this line from builtins.error:

3-Argument built-in functions [152]
vector float __builtin_altivec_vmaddfp (vector float, vector float, vector floa\
t): icode: 2299, code: 1
  mask: RS6000_BTM_ALTIVEC
  attr: RS6000_BTC_TERNARY|RS6000_BTC_FP





  

    switch (fcode) {

    }


    if ((attributes & RS6000_BTC_OVERLOADED) == 0)
      {
	switch (type_mask) {
	case RS6000_BTC_PREDICATE:
	  /* Two source values plus an integer predicate code.  */
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
	  /* No args, but still a result type.  */
	  tmode = insn_data[bidp->icode].operand[0].mode;
	  fprintf (stderr, "%s %s (): ", t2s (tmode), bidp->name);
	  break;
	}
	fprintf (stderr, "icode: %d, code: %d\n", bidp->icode, bidp->code);
	fprintf (stderr, "  mask: %s\n", m2s (bidp->mask));
	fprintf (stderr, "  attr: %s\n", a2s (attributes));
      }
    bidp++;
  }
  fprintf (stderr, "\n");
}

void
dump_one_table (const char *title,
		const struct builtin_description *bidp, int num_entries)
{
  machine_mode tmode, mode0, mode1, mode2;

  fprintf (stderr, "%s [%d]\n", title, num_entries);
  while (num_entries--) {
    unsigned attributes = get_type (bidp->name)->attr;
    unsigned type_mask = attributes & RS6000_BTC_TYPE_MASK;

    if ((attributes & RS6000_BTC_OVERLOADED) == 0)
      {
	switch (type_mask) {
	case RS6000_BTC_PREDICATE:
	  /* Two source values plus an integer predicate code.  */
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
	  /* No args, but still a result type.  */
	  tmode = insn_data[bidp->icode].operand[0].mode;
	  fprintf (stderr, "%s %s (): ", t2s (tmode), bidp->name);
	  break;
	}
	fprintf (stderr, "icode: %d, code: %d\n", bidp->icode, bidp->code);
	fprintf (stderr, "  mask: %s\n", m2s (bidp->mask));
	fprintf (stderr, "  attr: %s\n", a2s (attributes));
      }
    bidp++;
  }
  fprintf (stderr, "\n");
}

void
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
  dump_special_table ("Special (non-standard) functions",
		  bdesc_special, ARRAY_SIZE (bdesc_special));
}

void
take_a_dump ()
{
  if (TARGET_DEBUG_BUILTIN)
    dump_monomorphics ();
}
