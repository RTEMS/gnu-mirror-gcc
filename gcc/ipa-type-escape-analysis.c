#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple-expr.h"
#include "predict.h"
#include "alloc-pool.h"
#include "tree-pass.h"
#include "cgraph.h"
#include "diagnostic.h"
#include "fold-const.h"
#include "gimple-fold.h"
#include "symbol-summary.h"
#include "tree-vrp.h"
#include "ipa-prop.h"
#include "tree-pretty-print.h"
#include "tree-inline.h"
#include "ipa-fnsummary.h"
#include "ipa-utils.h"
#include "tree-ssa-ccp.h"
#include "stringpool.h"
#include "attribs.h"

#include "gimple-collector.hpp"
#include "gimple-escaper.hpp"
#include "gimple-caster.hpp"
#include "gimple-accesser.hpp"
#include "type-stringifier.hpp"
#include "type-incomplete-equality.hpp"
#include "type-reconstructor.hpp"
#include "gimple-rewriter.hpp"
#include <vector>


static unsigned int iphw_execute();

namespace {
/* ==What is type-escape-analysis?==
 * type-escape-analysis is a SIMPLE_IPA_PASS that performs no transformations.
 * type-escape-analysis only performs analysis and outputs to a WPA dump file.
 *
 * ==Why should we run type-escape-analysis?==
 * type-escape-analysis is useful to run unit tests in gcc.
 * By having the type-escape-analysis execute during WPA we are able to use
 * the dejagnu framework to see if an expected output matches the observed
 * output in the wpa dump file.
 * 
 * ==How do we use type-escape-analysis?==
 * Compile with
 * -flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis
 *
 * To use type-escape-analysis in tests use the following lines
 * { dg-do link }
 * { dg-options  "-flto -fipa-type-escape-analysis -fdump-ipa-type-escape-analysis" } 
 * C code to test...
 * { dg-final { scan-wpa-ipa-dump "regex" "type-escape-analysis" } } 
 *
 * ==TODO==
 * At the moment, the tests are not set up to work with the current framework,
 * so I will need to update them in the following day.
 */
const pass_data pass_data_ipa_type_escape_analysis =
{
  SIMPLE_IPA_PASS,
  "type-escape-analysis",
  OPTGROUP_NONE,
  TV_NONE,
  (PROP_cfg | PROP_ssa),
  0,
  0,
  0,
  0,
};

class pass_ipa_type_escape_analysis : public simple_ipa_opt_pass
{
public:
  pass_ipa_type_escape_analysis (gcc::context *ctx)
    : simple_ipa_opt_pass(pass_data_ipa_type_escape_analysis, ctx)
  {}

  virtual bool gate(function*) { return in_lto_p && flag_ipa_type_escape_analysis && flag_profile_use; }
  virtual unsigned execute (function*) { return iphw_execute(); }
};
} // anon namespace

simple_ipa_opt_pass*
make_pass_ipa_type_escape_analysis (gcc::context *ctx)
{
  return new pass_ipa_type_escape_analysis (ctx);
}

static void collect_types();

static unsigned int
iphw_execute()
{
  collect_types();
  return 0;
}

static void
fix_escaping_types_in_set(ptrset_t &types)
{
  bool fixed_point_reached = false;
  TypeIncompleteEquality structuralEquality;
  TypeStringifier stringifier;
  do {
    std::vector<const_tree> fixes;
    fixed_point_reached = true;
    for (auto i = types.escaping.cbegin(), e = types.escaping.cend(); i != e; ++i)
    {
      for (auto j = types.non_escaping.cbegin(), f = types.non_escaping.cend(); j != f; ++j)
      {
       const_tree type_esc = *i;
       gcc_assert(type_esc);
       const_tree type_non = *j;
       gcc_assert(type_non);
       // There can be cases where incomplete types are marked as non-escaping
       // and complete types counter parts are marked as escaping.
       //const bool interesting_case = eq_type_compare(type_esc, type_non);
       //TODO: We are going to need a different type comparison because this one
       //fails to take into account the recursion...
       std::string type_esc_name = TypeStringifier::get_type_identifier(type_esc);
       std::string type_non_name = TypeStringifier::get_type_identifier(type_non);

       type_esc_name = stringifier.stringify(type_esc);
       type_non_name = stringifier.stringify(type_non);

       const bool equal = structuralEquality.equal(type_esc, type_non);
       if (!equal) continue;

       log("recalulating %s == %s\n", type_esc_name.c_str(), type_non_name.c_str());
       fixed_point_reached = false;
       // Add incomplete to escaping
       // delete incomplete from non_escaping
       // We shouldn't do that inside our iteration loop.
       fixes.push_back(type_non);
      }
    }

    for (auto i = fixes.cbegin(), e = fixes.cend(); i != e; ++i)
    {
      const_tree escaping_type = *i;
      types.escaping.insert(escaping_type);
      types.non_escaping.erase(escaping_type);
    }
  } while (!fixed_point_reached);
}

static void
collect_types()
{
  GimpleTypeCollector collector;
  collector.walk();
  collector.print_collected();
  ptrset_t types = collector.get_pointer_set();
  GimpleCaster caster(types);
  caster.walk();
  if (flag_print_cast_analysis) caster.print_reasons();
  ptrset_t casting = caster.get_sets();
  fix_escaping_types_in_set(casting);
  GimpleAccesser accesser;
  accesser.walk();
  if (flag_print_access_analysis) accesser.print_accesses();
  record_field_map_t record_field_map = accesser.get_map();
  TypeIncompleteEquality equality;
  bool has_fields_that_can_be_deleted = false;
  typedef std::set<unsigned> field_offsets_t;
  typedef std::map<const_tree, field_offsets_t> record_field_offset_map_t;
  record_field_offset_map_t record_field_offset_map;
  //TODO: We need to optimize this, compiling GCC is taking too long
  for (auto i = record_field_map.begin(), e = record_field_map.end(); i != e; ++i)
  {
    const_tree r_i = i->first;
    std::vector<const_tree> equivalence;
    for (auto j = record_field_map.cbegin(), f = record_field_map.cend(); j != f; j++)
    {
       const_tree r_j = j->first;
       const bool pointer_equal = r_i == r_j;
       if (pointer_equal) continue;

       bool is_p_record = casting.in_points_to_record(r_i) && casting.in_points_to_record(r_j);
       if (!is_p_record) continue;

       const bool are_equal = equality.equal(r_i, r_j);
       if (!are_equal) continue;

       equivalence.push_back(r_j);
    }

    field_offsets_t field_offset;
    field_access_map_t original_field_map = record_field_map[r_i];
    for (auto j = original_field_map.begin(), f = original_field_map.end(); j != f; ++j)
    {
	 const_tree f_k = j->first;
	 unsigned access = j->second;
	 const bool is_read = access & Read;
         unsigned f_offset = tree_to_uhwi(DECL_FIELD_OFFSET(f_k));
         unsigned f_offset_2 = tree_to_uhwi(DECL_FIELD_BIT_OFFSET(f_k));
	 //log("%s offset %u %u is_read %s\n", TypeStringifier::get_field_identifier(f_k).c_str(), f_offset, f_offset_2, is_read ? "t" :"f");
	 if (!is_read) continue;
	field_offset.insert(f_offset * 8 + f_offset_2);
    }
    for (auto j = equivalence.begin(), f = equivalence.end(); j != f; j++)
    {
      const_tree r_j = *j;
      field_access_map_t equivalent_field_map = record_field_map[r_j];

      for (auto k = equivalent_field_map.begin(), g = equivalent_field_map.end(); k != g; ++k)
      {
	const_tree f_k = k->first;
	unsigned access = k->second;
	const bool is_read = access & Read;
        unsigned f_offset = tree_to_uhwi(DECL_FIELD_OFFSET(f_k));
        unsigned f_offset_2 = tree_to_uhwi(DECL_FIELD_BIT_OFFSET(f_k));
	//log("%s offset %u %u is_read %s\n", TypeStringifier::get_field_identifier(f_k).c_str(), f_offset, f_offset_2, is_read ? "t" :"f");
	if (!is_read) continue;
	field_offset.insert(f_offset * 8 + f_offset_2);
      }
    }
    record_field_offset_map[r_i] = field_offset;
  }


  const typeset &non_escaping = casting.non_escaping;
  
  std::vector<const_tree> to_erase;
  std::set<const_tree> to_keep;
  for (auto i = record_field_offset_map.begin(), e = record_field_offset_map.end(); i != e; ++i)
  {
    const_tree record = i->first;
    const bool in_set = non_escaping.find(record) != non_escaping.end();
    if (!in_set) {
      to_erase.push_back(record);
      continue;
    }

    field_offsets_t field_offset = i->second;
    for (tree field = TYPE_FIELDS(record); field; field = DECL_CHAIN(field))
    {
        unsigned f_offset = tree_to_uhwi(DECL_FIELD_OFFSET(field));
        unsigned f_offset_2 = tree_to_uhwi(DECL_FIELD_BIT_OFFSET(field));
	f_offset = f_offset * 8 + f_offset_2;
	bool in_set2 = field_offset.find(f_offset) != field_offset.end();
	if (in_set2) {
          field_offset.erase(f_offset);
	  continue;
	}
	to_keep.insert(record);
	field_offset.insert(f_offset);
        has_fields_that_can_be_deleted = true;
	log("%s.%s may be deleted\n", TypeStringifier::get_type_identifier(record).c_str(), TypeStringifier::get_field_identifier(field).c_str());
    }
    record_field_offset_map[record] = field_offset;
  }

  for (auto i = to_erase.begin(), e = to_erase.end(); i != e; ++i)
  {
     const_tree record = *i;
     record_field_offset_map.erase(record);
  }

  if (!has_fields_that_can_be_deleted) return;

  TypeReconstructor reconstructor(record_field_offset_map);
  TypeStringifier stringifier;

  // TODO:
  // Here, what we want to do is we want to rewrite only the
  // types which we believe we can rewrite, that and all types which
  // point to those types...
  //
  // Otherwise, it will lead to difficulties in the future since
  // we could be modifying many different types.
  // So we have to make sure that we are only modifying the types of interest.
  for (auto i = types.points_to_record.cbegin(), e = types.points_to_record.cend(); i != e; ++i)
  {
    const_tree record = *i;
    std::string name_from = stringifier.stringify(TYPE_MAIN_VARIANT(record));
    bool points_to_record = false;
    const_tree tt = record;

   
    //TODO:
    //This is our little hack to make sure that we are 
    //only modifying types which are of interest.
    //However, we really shouldn't.
    //Let's clean the input to reconstructor.walk
    while (TREE_TYPE(tt)) { tt = TREE_TYPE(tt); };
    points_to_record = TREE_CODE(tt) == RECORD_TYPE;
    if (!points_to_record) continue;

    bool in_map = record_field_offset_map.find(tt) != record_field_offset_map.end();
    if (!in_map) continue;

    // We need to walk over the type main variant first...
    reconstructor.walk(TYPE_MAIN_VARIANT(record));
    log("walking main variant %s\n", name_from.c_str());
  }

  for (auto i = types.points_to_record.cbegin(), e = types.points_to_record.cend(); i != e; ++i)
  {
    const_tree record = *i;
    std::string name_from = stringifier.stringify(record);
    bool points_to_record = false;
    const_tree tt = record;

   
    //TODO:
    //This is our little hack to make sure that we are 
    //only modifying types which are of interest.
    //However, we really shouldn't.
    //Let's clean the input to reconstructor.walk
    while (TREE_TYPE(tt)) { tt = TREE_TYPE(tt); };
    points_to_record = TREE_CODE(tt) == RECORD_TYPE;
    if (!points_to_record) continue;

    bool in_map = record_field_offset_map.find(tt) != record_field_offset_map.end();
    if (!in_map) continue;

    // We need to walk over the type main variant first...
    reconstructor.walk(record);
    log("walking non main%s\n", name_from.c_str());
  }


  TypeReconstructor::reorg_record_map_t map = reconstructor.get_map();
  TypeReconstructor::reorg_field_map_t field_map = reconstructor.get_field_map();

  for (auto i = map.cbegin(), e = map.cend(); i != e; ++i)
  {
    const_tree o_record = i->first;
    tree r_record = i->second;
    tree m_record = TYPE_MAIN_VARIANT(r_record);
    std::string name_from = stringifier.stringify(o_record);
    std::string name_to = stringifier.stringify(r_record);
    std::string name_to_mv = stringifier.stringify(m_record);
    bool c_f = TYPE_CACHED_VALUES_P(o_record);
    bool c_t = TYPE_CACHED_VALUES_P(r_record);
    bool c_m = TYPE_CACHED_VALUES_P(m_record);
    TYPE_CACHED_VALUES_P((tree)o_record) = false;
    TYPE_CACHED_VALUES_P((tree)m_record) = false;
    
    log("map f: %s TYPE_CACHED_VALUES_P %s \n", name_from.c_str(), c_f ? "t" : "f");
    log("map t: %s TYPE_CACHED_VALUES_P %s \n", name_to.c_str(), c_t ? "t" : "f");
    log("map m: %s TYPE_CACHED_VALUES_P %s \n", name_to_mv.c_str(), c_m ? "t" : "f");
    bool in_map = map.find(m_record) != map.end();
    if (!in_map) continue;
    tree mm_record = map[m_record];
    std::string name_to_mmv = stringifier.stringify(mm_record);
    bool c_m2 = TYPE_CACHED_VALUES_P(mm_record);
    log("map m2: %s TYPE_CACHED_VALUES_P %s\n", name_to_mmv.c_str(), c_m2 ? "t" : "f");
    // TODO: This is a hack...
    TYPE_MAIN_VARIANT(r_record) = mm_record;
    // Do we need to layout the type again?
  }


  GimpleTypeRewriter rewriter(map, field_map);
  rewriter.walk();
  rewriter._rewrite_function_decl();

  GimpleWalker walker;
  walker.walk(); // Just for printing...

  log("FINISHED\n");
}
