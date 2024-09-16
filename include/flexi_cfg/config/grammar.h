#pragma once

#include <tao/pegtl.hpp>

namespace peg = TAO_PEGTL_NAMESPACE;

// The end goal of this is to be able to take a string of text (or a file)
// And create a structured tree of data.
// The data should consist only of specific types and eliminate all other
// unecessary data (e.g. whitespace, etc).

namespace flexi_cfg {

// This grammar parses a filename (path + filename) corresponding to a cfg file.
namespace filename {

struct DOTDOT : peg::two<'.'> {};
struct EXT : TAO_PEGTL_KEYWORD(".cfg") {};
struct SEP : peg::one<'/'> {};

// There may be other valid characters in a filename. What might they be?
struct ALPHAPLUS : peg::plus<peg::sor<peg::identifier_other, peg::one<'-'>>> {};

struct ENVIRONMENT_VAR : peg::seq<peg::one<'$'>, peg::one<'{'>,
                                  peg::star<peg::sor<peg::alnum, peg::one<'_'>>>, peg::one<'}'>> {};
struct FILEPART : peg::sor<ENVIRONMENT_VAR, DOTDOT, ALPHAPLUS> {};
struct FILENAME : peg::seq<peg::list<FILEPART, SEP>, EXT> {};

}  // namespace filename

// Pre-declare the top level rule for the math grammar here.
namespace math {
struct expression;
}

namespace config {

struct SP : peg::plus<peg::blank> {};
struct SP0 : peg::star<peg::blank> {};
struct COMMENT : peg::seq<peg::one<'#'>, peg::until<peg::eol>> {};
//struct TAIL : peg::star<peg::blank, peg::sor<COMMENT, peg::until<peg::at<peg::eol>, peg::blank> {};
struct TAIL : peg::star<SP0, peg::sor<peg::eol, COMMENT>> {};

// A rule for padding another rule with blanks on either side
template <typename Rule>
struct pd : peg::pad<Rule, peg::blank> {};

struct COMMA : pd<peg::one<','>> {};
// Square brackets
struct SBo : pd<peg::one<'['>> {};
struct SBc : pd<peg::one<']'>> {};
// Curly braces
struct CBo : pd<peg::one<'{'>> {};
struct CBc : pd<peg::one<'}'>> {};
// Key/value separator
struct KVs : pd<peg::one<'='>> {};
// These two rules define the enclosing delimiters for a math expression (i.e. "{{" and "}}")
struct Eo : pd<peg::two<'{'>> {};
struct Ec : pd<peg::two<'}'>> {};

// These are reserved keywords that can't be used elsewhere
struct STRUCTk : TAO_PEGTL_KEYWORD("struct") {};
struct PROTOk : TAO_PEGTL_KEYWORD("proto") {};
struct REFk : TAO_PEGTL_KEYWORD("reference") {};
struct ASk : TAO_PEGTL_KEYWORD("as") {};
struct OVERRIDEk : TAO_PEGTL_KEYWORD("[override]") {};
struct PARENTNAMEk : TAO_PEGTL_KEYWORD("$PARENT_NAME") {};

struct RESERVED : peg::sor<STRUCTk, PROTOk, REFk, ASk, OVERRIDEk, PARENTNAMEk> {};

struct HEXTAG : peg::seq<peg::one<'0'>, peg::one<'x', 'X'>> {};
struct HEX : peg::seq<HEXTAG, peg::plus<peg::xdigit>> {};

struct sign : peg::one<'+', '-'> {};
struct exp : peg::seq<peg::one<'e', 'E'>, peg::opt<sign>, peg::plus<peg::digit>> {};

// NOTE: We want to use the same common part for an "INTEGER" and a "FLOAT", but we don't want the
// "INTEGER" sub-rule to match when looking at the beginning of a float, so we do this trick here.
// "INTEGER_" is the same for both an "INTEGER" and a "FLOAT", but when matching an "INTEGER" we
// ensure that there is no decimal point at the end.
struct INTEGER_
    : peg::seq<peg::opt<sign>,
               peg::sor<peg::one<'0'>, peg::seq<peg::range<'1', '9'>, peg::star<peg::digit>>>> {};
struct INTEGER : peg::seq<INTEGER_, peg::not_at<peg::one<'.'>>> {};
struct FLOAT
    : peg::seq<INTEGER_,
               peg::sor<peg::seq<peg::one<'.'>, peg::star<peg::digit>, peg::opt<exp>>, exp>> {};
struct NUMBER : peg::sor<FLOAT, INTEGER> {};

struct TRUE : TAO_PEGTL_KEYWORD("true") {};
struct FALSE : TAO_PEGTL_KEYWORD("false") {};
struct BOOLEAN : peg::sor<TRUE, FALSE> {};

struct STRING : peg::seq<peg::one<'"'>, peg::plus<peg::not_one<'"'>>, peg::one<'"'>> {};

struct LIST;
struct VALUE_LOOKUP;
struct VALUE : peg::sor<HEX, NUMBER, STRING, BOOLEAN, VALUE_LOOKUP, LIST> {};
// 'seq' is used here so that the 'VALUE' action will collect the location information.
struct LIST_ELEMENT : peg::seq<VALUE> {};
struct LIST_CONTENT : peg::list<LIST_ELEMENT, COMMA, peg::space> {};
// Should the 'space' here be a 'blank'? Allow multi-line lists (w/o \)?
struct LIST : peg::seq<SBo, TAIL, LIST_CONTENT, TAIL, SBc> {
  using begin = SBo;
  using end = SBc;
  using element = LIST_ELEMENT;
};

struct EXPRESSION : peg::seq<Eo, math::expression, Ec> {};

// Account for all reserved keywords when looking for keys
struct KEY : peg::seq<peg::not_at<RESERVED>, peg::lower, peg::star<peg::identifier_other>> {};
struct FLAT_KEY : peg::list<KEY, peg::one<'.'>> {};

// A 'VAR' can only be found within a proto (and it's children)
struct VARc : peg::seq<peg::upper, peg::star<peg::ranges<'A', 'Z', '0', '9', '_'>>> {};
// Allow for VAR to be expessed as: $VAR or ${VAR}
struct VAR : peg::seq<peg::one<'$'>, peg::sor<peg::seq<peg::one<'{'>, VARc, peg::one<'}'>>, VARc>> {
};

struct VALUE_LOOKUP : peg::seq<TAO_PEGTL_STRING("$("), peg::list<peg::sor<KEY, VAR>, peg::one<'.'>>,
                               peg::one<')'>> {};

// A special type of list for lists containing VAR elements.
struct PROTO_LIST_ELEMENT : peg::sor<VALUE, VAR> {};
struct PROTO_LIST_CONTENT : peg::list_must<PROTO_LIST_ELEMENT, COMMA, peg::space> {};
// Should the 'space' here be a 'blank'? Allow multi-line lists (w/o \)?
struct PROTO_LIST : peg::if_must<SBo, TAIL, PROTO_LIST_CONTENT, TAIL, SBc> {
  using begin = SBo;
  using end = SBc;
  using element = PROTO_LIST_ELEMENT;
};

struct KV_NOMINAL : peg::sor<VALUE, VALUE_LOOKUP, EXPRESSION> {};

struct REF_ADDKVP : peg::seq<peg::one<'+'>, KEY, KVs, KV_NOMINAL, TAIL> {};
struct REF_VARDEF
    : peg::seq<VAR, KVs, peg::sor<VALUE, VALUE_LOOKUP, EXPRESSION, PARENTNAMEk>, TAIL> {};

// A 'FULLPAIR' is a flattened key followed by a limited set of "value" options
struct FULLPAIR : peg::seq<FLAT_KEY, peg::opt<pd<OVERRIDEk>>, KVs, KV_NOMINAL, TAIL> {};
struct PAIR : peg::seq<KEY, peg::opt<pd<OVERRIDEk>>, KVs, KV_NOMINAL, TAIL> {};
// NOTE: Within a 'PROTO_PAIR' it may make sense to support a special type of list that can contain
// one or more 'VAR' elements
struct PROTO_PAIR
    : peg::seq<KEY, KVs, peg::sor<VALUE, VALUE_LOOKUP, EXPRESSION, VAR, PROTO_LIST>, TAIL> {};

// A rule for defining struct-like objects
template <typename Start, typename Content>
struct STRUCT_LIKE : peg::seq<Start, peg::if_must<CBo, TAIL, Content, CBc, TAIL>> {};

struct REFs : peg::seq<REFk, SP, FLAT_KEY, SP, ASk, SP, KEY> {};
struct REFc : peg::star<peg::sor<REF_VARDEF, REF_ADDKVP>> {};
struct REFERENCE : STRUCT_LIKE<REFs, REFc> {};

struct PROTOc;
struct PROTOs : peg::seq<PROTOk, SP, KEY> {};
struct PROTO : STRUCT_LIKE<PROTOs, PROTOc> {};

struct STRUCTc;
struct STRUCTs : peg::seq<STRUCTk, SP, KEY> {};
struct STRUCT : STRUCT_LIKE<STRUCTs, STRUCTc> {};

// Special definition of a struct contained in a proto
struct STRUCT_IN_PROTO : STRUCT_LIKE<STRUCTs, PROTOc> {};

struct PROTOc : peg::plus<peg::sor<PROTO_PAIR, STRUCT_IN_PROTO, REFERENCE>> {};
struct STRUCTc : peg::plus<peg::sor<PAIR, STRUCT, REFERENCE, PROTO>> {};

// Include syntax
struct INCLUDE : peg::seq<TAO_PEGTL_KEYWORD("include"), SP, filename::FILENAME, TAIL> {};
struct include_list : peg::star<INCLUDE> {};

// Include relative syntax
struct INCLUDE_RELATIVE
    : peg::seq<TAO_PEGTL_KEYWORD("include_relative"), SP, filename::FILENAME, TAIL> {};
struct include_relative_list : peg::star<INCLUDE_RELATIVE> {};

struct includes : peg::seq<include_list, include_relative_list> {};

// A single file should look like this:
//
//  1. Optional list of include files
//  2. Optional list of relative include files
//  3. Elements of a config file: flat keys OR struct / proto / reference / pair

// The `peg::sor<...>` here matches one or more `FULLPAIR` objects or the contents of a `STRUCT`,
// which includes the following items:
//  * STRUCT
//  * REFERENCE
//  * PROTO
//  * PAIR
//
// but never both in the same file. The `peg::not_at<PAIR>` prevents the PAIR that might appear in a
// `STRUCTc` from being matched as a `FULLPAIR` object.
struct config_fields
    : peg::opt<peg::sor<peg::seq<peg::not_at<PAIR>, peg::plus<FULLPAIR>>, STRUCTc>> {};

// struct CONFIG : peg::seq<TAIL, peg::not_at<peg::eolf>, includes, config_fields, TAIL> {};
struct CONFIG : peg::seq<TAIL, includes, TAIL, config_fields, TAIL> {};

struct grammar : peg::must<peg::seq<CONFIG, peg::eof>> {};

// Custom error messages for rules
template <typename>
inline constexpr const char* error_message = nullptr;

// clang-format off 
template <> inline constexpr auto error_message<CBc> = "expected a closing '}'";
template <> inline constexpr auto error_message<PROTO_LIST> = "invalid list in 'proto'";
template <> inline constexpr auto error_message<PROTO_LIST_CONTENT> = "invalid list in 'proto'";
template <> inline constexpr auto error_message<PROTO_LIST_ELEMENT> = "invalid element in proto list";
template <> inline constexpr auto error_message<SBc> = "expected a closing ']'";
template <> inline constexpr auto error_message<grammar> = "Invalid config file found!";

template <> inline constexpr auto error_message<PROTOc> = "expected a proto-pair, struct or reference";
template <> inline constexpr auto error_message<REFc> = "expected a variable definition or a added variable";
template <> inline constexpr auto error_message<STRUCTc> = "expected a pair, struct or reference";
template <> inline constexpr auto error_message<CONFIG> = "invalid config structure";
template <> inline constexpr auto error_message<filename::FILENAME> = "invalid filename";
//template <> inline constexpr auto error_message<peg::eof> = "expected end of file";
//template <> inline constexpr auto error_message<peg::eolf> = "expected end of line/file";
//template <> inline constexpr auto error_message<peg::eol> = "expected end of line";
//template <> inline constexpr auto error_message<peg::until<peg::eol>> =
//    "expected statement to continue till end of line";
//template <> inline constexpr auto error_message<peg::space> = "expected space (why are we here?)";
////template <> inline constexpr auto error_message<WS_> = "expected whitespace (why are we here?)";
template <> inline constexpr auto error_message<TAIL> = "expected a comment (why are we here?)";
//template <> inline constexpr auto error_message<tao::pegtl::seq<flexi_cfg::config::CONFIG, tao::pegtl::eolf>> = "expected a valid config";
template <> inline constexpr auto error_message<tao::pegtl::seq<flexi_cfg::config::CONFIG, tao::pegtl::eof>> = "expected a valid config";
// clang-format on

// As must_if can not take error_message as a template parameter directly, we need to wrap it:
struct error {
  //  template <typename Rule>
  //  static constexpr bool raise_on_failure = true;
  template <typename Rule>
  static constexpr auto message = error_message<Rule>;
};

template <typename Rule>
using control = tao::pegtl::must_if<error>::control<Rule>;

}  // namespace config

}  // namespace flexi_cfg

#include "flexi_cfg/math/grammar.h"
