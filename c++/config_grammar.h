#pragma once

#include <tao/pegtl.hpp>

namespace peg = TAO_PEGTL_NAMESPACE;

// The end goal of this is to be able to take a string of text (or a file)
// And create a structured tree of data.
// The data should consist only of specific types and eliminate all other
// unecessary data (e.g. whitespace, etc).

// This grammar parses a filename (path + filename) corresponding to a cfg file.
namespace filename {

struct DOTDOT : peg::two<'.'> {};
struct EXT : TAO_PEGTL_KEYWORD(".cfg") {};
struct SEP : peg::one<'/'> {};

// There may be other valid characters in a filename. What might they be?
struct ALPHAPLUS
    : peg::plus<peg::sor<peg::ranges<'A', 'Z', 'a', 'z', '0', '9', '_'>, peg::one<'-'>>> {};

struct FILEPART : peg::sor<DOTDOT, ALPHAPLUS> {};
struct FILENAME : peg::seq<peg::list<FILEPART, SEP>, EXT> {};

struct grammar : peg::must<FILENAME> {};

}  // namespace filename

namespace config {

/*
  TODO: Add validator to throw on duplicate keys
 */

// clang-format off
/*
grammar my_config
  map        <-  _ (struct / proto / reference)+ _ %make_map
  struct     <-  STRUCTs KEY TAIL STRUCTc END _ %make_struct
  proto      <-  PROTOs KEY TAIL STRUCTc END _ %make_proto
  reference  <-  REFs FLAT_KEY _ "as" _ KEY TAIL REFc END _ %make_reference
  STRUCTs    <-  "struct" SP
  PROTOs     <-  "proto" SP
  REFs       <-  "reference" SP
  END        <-  "end" SP KEY
  STRUCTc    <-  (struct / PAIR / reference / proto)+
  REFc       <-  (VARREF / VARADD)+
  PAIR       <-  KEY KVs (value / VAR_REF) TAIL %make_pair
  REF_VARSUB <-  VAR KVs value TAIL %ref_sub_var
  REF_VARADD <-  "+" KEY KVs value TAIL %ref_add_var
  FLAT_KEY   <-  KEY ("." KEY)+  %found_key  # Flattened struct/reference syntax
  KEY        <-  [a-z] [a-zA-Z0-9_]*  %found_key
  value      <-  list / HEX / number / string
  string     <-  '"' [^"]* '"' %make_string
  list       <-  SBo value (COMMA value)* SBc %make_list
  number     <-  (!HEX) [+-]? [0-9]+ ("." [0-9]*)? ("e" [+-]? [0-9]+)? %make_number
  VAR        <-  "$" [A-Z0-9_]+  %make_var
  VAR_REF    <-  "$(" FLAT_KEY ")" %var_ref
  HEX        <-  "0" [xX] [0-9a-fA-F]+ %make_hex
  KVs        <-  oSP "=" oSP
  CBo        <-  "{" oSP
  CBc        <- oSP "}" _
  SBo        <-  "[" oSP
  SBc        <-  oSP "]"
  COMMA      <-  oSP "," oSP
  TAIL       <-  _ (COMMENT)*
  COMMENT    <-  "#" [^\n\r]* _
  oSP        <-  [ \t]*      # optional space
  SP         <-  [ \t]+      # mandatory space
  NL         <-  [\r\n]+     # (required) new line
  _          <-  [ \t\r\n]*  # All whitespace
*/
// clang-format on

struct WS_ : peg::star<peg::space> {};
struct NL : peg::plus<peg::eol> {};
struct SP : peg::plus<peg::blank> {};
struct oSP : peg::star<peg::blank> {};
struct COMMENT : peg::seq<peg::one<'#'>, peg::until<peg::eol>, WS_> {};
struct TAIL : peg::seq<WS_, peg::star<COMMENT>> {};
struct COMMA : peg::pad<peg::one<','>, peg::blank> {};
struct SBo : peg::pad<peg::one<'['>, peg::blank> {};
struct SBc : peg::pad<peg::one<']'>, peg::blank> {};
struct CBo : peg::pad<peg::one<'{'>, peg::blank> {};
struct CBc : peg::pad<peg::one<'}'>, peg::blank> {};
struct KVs : peg::pad<peg::one<'='>, peg::blank> {};

struct STRUCTk : TAO_PEGTL_KEYWORD("struct") {};
struct PROTOk : TAO_PEGTL_KEYWORD("proto") {};
struct REFk : TAO_PEGTL_KEYWORD("reference") {};
struct ASk : TAO_PEGTL_KEYWORD("as") {};
struct ENDk : TAO_PEGTL_KEYWORD("end") {};

struct RESERVED : peg::sor<STRUCTk, PROTOk, REFk, ASk, ENDk> {};

struct HEXTAG : peg::seq<peg::one<'0'>, peg::one<'x', 'X'>> {};
struct HEX : peg::seq<HEXTAG, peg::plus<peg::xdigit>> {};

// TODO: Enforce that variables start with a letter?
struct VAR : peg::seq<peg::one<'$'>, peg::plus<peg::ranges<'A', 'Z', '0', '9', '_'>>> {};

struct sign : peg::one<'+', '-'> {};
struct exp : peg::seq<peg::one<'e', 'E'>, peg::opt<sign>, peg::plus<peg::digit>> {};
struct INTEGER
    : peg::seq<peg::opt<sign>,
               peg::sor<peg::one<'0'>, peg::seq<peg::range<'1', '9'>, peg::star<peg::digit>>>> {};
struct FLOAT : peg::seq<INTEGER, peg::one<'.'>, peg::star<peg::digit>, peg::opt<exp>> {};
struct NUMBER : peg::sor<FLOAT, INTEGER> {};

struct STRING : peg::seq<peg::one<'"'>, peg::plus<peg::not_one<'"'>>, peg::one<'"'>> {};

struct VALUE;
// Should the 'space' here be a 'blank'? Allow multi-line lists (w/o \)?
struct LIST : peg::seq<SBo, peg::list<VALUE, COMMA, peg::space>, SBc> {};

struct VALUE : peg::sor<LIST, HEX, NUMBER, STRING> {};

// Account for the reserved keyword: "end" when looking for keys (don't match "end" as a key, ever!)
struct KEY : peg::seq<peg::not_at<RESERVED>, peg::range<'a', 'z'>,
                      peg::star<peg::ranges<'a', 'z', 'A', 'Z', '0', '9', '_'>>> {};
struct FLAT_KEY : peg::list<KEY, peg::one<'.'>> {};
struct REF_VARADD : peg::seq<peg::one<'+'>, KEY, KVs, VALUE, TAIL> {};
struct REF_VARSUB : peg::seq<VAR, KVs, VALUE, TAIL> {};

struct VAR_REF : peg::seq<TAO_PEGTL_STRING("$("), FLAT_KEY, peg::one<')'>> {};

struct FULLPAIR : peg::seq<FLAT_KEY, KVs, peg::sor<VALUE, VAR_REF>, TAIL> {};
struct PAIR : peg::seq<KEY, KVs, peg::sor<VALUE, VAR_REF>, TAIL> {};
struct PROTO_PAIR : peg::seq<KEY, KVs, peg::sor<VAR, VAR_REF, VALUE>, TAIL> {};

// TODO: Fix this "end" is being matched as a "KEY", but it shouldn't be.
struct END : peg::seq<ENDk, SP, KEY> {};

struct REFs : peg::seq<REFk, SP, FLAT_KEY, SP, ASk, SP, KEY, TAIL> {};
struct REFc : peg::plus<peg::sor<REF_VARSUB, REF_VARADD>> {};
struct REFERENCE : peg::seq<REFs, REFc, END, WS_> {};

struct PROTOc;
struct PROTOs : peg::seq<PROTOk, SP, KEY, TAIL> {};
struct PROTO : peg::seq<PROTOs, PROTOc, END, WS_> {};

struct STRUCTc;
struct STRUCTs : peg::seq<STRUCTk, SP, KEY, TAIL> {};
struct STRUCT : peg::seq<STRUCTs, STRUCTc, END, WS_> {};

struct PROTOc : peg::plus<peg::sor<PROTO_PAIR, STRUCT, REFERENCE, PROTO>> {};
struct STRUCTc : peg::plus<peg::sor<STRUCT, PAIR, REFERENCE, PROTO>> {};

// TODO: Improve this. A single file should look like this:
//
//  1. Optional list of include files
//  2. Elements of a config file: struct, proto, reference, pair
//
// How do we fit in flat keys? I think we want to support flat keys or
// structured keys in a single file. But not both.

struct INCLUDE : peg::seq<TAO_PEGTL_KEYWORD("include"), SP, filename::grammar, TAIL> {};
struct include_list : peg::star<INCLUDE> {};

// This is a little weird, but we need to match the `FULLPAIR` before `STRUCTc`, otherwise the
// `PAIR` that can be contained by `STRUCTc` will start to match a single key, then fall back to the
// `FULLPAIR` match, which results in the first `KEY` of the `FLAT_KEY` in the `FULLPAIR` to be
// matched twice.
struct CONFIG
    : peg::seq<TAIL, include_list,
               peg::sor<peg::seq<peg::not_at<PAIR>, peg::plus<FULLPAIR>>, STRUCTc>, TAIL> {};

struct grammar : peg::seq<CONFIG, peg::eolf> {};

}  // namespace config
