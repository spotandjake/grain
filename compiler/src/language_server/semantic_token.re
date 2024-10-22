open Grain_typed;
open Grain_diagnostics;

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokensLegend
[@deriving (enum, yojson)]
type semantic_token =
  | [@value 0] Namespace
  | Type
  | Enum
  | Interface
  | Struct
  | TypeParameter
  | Parameter
  | Variable
  | Property
  | EnumMember
  | Function
  | Macro
  | Keyword
  | Modifier
  | Comment
  | String
  | Number
  | Regexp
  | Operator
  | Decorator;

type semantic_token_item = {
  line: int,
  startChar: int,
  length: int,
  token_type: semantic_token,
  // TODO: token_modifiers
};

let token_types = [
  "namespace",
  "type",
  "enum",
  "interface",
  "struct",
  "typeParameter",
  "parameter",
  "variable",
  "property",
  "enumMember",
  "function",
  "macro",
  "keyword",
  "modifier",
  "comment",
  "string",
  "number",
  "regexp",
  "operator",
  "decorator",
];

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokensParams
module RequestParams = {
  [@deriving yojson({strict: false})]
  type t = {
    [@key "textDocument"]
    text_document: Protocol.text_document_identifier,
  };
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokens
module ResponseResult = {
  [@deriving yojson]
  type t = {data: list(int)};
};

let send_empty_response = (~id: Protocol.message_id) =>
  Protocol.response(~id, `Null);

let send_semantic_token_response =
    (~id: Protocol.message_id, tokens: list(semantic_token_item)) => {
  let tokens =
    List.sort(
      (tknA, tknB) =>
        if (tknA.line == tknB.line) {
          tknA.startChar - tknB.startChar;
        } else {
          tknA.line - tknB.line;
        },
      tokens,
    );
  Trace.log("Sorted Tokens");
  List.iter(
    tkn => {
      Trace.log(
        Printf.sprintf(
          "(line: %d, startChar: %d, length: %d)",
          tkn.line,
          tkn.startChar,
          tkn.length,
        ),
      )
    },
    tokens,
  );
  let (_, _, tokens) =
    List.fold_left(
      ((last_line, last_start_char, acc), token: semantic_token_item) => {
        let last_start_char =
          if (last_line != token.line) {
            0;
          } else {
            last_start_char;
          };
        let delta_line = token.line - last_line - 1;
        let delta_start_char = token.startChar - last_start_char;
        let token = {
          line: delta_line,
          startChar: delta_start_char,
          length: token.length,
          token_type: token.token_type,
        };
        (token.line, token.startChar, [token, ...acc]);
      },
      (0, 0, []),
      tokens,
    );
  let tokens = List.rev(tokens);
  Trace.log("Mapped Tokens");
  List.iter(
    tkn => {
      Trace.log(
        Printf.sprintf(
          "(line: %d, startChar: %d, length: %d)",
          tkn.line,
          tkn.startChar,
          tkn.length,
        ),
      )
    },
    tokens,
  );
  let data =
    List.fold_right(
      (token: semantic_token_item, data) => {
        let encoded_token_type = semantic_token_to_enum(token.token_type);
        let encoded_token_modifiers = 0;
        [
          token.line,
          token.startChar,
          token.length,
          encoded_token_type,
          encoded_token_modifiers,
          ...data,
        ];
      },
      tokens,
      [],
    );
  Protocol.response(~id, ResponseResult.to_yojson({data: data}));
};

let build_token = (loc: Grain_utils.Warnings.loc, token_type: semantic_token) => {
  let (_, startline, startchar, _) =
    Locations.get_raw_pos_info(loc.loc_start);
  let length = loc.loc_end.Lexing.pos_cnum - loc.loc_start.Lexing.pos_cnum;
  {line: startline, startChar: startchar, length, token_type};
};
let collect_tokens = program => {
  open Typedtree;
  // TODO: It would be better to collect the tokens from the parser
  let tokens = ref([]);
  module Iterator =
    TypedtreeIter.MakeIterator({
      include TypedtreeIter.DefaultIteratorArgument;
      // let leave_typed_program: typed_program => unit;
      // let leave_pattern: pattern => unit;
      let leave_expression = (expr: expression) => {
        switch (expr.exp_desc) {
        | TExpIdent(_, _, _) =>
          tokens := [build_token(expr.exp_loc, Variable), ...tokens^]
        | _ => ()
        };
      };
      // let leave_core_type: core_type => unit;
      // let leave_toplevel_stmt: toplevel_stmt => unit;
      // leave_binding
      // let leave_data_declaration = (decl: Typedtree.data_declaration) => {
      //   ();
      // };
      // TODO: Remove code below
      let enter_toplevel_stmt = (stmt: toplevel_stmt) => {
        switch (stmt.ttop_desc) {
        | TTopInclude({tinc_loc}) =>
          tokens := [build_token(tinc_loc, Keyword), ...tokens^]
        | _ => ()
        };
      };
    });
  Iterator.iter_typed_program(program);
  tokens^;
};

let process =
    (
      ~id: Protocol.message_id,
      ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
      ~documents: Hashtbl.t(Protocol.uri, string),
      params: RequestParams.t,
    ) => {
  let program =
    switch (Hashtbl.find_opt(compiled_code, params.text_document.uri)) {
    | None => None
    | Some({program}) => Some(program)
    };
  let tokens =
    switch (program) {
    | None => []
    | Some(program) => collect_tokens(program)
    };
  switch (tokens) {
  | [] => send_empty_response(~id)
  | tokens => send_semantic_token_response(~id, tokens)
  };
};