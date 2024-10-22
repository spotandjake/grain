open Grain_typed;

let token_types: list(string);

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokensParams
module RequestParams: {
  [@deriving yojson({strict: false})]
  type t;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#semanticTokens
module ResponseResult: {
  [@deriving yojson]
  type t;
};

let process:
  (
    ~id: Protocol.message_id,
    ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
    ~documents: Hashtbl.t(Protocol.uri, string),
    RequestParams.t
  ) =>
  unit;