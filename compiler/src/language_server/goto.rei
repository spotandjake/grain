open Grain_typed;

type goto_request_type =
  | Definition
  | TypeDefinition;

module RequestParams: {
  [@deriving yojson({strict: false})]
  type t;
};

// https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#location
module ResponseResult: {
  [@deriving yojson]
  type t;
};

let process:
  (
    ~id: Protocol.message_id,
    ~compiled_code: Hashtbl.t(Protocol.uri, Lsp_types.code),
    ~documents: Hashtbl.t(Protocol.uri, string),
    goto_request_type,
    RequestParams.t
  ) =>
  unit;
