open Grain_parsing;
open Grain_utils;

type t;

let empty: t;

let from_comments: list(Parsetree.comment) => t;

let query: (t, Location.t) => list(Parsetree.comment);
let query_line: (t, int) => option(Parsetree.comment);
