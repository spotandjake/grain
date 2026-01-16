open Grain_parsing;
open Grain_typed;
open Grain_utils;
open Types;
open Anftree;

type str = Location.loc(string);
type env = Env.t;
type ident = Ident.t;
type attributes = Typedtree.attributes;

module Imm: {
  let mk:
    (~loc: Location.t, ~env: env=?, imm_expression_desc) => imm_expression;
  let id: (~loc: Location.t, ~env: env=?, ident) => imm_expression;
  let const: (~loc: Location.t, ~env: env=?, constant) => imm_expression;
  let trap: (~loc: Location.t, ~env: env=?, unit) => imm_expression;
};

module Comp: {
  let mk:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type=?,
      ~env: env=?,
      comp_expression_desc
    ) =>
    comp_expression;
  let imm:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      imm_expression
    ) =>
    comp_expression;
  let number:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      Asttypes.number_type
    ) =>
    comp_expression;
  let int32:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, int32) =>
    comp_expression;
  let int64:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, int64) =>
    comp_expression;
  let uint32:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, int32) =>
    comp_expression;
  let uint64:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, int64) =>
    comp_expression;
  let float32:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, float) =>
    comp_expression;
  let float64:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, float) =>
    comp_expression;
  let prim0:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      prim0
    ) =>
    comp_expression;
  let prim1:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      prim1,
      imm_expression
    ) =>
    comp_expression;
  let prim2:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      prim2,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let primn:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      primn,
      list(imm_expression)
    ) =>
    comp_expression;
  let box_assign:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let local_assign:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      Ident.t,
      imm_expression
    ) =>
    comp_expression;
  let assign:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let tuple:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      list(imm_expression)
    ) =>
    comp_expression;
  let array:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      list(imm_expression)
    ) =>
    comp_expression;
  let array_get:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let array_set:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      imm_expression,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let record:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      imm_expression,
      imm_expression,
      list((option(str), imm_expression))
    ) =>
    comp_expression;
  let adt:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      imm_expression,
      imm_expression,
      imm_expression,
      list(imm_expression)
    ) =>
    comp_expression;
  let tuple_get:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      int32,
      imm_expression
    ) =>
    comp_expression;
  let tuple_set:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      int32,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let adt_get:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      int32,
      imm_expression
    ) =>
    comp_expression;
  let adt_get_tag:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      imm_expression
    ) =>
    comp_expression;
  let record_get:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      int32,
      imm_expression
    ) =>
    comp_expression;
  let record_set:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      int32,
      imm_expression,
      imm_expression
    ) =>
    comp_expression;
  let if_:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      imm_expression,
      anf_expression,
      anf_expression
    ) =>
    comp_expression;
  let for_:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      option(anf_expression),
      option(anf_expression),
      anf_expression
    ) =>
    comp_expression;
  let continue:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, unit) =>
    comp_expression;
  let break:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, unit) =>
    comp_expression;
  let return:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      option(imm_expression)
    ) =>
    comp_expression;
  let switch_:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      imm_expression,
      list((int, anf_expression)),
      partial
    ) =>
    comp_expression;
  let app:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~allocation_type: allocation_type,
      ~env: env=?,
      ~tail: bool=?,
      (imm_expression, (list(allocation_type), allocation_type)),
      list(imm_expression)
    ) =>
    comp_expression;
  let lambda:
    (
      ~loc: Location.t,
      ~attributes: attributes=?,
      ~env: env=?,
      ~name: string=?,
      list((ident, allocation_type)),
      (anf_expression, allocation_type)
    ) =>
    comp_expression;
  let bytes:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, bytes) =>
    comp_expression;
  let string:
    (~loc: Location.t, ~attributes: attributes=?, ~env: env=?, string) =>
    comp_expression;
};

module AExp: {
  let mk:
    (
      ~loc: Location.t,
      ~env: env=?,
      ~alloc_type: allocation_type,
      anf_expression_desc
    ) =>
    anf_expression;
  let let_:
    (
      ~loc: Location.t,
      ~env: env=?,
      ~global: global_flag=?,
      ~mut_flag: mut_flag=?,
      rec_flag,
      list((ident, comp_expression)),
      anf_expression
    ) =>
    anf_expression;
  let seq:
    (~loc: Location.t, ~env: env=?, comp_expression, anf_expression) =>
    anf_expression;
  let comp: (~loc: Location.t, ~env: env=?, comp_expression) => anf_expression;
};

module IncludeDeclaration: {
  let mk: (ident, import_desc, import_shape, global_flag) => import_spec;
  let grain_value:
    (~global: global_flag=?, ident, string, string, import_shape) =>
    import_spec;
  let wasm_func:
    (~global: global_flag=?, ident, string, string, import_shape) =>
    import_spec;
  let wasm_value:
    (~global: global_flag=?, ident, string, string, import_shape) =>
    import_spec;
};
