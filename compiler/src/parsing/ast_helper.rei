/* This file is largely copied from OCaml's parsing/ast_helper.mli.
   The original copyright notice is reproduced below. */
/**************************************************************************/
/*                                                                        */
/*                                 OCaml                                  */
/*                                                                        */
/*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           */
/*                                                                        */
/*   Copyright 1996 Institut National de Recherche en Informatique et     */
/*     en Automatique.                                                    */
/*                                                                        */
/*   All rights reserved.  This file is distributed under the terms of    */
/*   the GNU Lesser General Public License version 2.1, with the          */
/*   special exception on linking described in the file LICENSE.          */
/*                                                                        */
/**************************************************************************/

open Grain_utils;
open Parsetree;

type id = Location.loc(Identifier.t);
type str = Location.loc(string);

module Number: {
  let rational: (str, Location.t, str) => number_type;
};

module Constant: {
  let bytes: str => constant;
  let string: str => constant;
  let char: str => constant;
  let number: number_type => constant;
  let int8: str => constant;
  let int16: str => constant;
  let int32: str => constant;
  let int64: str => constant;
  let uint8: str => constant;
  let uint16: str => constant;
  let uint32: str => constant;
  let uint64: str => constant;
  let float32: str => constant;
  let float64: str => constant;
  let wasmi32: str => constant;
  let wasmi64: str => constant;
  let wasmf32: str => constant;
  let wasmf64: str => constant;
  let bigint: str => constant;
  let rational: str => constant;
  let bool: bool => constant;
  let void: constant;
};

module Type: {
  let mk: (~loc: Location.t, parsed_type_desc) => parsed_type;
  let any: (~loc: Location.t, unit) => parsed_type;
  let var: (~loc: Location.t, string) => parsed_type;
  let arrow:
    (~loc: Location.t, list(parsed_type_argument), parsed_type) => parsed_type;
  let tuple: (~loc: Location.t, list(parsed_type)) => parsed_type;
  let constr: (~loc: Location.t, id, list(parsed_type)) => parsed_type;
  let poly: (~loc: Location.t, list(str), parsed_type) => parsed_type;
  let force_poly: parsed_type => parsed_type;
};

module ConstructorDeclaration: {
  let mk:
    (~loc: Location.t, str, constructor_arguments) => constructor_declaration;
  let singleton: (~loc: Location.t, str) => constructor_declaration;
  let tuple:
    (~loc: Location.t, str, Location.loc(list(parsed_type))) =>
    constructor_declaration;
  let record:
    (~loc: Location.t, str, Location.loc(list(label_declaration))) =>
    constructor_declaration;
};

module LabelDeclaration: {
  let mk: (~loc: Location.t, id, parsed_type, mut_flag) => label_declaration;
};

module DataDeclaration: {
  let mk:
    (
      ~loc: Location.t,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      data_kind,
      option(parsed_type)
    ) =>
    data_declaration;
  let abstract:
    (
      ~loc: Location.t,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      option(parsed_type)
    ) =>
    data_declaration;
  let variant:
    (
      ~loc: Location.t,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      list(constructor_declaration)
    ) =>
    data_declaration;
  let record:
    (
      ~loc: Location.t,
      ~rec_flag: rec_flag=?,
      str,
      list(parsed_type),
      list(label_declaration)
    ) =>
    data_declaration;
};

module Exception: {
  let mk: (~loc: Location.t, str, constructor_arguments) => type_exception;
  let singleton: (~loc: Location.t, str) => type_exception;
  let tuple:
    (~loc: Location.t, str, Location.loc(list(parsed_type))) =>
    type_exception;
  let record:
    (~loc: Location.t, str, Location.loc(list(label_declaration))) =>
    type_exception;
};

module Pattern: {
  let mk: (~loc: Location.t, pattern_desc) => pattern;
  let any: (~loc: Location.t, unit) => pattern;
  let var: (~loc: Location.t, str) => pattern;
  let tuple: (~loc: Location.t, list(pattern)) => pattern;
  let array: (~loc: Location.t, list(pattern)) => pattern;
  let record:
    (
      ~loc: Location.t,
      list((option((id, pattern)), Asttypes.closed_flag))
    ) =>
    pattern;
  let list: (~loc: Location.t, list(list_item(pattern))) => pattern;
  let constant: (~loc: Location.t, constant) => pattern;
  let constraint_: (~loc: Location.t, pattern, parsed_type) => pattern;
  let construct: (~loc: Location.t, id, constructor_pattern) => pattern;
  let singleton_construct: (~loc: Location.t, id) => pattern;
  let tuple_construct: (~loc: Location.t, id, list(pattern)) => pattern;
  let record_construct:
    (
      ~loc: Location.t,
      id,
      list((option((id, pattern)), Asttypes.closed_flag))
    ) =>
    pattern;
  let or_: (~loc: Location.t, pattern, pattern) => pattern;
  let alias: (~loc: Location.t, pattern, str) => pattern;
};

module Expression: {
  let mk:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression_desc
    ) =>
    expression;
  let ident:
    (~loc: Location.t, ~core_loc: Location.t, ~attributes: attributes=?, id) =>
    expression;
  let constant:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      constant
    ) =>
    expression;
  let tuple:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list(expression)
    ) =>
    expression;
  let record:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      option(expression),
      list((id, expression))
    ) =>
    expression;
  let record_fields:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list(record_item(expression))
    ) =>
    expression;
  let record_get:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      id
    ) =>
    expression;
  let record_set:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      id,
      expression
    ) =>
    expression;
  let list:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list(list_item(expression))
    ) =>
    expression;
  let array:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list(expression)
    ) =>
    expression;
  let array_get:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let array_set:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      ~infix_op: expression=?,
      ~lhs_loc: Location.t,
      expression,
      expression,
      expression
    ) =>
    expression;
  let let_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      rec_flag,
      mut_flag,
      list(value_binding)
    ) =>
    expression;
  let match:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      Location.loc(list(match_branch))
    ) =>
    expression;
  let prim0:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      prim0
    ) =>
    expression;
  let prim1:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      prim1,
      expression
    ) =>
    expression;
  let prim2:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      prim2,
      expression,
      expression
    ) =>
    expression;
  let primn:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      primn,
      list(expression)
    ) =>
    expression;
  let if_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      expression,
      option(expression)
    ) =>
    expression;
  let while_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let for_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      option(expression),
      option(expression),
      option(expression),
      expression
    ) =>
    expression;
  let continue:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      unit
    ) =>
    expression;
  let break:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      unit
    ) =>
    expression;
  let return:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      option(expression)
    ) =>
    expression;
  let constraint_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      parsed_type
    ) =>
    expression;
  let use:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      id,
      use_items
    ) =>
    expression;
  let box_assign:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let assign:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      expression
    ) =>
    expression;
  let lambda:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list(lambda_argument),
      expression
    ) =>
    expression;
  let apply:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      list(application_argument)
    ) =>
    expression;
  let construct:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      id,
      constructor_expression
    ) =>
    expression;
  let singleton_construct:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      Location.loc(Identifier.t)
    ) =>
    expression;
  let tuple_construct:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      id,
      list(expression)
    ) =>
    expression;
  let record_construct:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      id,
      list(record_item(expression))
    ) =>
    expression;
  let binop:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression,
      expression,
      expression
    ) =>
    expression;
  let block:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list(expression)
    ) =>
    expression;
  let ignore: expression => expression;
};

module Toplevel: {
  let mk:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      toplevel_stmt_desc
    ) =>
    toplevel_stmt;
  let include_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      include_declaration
    ) =>
    toplevel_stmt;
  let foreign:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      provide_flag,
      value_description
    ) =>
    toplevel_stmt;
  let module_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      provide_flag,
      module_declaration
    ) =>
    toplevel_stmt;
  let primitive:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      provide_flag,
      primitive_description
    ) =>
    toplevel_stmt;
  let data:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list((provide_flag, data_declaration, Location.t))
    ) =>
    toplevel_stmt;
  let let_:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      provide_flag,
      rec_flag,
      mut_flag,
      list(value_binding)
    ) =>
    toplevel_stmt;
  let expr:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      expression
    ) =>
    toplevel_stmt;
  let grain_exception:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      provide_flag,
      type_exception
    ) =>
    toplevel_stmt;
  let provide:
    (
      ~loc: Location.t,
      ~core_loc: Location.t,
      ~attributes: attributes=?,
      list(provide_item)
    ) =>
    toplevel_stmt;
};

module PrimitiveDescription: {
  let mk:
    (~loc: Location.t, ~ident: str, ~name: str, unit) => primitive_description;
};

module ValueDescription: {
  let mk:
    (
      ~loc: Location.t,
      ~mod_: str,
      ~name: str,
      ~alias: option(str),
      ~typ: parsed_type,
      unit
    ) =>
    value_description;
};

module ValueBinding: {
  let mk: (~loc: Location.t, pattern, expression) => value_binding;
};

module MatchBranch: {
  let mk:
    (~loc: Location.t, pattern, expression, option(expression)) =>
    match_branch;
};

module IncludeDeclaration: {
  let mk: (~loc: Location.t, str, str, option(str)) => include_declaration;
};

module TypeArgument: {
  let mk:
    (~loc: Location.t, Asttypes.argument_label, parsed_type) =>
    parsed_type_argument;
};

module LambdaArgument: {
  let mk: (~loc: Location.t, pattern, option(expression)) => lambda_argument;
};

module ModuleDeclaration: {
  let mk: (~loc: Location.t, str, list(toplevel_stmt)) => module_declaration;
};

module Attribute: {
  let mk: (~loc: Location.t, str, list(str)) => attribute;
};
