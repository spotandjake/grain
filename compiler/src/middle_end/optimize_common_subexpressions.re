open Anftree;
open Grain_typed;
open Grain_utils;
open Types;

module ExpressionHash = {
  type t = comp_expression_desc;
  let equal = (a, b) =>
    switch (a, b) {
    | (
        CAdt(
          {imm_desc: type_hash1},
          {imm_desc: ttag1},
          {imm_desc: vtag1},
          [],
        ),
        CAdt(
          {imm_desc: type_hash2},
          {imm_desc: ttag2},
          {imm_desc: vtag2},
          [],
        ),
      ) =>
      type_hash1 == type_hash2 && ttag1 == ttag2 && vtag1 == vtag2
    | _ => false
    };
  let hash = a =>
    switch (a) {
    | CAdt({imm_desc: type_hash}, {imm_desc: ttag}, {imm_desc: vtag}, []) =>
      Hashtbl.hash("CAdt")
      lxor Hashtbl.hash(type_hash)
      lxor Hashtbl.hash(ttag)
      lxor Hashtbl.hash(vtag)
    | _ => Hashtbl.hash(a)
    };
};

module ExpressionHashtbl = Hashtbl.Make(ExpressionHash);
let known_expressions = ExpressionHashtbl.create(50);
let rewrite_map = ref(Ident.empty: Ident.tbl(Ident.t));

let add_rewrite = (id, known_id) =>
  rewrite_map := Ident.add(id, known_id, rewrite_map^);
let get_rewrite = id => {
  switch (Ident.find_same_opt(id, rewrite_map^)) {
  | Some(known_id) => known_id
  | None => id
  };
};
let expr_is_pure = expr =>
  switch (Analyze_purity.comp_expression_purity(expr)) {
  | Some(true) => true
  | _ => false
  };
let add_expression = (id, {comp_desc} as expr) => {
  switch (comp_desc) {
  | CAdt(_, _, _, []) =>
    switch (ExpressionHashtbl.find_opt(known_expressions, comp_desc)) {
    | Some(known_id) => add_rewrite(id, known_id)
    | None when expr_is_pure(expr) =>
      ExpressionHashtbl.add(known_expressions, comp_desc, id)
    | None => ()
    }
  | _ => ()
  };
};

let remove_expression = (id, {comp_desc}) => {
  switch (ExpressionHashtbl.find_opt(known_expressions, comp_desc)) {
  | Some(known_id) when id == known_id =>
    ExpressionHashtbl.remove(known_expressions, comp_desc)
  | _ => ()
  };
};

module CommonSubExpressionArg: Anf_mapper.MapArgument = {
  include Anf_mapper.DefaultMapArgument;
  let enter_anf_expression = ({anf_desc: desc} as a) => {
    switch (desc) {
    | AELet(_, _, _, binds, _) =>
      List.iter(((id, expr)) => add_expression(id, expr), binds)
    | _ => ()
    };
    a;
  };

  let leave_imm_expression = ({imm_desc: desc} as i) =>
    switch (desc) {
    | ImmId(id) => {...i, imm_desc: ImmId(get_rewrite(id))}
    | _ => i
    };

  let leave_anf_expression = ({anf_desc: desc} as a) => {
    switch (desc) {
    | AELet(_, _, _, binds, _) =>
      List.iter(((id, expr)) => remove_expression(id, expr), binds)
    | _ => ()
    };
    a;
  };
};

module CSEMapper = Anf_mapper.MakeMap(CommonSubExpressionArg);

let optimize = anfprog => {
  // Reset state
  rewrite_map := Ident.empty;
  CSEMapper.map_anf_program(anfprog);
};