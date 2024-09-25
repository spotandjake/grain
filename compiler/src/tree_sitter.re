// Referencing https://github.com/marceline-cramer/tree-sitter-grain/blob/main/src/grammar.json to compare output

// MenhirSdk API: https://gitlab.inria.fr/fpottier/menhir/-/blob/master/sdk/cmly_api.ml
module Grammar =
  MenhirSdk.Cmly_read.Read({
    let filename = "parsing/parser.cmly";
  });

type precedence =
  | Prec
  | Prec_LEFT
  | Prec_RIGHT
  | Prec_DYNAMIC;
// https://github.com/tree-sitter/tree-sitter/blob/20924fa4cdeb10d82ac308481e39bf8519334e55/cli/src/generate/parse_grammar.rs#L10
// Also useful: https://tree-sitter.github.io/tree-sitter/creating-parsers#the-grammar-dsl
type tree_sitter_node =
  | BlankRule
  | String({value: string})
  | Pattern({value: string})
  | Symbol({name: string})
  | Sequence({members: list(tree_sitter_node)})
  | Choice({members: list(tree_sitter_node)})
  | Alias({
      value: string,
      named: bool,
      content: tree_sitter_node,
    })
  | Repeat({content: tree_sitter_node})
  | RepeatOne({content: tree_sitter_node})
  | Token({
      immediate: bool,
      content: tree_sitter_node,
    })
  | Field({
      name: string,
      content: tree_sitter_node,
    })
  | Prec({
      precedence,
      value: int,
      content: tree_sitter_node,
    });

// Needs to map to JSONSchema defined at https://github.com/tree-sitter/tree-sitter/blob/20924fa4cdeb10d82ac308481e39bf8519334e55/cli/src/generate/grammar-schema.json
let rec yojson_of_tree_sitter_node = (node: tree_sitter_node) => {
  switch (node) {
  | BlankRule => `Assoc([("type", `String("BLANK"))])
  | String({value}) =>
    `Assoc([("type", `String("STRING")), ("value", `String(value))])
  | Pattern({value}) =>
    `Assoc([("type", `String("PATTERN")), ("value", `String(value))])
  | Symbol({name}) =>
    `Assoc([("type", `String("SYMBOL")), ("name", `String(name))])
  | Sequence({members}) =>
    `Assoc([
      ("type", `String("SEQ")),
      ("members", yojson_of_tree_sitter_nodes(members)),
    ])
  | Choice({members}) =>
    `Assoc([
      ("type", `String("CHOICE")),
      ("members", yojson_of_tree_sitter_nodes(members)),
    ])
  | Alias({value, named, content}) =>
    `Assoc([
      ("type", `String("ALIAS")),
      ("value", `String(value)),
      ("named", `Bool(named)),
      ("content", yojson_of_tree_sitter_node(content)),
    ])
  | Repeat({content}) =>
    `Assoc([
      ("type", `String("REPEAT")),
      ("content", yojson_of_tree_sitter_node(content)),
    ])
  | RepeatOne({content}) =>
    `Assoc([
      ("type", `String("REPEAT1")),
      ("content", yojson_of_tree_sitter_node(content)),
    ])
  | Token({immediate, content}) =>
    `Assoc([
      ("type", `String(if (immediate) {"IMMEDIATE_TOKEN"} else {"TOKEN"})),
      ("content", yojson_of_tree_sitter_node(content)),
    ])
  | Field({name, content}) =>
    `Assoc([
      ("type", `String("FIELD")),
      ("name", `String(name)),
      ("content", yojson_of_tree_sitter_node(content)),
    ])
  | Prec({precedence, value, content}) =>
    let token_type =
      switch (precedence) {
      | Prec => "PREC"
      | Prec_LEFT => "PREC_LEFT"
      | Prec_RIGHT => "PREC_RIGHT"
      | Prec_DYNAMIC => "PREC_DYNAMIC"
      };
    `Assoc([
      ("type", `String(token_type)),
      ("value", `Int(value)),
      ("content", yojson_of_tree_sitter_node(content)),
    ]);
  };
}
and yojson_of_tree_sitter_nodes = (members: list(tree_sitter_node)) => {
  `List(List.map(node => yojson_of_tree_sitter_node(node), members));
};

module StringMap =
  Map.Make({
    type t = string;
    let compare = compare;
  });

module StringSet =
  Set.Make({
    type t = string;
    let compare = compare;
  });

// https://github.com/tree-sitter/tree-sitter/blob/20924fa4cdeb10d82ac308481e39bf8519334e55/cli/src/generate/parse_grammar.rs#L75
type grammar = {
  name: string,
  rules: list((string, tree_sitter_node)),
  // precedences: Vec<Vec<RuleJSON>>,
  // conflicts: Vec<Vec<String>>,
  // externals: Vec<RuleJSON>,
  // extras: Vec<RuleJSON>,
  // inline: Vec<String>,
  // supertypes: Vec<String>,
  // word: Option<String>,
};
let yojson_of_grammar = (m: grammar) => {
  let rules =
    List.map(
      ((name, value)) => (name, yojson_of_tree_sitter_node(value)),
      m.rules,
    );
  `Assoc([("name", `String(m.name)), ("rules", `Assoc(rules))]);
};

type converter_state = {ignore_list: StringSet.t};
// Helpers
let convert_symbol =
    (converter_state: converter_state, symbol: Grammar.symbol) => {
  let symbol_name = Grammar.symbol_name(symbol, ~mangled=true);
  if (!StringSet.mem(symbol_name, converter_state.ignore_list)) {
    Some(Symbol({name: symbol_name}));
  } else {
    None;
  };
};
let build_rule_grammar =
    (converter_state: converter_state, contents: list(list(Grammar.symbol))) => {
  /*
   * Rule value comes in like:
   * [
   *  [ rule1_tkn1, rule1_tkn2 ]
   *  [ rule2_tkn1, rule2_tkn2 ]
   * ]
   */
  // TODO: We want to merge the rule_contents so that we are not producing as much on the output
  switch (contents) {
  | [] => None
  | [rule] =>
    let rule =
      List.filter_map(t => convert_symbol(converter_state, t), rule);
    if (List.length(rule) != 0) {
      Some(Sequence({members: rule}));
    } else {
      None;
          // TODO: We should invesitgate if we ever hit here and why, it might indicate a bad rule
    };
  | rules =>
    let branches =
      List.filter_map(
        branch => {
          let branch =
            List.filter_map(t => convert_symbol(converter_state, t), branch);
          if (List.length(branch) != 0) {
            Some(Sequence({members: branch}));
          } else {
            None;
                // TODO: We should invesitgate if we ever hit here and why, it might indicate a bad rule
          };
        },
        rules,
      );
    switch (branches) {
    // TODO: Investigate if we ever hit here and why
    | [] => None
    | [branch] => Some(branch)
    | branches => Some(Choice({members: branches}))
    };
  };
};
let convert_rule =
    (
      converter_state,
      rule_name: string,
      is_optional: bool,
      rule_contents: list(list(Grammar.symbol)),
    ) => {
  let rule_contents = build_rule_grammar(converter_state, rule_contents);
  let rule_value =
    switch (rule_contents) {
    | Some(rule) when is_optional =>
      Some(Choice({members: [rule, BlankRule]}))
    | Some(rule) => Some(rule)
    | None => None
    };
  switch (rule_value) {
  | Some(rule_value) => Some((rule_name, rule_value))
  | None => None
  };
};

// let add_rule = (name: string, rule: tree_sitter_node, grammar: grammar) => {
//   grammar.rules := StringMap.add(name, rule, grammar.rules^);
// };

// let convert_symbol = (grammar: grammar, symbol: Grammar.symbol) => {
//   // TODO: We cannot just use the raw name, we also need to check for the properties of the symbol
//   let symbol_name = Grammar.symbol_name(symbol, ~mangled=true);
//   if (!StringSet.exists(s => s == symbol_name, grammar.ignore_list^)) {
//     switch (symbol) {
//     | N(non_terminal) =>
//       let is_optional = Grammar.Nonterminal.nullable(non_terminal);
//       if (is_optional) {
//         Some(Choice({members: [Symbol({name: symbol_name}), BlankRule]}));
//       } else {
//         Some(Symbol({name: symbol_name}));
//       };
//     | T(terminal) => Some(Symbol({name: symbol_name}))
//     };
//   } else {
//     None;
//   };
// };
// let convert_rule = (grammar: grammar, rule: list(Grammar.symbol)) => {
//   switch (rule) {
//   | [] => failwith("Invalid Rule convert_rule") // TODO: Validate this is the truth
//   | [token] => convert_symbol(grammar, token)
//   | tokens =>
//     let tokens = List.map(t => convert_symbol(grammar, t), tokens);
//     let tokens = List.filter_map(t => t, tokens);
//     if (List.length(tokens) != 0) {
//       Some(Sequence({members: tokens}));
//     } else {
//       None;
//     };
//   };
// };

// let convert_rule_contents =
//     (grammar: grammar, rule_contents: list(list(Grammar.symbol))) => {
//   switch (rule_contents) {
//   | [] => failwith("Invalid Rule") // TODO: Validate this is the truth
//   | [rule] => convert_rule(grammar: grammar, rule)
//   | rules =>
//     let rules = List.filter_map(r => convert_rule(grammar, r), rules);
//     if (List.length(rules) != 0) {
//       Some(Choice({members: rules}));
//     } else {
//       None;
//     };
//   };
// };

let _ = {
  // Map our Terminals
  let (ignore_list, token_list) =
    Grammar.Terminal.fold(
      (token, (ignore_list, token_list)) => {
        let token_name = Grammar.Terminal.name(token);
        let token_kind = Grammar.Terminal.kind(token);
        let should_include_token =
          switch (token_kind) {
          | `REGULAR
          | `EOF => true
          | _ => false
          };
        let (ignore_list, token_list) =
          if (should_include_token) {
            // Search for the token attributes
            let token_attributes = Grammar.Terminal.attributes(token);
            let token_value =
              List.find_map(
                attr => {
                  let attribute_label = Grammar.Attribute.label(attr);
                  switch (attribute_label) {
                  | "pattern" =>
                    let pattern = Grammar.Attribute.payload(attr);
                    let pattern =
                      Grain_utils.String_utils.slice(
                        ~first=1,
                        ~last=-1,
                        pattern,
                      );
                    let pattern = Scanf.unescaped(pattern);
                    Some(pattern);
                  | _ => None
                  };
                },
                token_attributes,
              );
            switch (token_value) {
            // TODO: We might need to work out something better then than
            | Some(pattern) when String.length(pattern) == 0 =>
              let ignore_list = StringSet.add(token_name, ignore_list);
              (ignore_list, token_list);
            | Some(pattern) =>
              let token_list =
                StringMap.add(
                  token_name,
                  Pattern({value: pattern}),
                  token_list,
                );
              (ignore_list, token_list);
            | None =>
              failwith(
                Printf.sprintf("Token %s missing pattern", token_name),
              )
            };
          } else {
            (ignore_list, token_list);
          };
        (ignore_list, token_list);
      },
      (StringSet.empty, StringMap.empty),
    );
  let token_list =
    StringMap.fold(
      (rule_name, rule, acc) => [(rule_name, rule), ...acc],
      token_list,
      [],
    );
  // Collect Our NonTerminals
  let (rule_list, _) =
    Grammar.Production.fold(
      (production, (rule_list, index)) => {
        let lhs = Grammar.Production.lhs(production);
        let rule_name = Grammar.Nonterminal.mangled_name(lhs);
        let rule_optional = Grammar.Nonterminal.nullable(lhs);
        let rhs = Grammar.Production.rhs(production);
        let rule_grammar =
          Array.to_list(Array.map(((symbol, _, _)) => symbol, rhs));
        let rule_list =
          StringMap.update(
            rule_name,
            rule =>
              switch (rule) {
              | Some((rule_order, rule_existing_optional, rule_grammars)) =>
                if (rule_existing_optional != rule_optional) {
                  failwith(
                    "Invalid Assumption, optional non optional same rule",
                  );
                } else {
                  Some((
                    rule_order,
                    rule_optional,
                    [rule_grammar, ...rule_grammars],
                  ));
                }
              | None => Some((index, rule_optional, [rule_grammar]))
              },
            rule_list,
          );
        (rule_list, index + 1);
      },
      (StringMap.empty, 0),
    );
  // Map the Nonterminals
  let converter_state = {ignore_list: ignore_list};
  let rule_list =
    StringMap.fold(
      (rule_name, (rule_order, rule_optional, rule_grammar), acc) =>
        [(rule_name, rule_order, rule_optional, rule_grammar), ...acc],
      rule_list,
      [],
    );
  let rule_list =
    List.sort(((_, a, _, _), (_, b, _, _)) => a - b, rule_list);
  // TODO: Investigate if we ever get a None rule this might be problematic
  let rule_list =
    List.filter_map(
      ((rule_name, _, rule_optioanl, rule_grammars)) =>
        convert_rule(
          converter_state,
          rule_name,
          rule_optioanl,
          rule_grammars,
        ),
      rule_list,
    );
  ();
  // TODO: Map Our Entry Points
  // Generate grammar
  let grammar = {name: "grain", rules: rule_list @ token_list};
  print_endline(Yojson.Safe.pretty_to_string(yojson_of_grammar(grammar)));
  // // Map Our Entry Points
  // let source_file_rule =
  //   Choice({
  //     members:
  //       List.map(
  //         ((rule_name, rule_production, _)) =>
  //           Symbol({name: Grammar.Nonterminal.mangled_name(rule_name)}),
  //         Grammar.Grammar.entry_points,
  //       ),
  //   });
  // add_rule("source_file", source_file_rule, grammar);
  // // Map Terminals
  // Grammar.Terminal.iter(t => {
  //   let token_name = Grammar.Terminal.name(t);
  //   let include_token =
  //     switch (Grammar.Terminal.kind(t)) {
  //     | `REGULAR
  //     | `EOF => true
  //     | _ => false
  //     };
  //   if (include_token) {
  //     let value =
  //       List.find_map(
  //         attr => {
  //           switch (Grammar.Attribute.label(attr)) {
  //           | "pattern" =>
  //             let pattern = Grammar.Attribute.payload(attr);
  //             let pattern =
  //               Grain_utils.String_utils.slice(~first=1, ~last=-1, pattern);
  //             let pattern = Scanf.unescaped(pattern);
  //             Some(Pattern({value: pattern}));
  //           | "ignore" => Some(BlankRule)
  //           | _ => None
  //           }
  //         },
  //         Grammar.Terminal.attributes(t),
  //       );
  //     switch (value) {
  //     | Some(BlankRule) =>
  //       grammar.ignore_list := StringSet.add(token_name, grammar.ignore_list^)
  //     | Some(v) => add_rule(token_name, v, grammar)
  //     // TODO: I think this should probably fail
  //     | None => ()
  //     };
  //   };
  // });
  // // Debug Help
  // Grammar.Production.iter(p => {
  //   let lhs = Grammar.Production.lhs(p);
  //   let rule_name = Grammar.Nonterminal.name(lhs);
  //   let rhs = Grammar.Production.rhs(p);
  //   let rule_contents =
  //     Array.fold_left(
  //       (acc, (sym, i, a)) =>
  //         acc ++ Grammar.symbol_name(sym) ++ " (" ++ i ++ "), ",
  //       "",
  //       rhs,
  //     );
  //   print_endline("// " ++ rule_name ++ " -> " ++ rule_contents);
  // });
};