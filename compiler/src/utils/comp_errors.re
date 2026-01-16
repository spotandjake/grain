// Compiler error and warning handling
module Message = {
  type number_type =
    | Int8
    | Int16
    | Int32
    | Int64
    | Uint8
    | Uint16
    | Uint32
    | Uint64
    | Float32
    | Float64
    | Rational
    | BigInt;
  type t =
    // Warnings
    | LetRecNonFunction(string)
    | AmbiguousName(list(string), list(string), bool)
    | NotPrincipal(string)
    | NameOutOfScope(string, list(string), bool)
    | StatementType
    | NonreturningStatement
    | AllClausesGuarded
    | PartialMatch(string)
    | FragileMatch(string)
    | UnusedMatch
    | UnusedPat
    | UnusedExtension
    | NonClosedRecordPattern(string)
    | UnreachableCase
    | ShadowConstructor(string)
    | NoCmiFile(string, option(string))
    | FuncWasmUnsafe(string, string, string)
    | FromNumberLiteral(number_type, string, string)
    | UselessRecordSpread
    | PrintUnsafe(string)
    | ToStringUnsafe(string)
    | ArrayIndexNonInteger(string)
    // Lexer errors
    | UnrecognizedToken
    | UnclosedString(int)
    | UnclosedBytes(int)
    | UnclosedChar(int)
    | UnclosedBlockComment(int)
    | UnclosedDocComment(int)
    | FloatWithoutLeadingZero(string)
    // Parsing errors
    | BadEncoding
    | SyntaxError(string)
    | NoInlineMutableRecordField
    | NoExceptionMutableRecordField
    | RecordSpreadNotLeading
    | MultipleRecordSpreads
    | RecordSpreadInConstructor
    | DefaultArgumentNotNamed
    | ListSpreadNotTrailing
    | InvalidModuleInclude
    | InvalidModuleAlias
    // Well-formedness errors
    | MalformedString
    | EmptyRecordPattern
    | RHSLetRecMayOnlyBeFunction
    | NoLetRecMut
    | RationalZeroDenominator
    | UnknownAttribute(string, string)
    | InvalidAttributeArity(string, int)
    | AttributeDisallowed(string)
    | LoopControlOutsideLoop(string)
    | ReturnStatementOutsideFunction
    | MismatchedReturnStyles
    | LocalIncludeStatement
    | ProvidedMultipleTimes(string)
    | MutualRecTypesMissingRec
    | MutualRecExtraneousNonfirstRec
    // Typechecker errors
    | UnableToParseBigIntLiteral(string)
    | NumberLiteralOutOfFloatRange(string)
    | NumberLiteralOutOfRationalRange(string, string)
    | IntLiteralOutOfRange(string, string)
    | UintLiteralOutOfRange(string, string)
    | UintLiteralWithSign(string, string, option(string))
    | WasmLiteralOutOfRange(string, string, string)
    | FloatLiteralOutOfRange(string, string)
    | RationalLiteralOutOfRange(string)
    | InvalidBytesLiteral(string)
    | InvalidStringLiteral(string)
    | InvalidCharLiteral(string)
    | No_module_file(string, option(string));

  let default_severity: t => Asai.Diagnostic.severity =
    fun
    // Warnings
    | LetRecNonFunction(_)
    | AmbiguousName(_, _, _)
    | NotPrincipal(_)
    | NameOutOfScope(_, _, _)
    | StatementType
    | NonreturningStatement
    | AllClausesGuarded
    | PartialMatch(_)
    | FragileMatch(_)
    | UnusedMatch
    | UnusedPat
    | UnusedExtension
    | NonClosedRecordPattern(_)
    | UnreachableCase
    | ShadowConstructor(_)
    | NoCmiFile(_, _)
    | FuncWasmUnsafe(_, _, _)
    | FromNumberLiteral(_, _, _)
    | UselessRecordSpread
    | PrintUnsafe(_)
    | ToStringUnsafe(_)
    | ArrayIndexNonInteger(_) => Asai.Diagnostic.Warning
    // Lexer errors
    | UnrecognizedToken
    | UnclosedString(_)
    | UnclosedBytes(_)
    | UnclosedChar(_)
    | UnclosedBlockComment(_)
    | UnclosedDocComment(_)
    | FloatWithoutLeadingZero(_) => Asai.Diagnostic.Error
    // Parsing errors
    | BadEncoding
    | SyntaxError(_)
    | NoInlineMutableRecordField
    | NoExceptionMutableRecordField
    | RecordSpreadNotLeading
    | MultipleRecordSpreads
    | RecordSpreadInConstructor
    | DefaultArgumentNotNamed
    | ListSpreadNotTrailing
    | InvalidModuleInclude
    | InvalidModuleAlias => Asai.Diagnostic.Error
    // Well-formedness errors
    | MalformedString
    | EmptyRecordPattern
    | RHSLetRecMayOnlyBeFunction
    | NoLetRecMut
    | RationalZeroDenominator
    | UnknownAttribute(_, _)
    | InvalidAttributeArity(_, _)
    | AttributeDisallowed(_)
    | LoopControlOutsideLoop(_)
    | ReturnStatementOutsideFunction
    | MismatchedReturnStyles
    | LocalIncludeStatement
    | ProvidedMultipleTimes(_)
    | MutualRecTypesMissingRec
    | MutualRecExtraneousNonfirstRec => Asai.Diagnostic.Error
    // Typechecker errors
    | UnableToParseBigIntLiteral(_)
    | NumberLiteralOutOfFloatRange(_)
    | NumberLiteralOutOfRationalRange(_, _)
    | IntLiteralOutOfRange(_, _)
    | UintLiteralOutOfRange(_, _)
    | UintLiteralWithSign(_, _, _)
    | WasmLiteralOutOfRange(_, _, _)
    | FloatLiteralOutOfRange(_, _)
    | RationalLiteralOutOfRange(_)
    | InvalidBytesLiteral(_)
    | InvalidStringLiteral(_)
    | InvalidCharLiteral(_)
    | No_module_file(_, _) => Asai.Diagnostic.Error;

  /**
   * Maps a given error/warning to a short code string.
   *
   * Codes are intended to be short, stable and unique identifiers for
   * each error/warning, suitable for use in documentation.
   *
   * Codes are mapped into the following ranges:
   * - W1001-W1999: Compiler Warnings
   * - E2001-E2999: Lexer Errors
   * - E3001-E3999: Parser Errors
   * - E4001-E4999: Well-formedness Errors
   * - E5001-E5999: Typechecker Errors
   *
   * With `W` standing for "Warning" and `E` for "Error" respectively.
   */
  let short_code: t => string =
    fun
    // Warnings
    | LetRecNonFunction(_) => "W1001"
    | AmbiguousName(_, _, _) => "W1002"
    | NotPrincipal(_) => "W1003"
    | NameOutOfScope(_, _, _) => "W1004"
    | StatementType => "W1005"
    | NonreturningStatement => "W1006"
    | AllClausesGuarded => "W1007"
    | PartialMatch(_) => "W1008"
    | FragileMatch(_) => "W1009"
    | UnusedMatch => "W1010"
    | UnusedPat => "W1011"
    | UnusedExtension => "W1012"
    | NonClosedRecordPattern(_) => "W1013"
    | UnreachableCase => "W1014"
    | ShadowConstructor(_) => "W1015"
    | NoCmiFile(_, _) => "W1016"
    | FuncWasmUnsafe(_, _, _) => "W1017"
    | FromNumberLiteral(_, _, _) => "W1018"
    | UselessRecordSpread => "W1019"
    | PrintUnsafe(_) => "W1020"
    | ToStringUnsafe(_) => "W1021"
    | ArrayIndexNonInteger(_) => "W1022"
    // Lexer errors
    | UnrecognizedToken => "E2001"
    | UnclosedString(_) => "E2002"
    | UnclosedBytes(_) => "E2003"
    | UnclosedChar(_) => "E2004"
    | UnclosedBlockComment(_) => "E2005"
    | UnclosedDocComment(_) => "E2006"
    | FloatWithoutLeadingZero(_) => "E2007"
    // Parsing errors
    | BadEncoding => "E3001"
    | SyntaxError(msg) => "E3002"
    | NoInlineMutableRecordField => "E3003"
    | NoExceptionMutableRecordField => "E3004"
    | RecordSpreadNotLeading => "E3005"
    | MultipleRecordSpreads => "E3006"
    | RecordSpreadInConstructor => "E3007"
    | DefaultArgumentNotNamed => "E3008"
    | ListSpreadNotTrailing => "E3009"
    | InvalidModuleInclude => "E3010"
    | InvalidModuleAlias => "E3011"
    // Well-formedness errors
    | MalformedString => "E4001"
    | EmptyRecordPattern => "E4002"
    | RHSLetRecMayOnlyBeFunction => "E4003"
    | NoLetRecMut => "E4004"
    | RationalZeroDenominator => "E4005"
    | UnknownAttribute(_, _) => "E4006"
    | InvalidAttributeArity(_, _) => "E4007"
    | AttributeDisallowed(_) => "E4008"
    | LoopControlOutsideLoop(_) => "E4009"
    | ReturnStatementOutsideFunction => "E4010"
    | MismatchedReturnStyles => "E4011"
    | LocalIncludeStatement => "E4012"
    | ProvidedMultipleTimes(_) => "E4013"
    | MutualRecTypesMissingRec => "E4014"
    | MutualRecExtraneousNonfirstRec => "E4015"
    // Typechecker errors
    | UnableToParseBigIntLiteral(_) => "E5001"
    | NumberLiteralOutOfFloatRange(_) => "E5002"
    | NumberLiteralOutOfRationalRange(_, _) => "E5003"
    | IntLiteralOutOfRange(_, _) => "E5004"
    | UintLiteralOutOfRange(_, _) => "E5005"
    | UintLiteralWithSign(_, _, _) => "E5006"
    | WasmLiteralOutOfRange(_, _, _) => "E5007"
    | FloatLiteralOutOfRange(_, _) => "E5008"
    | RationalLiteralOutOfRange(_) => "E5009"
    | InvalidBytesLiteral(_) => "E5010"
    | InvalidStringLiteral(_) => "E5011"
    | InvalidCharLiteral(_) => "E5012"
    | No_module_file(_, _) => "E5013";
};

include Asai.Reporter.Make(Message);

/** Maps a given error/warning to a human-readable message. */
open Printf;
let get_message =
  fun
  // Warnings
  | Message.LetRecNonFunction(name) =>
    sprintf("'%s' is not a function, but is bound with 'let rec'", name)
  | NotPrincipal(s) => sprintf("%s is not principal.", s)
  | NameOutOfScope(ty, [nm], false) =>
    sprintf(
      "%s was selected from type %s.\nIt is not visible in the current scope, and will not \nbe selected if the type becomes unknown.",
      nm,
      ty,
    )
  // TODO: Why does this just assert false???
  | NameOutOfScope(_, _, false) => assert(false)
  | NameOutOfScope(ty, slist, true) =>
    sprintf(
      "this record of type %s contains fields that are \nnot visible in the current scope: %s.\nThey will not be selected if the type becomes unknown.",
      ty,
      String.concat(" ", slist),
    )
  | AmbiguousName([s], tl, false) =>
    sprintf(
      "%s belongs to several types: %s\nThe first one was selected. Please disambiguate if this is wrong.",
      s,
      String.concat(" ", tl),
    )
  // TODO: Why does this just assert false???
  | AmbiguousName(_, _, false) => assert(false)
  | AmbiguousName(_, tl, true) =>
    sprintf(
      "these field labels belong to several types: %s\nThe first one was selected. Please disambiguate if this is wrong.",
      String.concat(" ", tl),
    )
  | StatementType => "this expression should have type void."
  | NonreturningStatement => "this statement never returns (or has an unsound type)."
  | AllClausesGuarded => "this pattern-matching is not exhaustive.\nAll clauses in this pattern-matching are guarded."
  | PartialMatch("") => "this pattern-matching is not exhaustive."
  | PartialMatch(s) =>
    sprintf(
      "this pattern-matching is not exhaustive.\nHere is an example of a case that is not matched:\n%s",
      s,
    )
  | FragileMatch("") => "this pattern-matching is fragile."
  | FragileMatch(s) =>
    sprintf(
      "this pattern-matching is fragile.\nIt will remain exhaustive when constructors are added to type %s.",
      s,
    )
  | UnusedMatch => "this match case is unused."
  | UnusedPat => "this sub-pattern is unused."
  | UnusedExtension => "this type extension is unused."
  | UnreachableCase => "this match case is unreachable."
  | ShadowConstructor(s) =>
    sprintf(
      "the pattern variable %s shadows a constructor of the same name.",
      s,
    )
  | NoCmiFile(name, None) =>
    sprintf("no cmi file was found in path for module %s", name)
  | NoCmiFile(name, Some(msg)) =>
    sprintf(
      "no valid cmi file was found in path for module %s. %s",
      name,
      msg,
    )
  | NonClosedRecordPattern(s) =>
    sprintf(
      "the following fields are missing from the record pattern: %s\nUse `_` to ignore unused fields.",
      s,
    )
  | FuncWasmUnsafe(func, f, m) =>
    sprintf(
      "it looks like you are using `%s` on an unsafe Wasm function here.\nThis is generally unsafe and will cause errors. Use `%s` from the `%s` module instead.",
      func,
      f,
      m,
    )
  | FromNumberLiteral(mod_type, mod_name, n) => {
      let literal =
        switch (mod_type) {
        | Int8 => sprintf("%ss", n)
        | Int16 => sprintf("%sS", n)
        | Int32 => sprintf("%sl", n)
        | Int64 => sprintf("%sL", n)
        | Uint8 => sprintf("%sus", n)
        | Uint16 => sprintf("%suS", n)
        | Uint32 => sprintf("%sul", n)
        | Uint64 => sprintf("%suL", n)
        | Float32 => sprintf("%sf", String.contains(n, '.') ? n : n ++ ".")
        | Float64 => sprintf("%sd", String.contains(n, '.') ? n : n ++ ".")
        | Rational => sprintf("%sr", String.contains(n, '/') ? n : n ++ "/1")
        | BigInt => sprintf("%st", n)
        };
      sprintf(
        "it looks like you are calling %s.fromNumber() with a constant number. Try using the literal syntax (e.g. `%s`) instead.",
        mod_name,
        literal,
      );
    }
  | UselessRecordSpread => "this record spread is useless as all of the record's fields are overridden."
  | PrintUnsafe(typ) =>
    sprintf(
      "it looks like you are using `print` on an unsafe Wasm value here.\nThis is generally unsafe and will cause errors. Use `DebugPrint.print`%s from the `runtime/debugPrint` module instead.",
      typ,
    )
  | ToStringUnsafe(typ) =>
    sprintf(
      "it looks like you are using `toString` on an unsafe Wasm value here.\nThis is generally unsafe and will cause errors. Use `DebugPrint.toString`%s from the `runtime/debugPrint` module instead.",
      typ,
    )
  | ArrayIndexNonInteger(idx) =>
    sprintf("Array index should be an integer, but found `%s`.", idx)
  // Lexer errors
  | UnrecognizedToken =>
    sprintf("The Grain lexer doesn't recognize this token.")
  | UnclosedString(line) =>
    sprintf("Unclosed string literal, opened on line %d", line)
  | UnclosedBytes(line) =>
    sprintf("Unclosed byte literal, opened on line %d", line)
  | UnclosedChar(line) =>
    sprintf("Unclosed character literal, opened on line %d", line)
  | UnclosedBlockComment(line) =>
    sprintf("Unclosed block comment, opened on line %d", line)
  | UnclosedDocComment(line) =>
    sprintf("Unclosed doc comment, opened on line %d", line)
  | FloatWithoutLeadingZero(f) =>
    sprintf("Floats must contain a leading zero. Use 0%s instead.", f)
  // Parsing errors
  | BadEncoding => "Grain programs must be UTF-8 encoded."
  | SyntaxError(msg) => sprintf("Syntax error: %s", msg)
  | NoInlineMutableRecordField => "An inline record constructor cannot have mutable fields."
  | NoExceptionMutableRecordField => "A record exception constructor cannot have mutable fields."
  | RecordSpreadNotLeading => "A record spread can only appear at the beginning of a record expression."
  | MultipleRecordSpreads => "A record expression may only contain one record spread."
  | RecordSpreadInConstructor => "A record spread cannot appear in an inline record constructor expression."
  | DefaultArgumentNotNamed => "Default arguments must be named."
  | ListSpreadNotTrailing => "A list spread can only appear at the end of a list."
  | InvalidModuleInclude => "A module include name cannot contain `.` as that would reference a binding within another module."
  | InvalidModuleAlias => "A module alias cannot contain `.` as that would reference a binding within another module."
  // Well-formedness errors
  | MalformedString => "Malformed string literal"
  | EmptyRecordPattern => "A record pattern must contain at least one named field."
  | RHSLetRecMayOnlyBeFunction => "let rec may only be used with recursive function definitions."
  | NoLetRecMut => "let rec may not be used with the `mut` keyword."
  | RationalZeroDenominator => "Rational numbers may not have a denominator of zero."
  | UnknownAttribute(attr_context, attr) =>
    sprintf("Unknown %s attribute `%s`.", attr_context, attr)
  | InvalidAttributeArity(attr, arity) =>
    switch (arity) {
    | 0 => sprintf("Attribute `%s` expects no arguments.", attr)
    | 1 => sprintf("Attribute `%s` expects one argument.", attr)
    | _ => sprintf("Attribute `%s` expects %d arguments.", attr, arity)
    }
  // TODO: Consider moving message generation into this file.
  | AttributeDisallowed(msg) => msg
  | LoopControlOutsideLoop(control) =>
    sprintf("`%s` statement used outside of a loop.", control)
  | ReturnStatementOutsideFunction => "`return` statement used outside of a function."
  | MismatchedReturnStyles => "All returned values must use the `return` keyword if the function returns early."
  | LocalIncludeStatement => "`include` statements may only appear at the file level."
  | ProvidedMultipleTimes(name) =>
    sprintf(
      "%s was provided multiple times, but can only be provided once.",
      name,
    )
  | MutualRecTypesMissingRec => "Mutually recursive type groups must include `rec` on the first type in the group."
  | MutualRecExtraneousNonfirstRec => "The `rec` keyword should only appear on the first type in the mutually recursive type group."
  // Typechecker errors
  | UnableToParseBigIntLiteral(n) =>
    sprintf("Unable to parse big-integer literal %st.", n)
  | NumberLiteralOutOfFloatRange(n) =>
    sprintf(
      "Number literal %s is outside of the floating-point range of the Number type.",
      n,
    )
  | NumberLiteralOutOfRationalRange(value, denominator) =>
    sprintf(
      "Rational literal %s/%s is outside of the rational number range of the Number type.",
      value,
      denominator,
    )
  | IntLiteralOutOfRange(num_bits, s) =>
    sprintf(
      "Int%s literal %s exceeds the range of representable %s-bit signed integers.",
      num_bits,
      s,
      num_bits,
    )
  | UintLiteralOutOfRange(num_bits, s) =>
    sprintf(
      "Uint%s literal %s exceeds the range of representable %s-bit unsigned integers.",
      num_bits,
      s,
      num_bits,
    )
  | UintLiteralWithSign(num_bits, s, Some(hex)) =>
    sprintf(
      "Uint%s literal %s contains a sign but should be unsigned; consider using 0x%s%s instead.",
      num_bits,
      s,
      hex,
      num_bits,
    )
  | UintLiteralWithSign(num_bits, s, None) =>
    sprintf(
      "Uint%s literal %s contains a sign but should be unsigned.",
      num_bits,
      s,
    )
  | WasmLiteralOutOfRange(prefix, bits, s) =>
    sprintf(
      "Wasm%s%s literal %s exceeds the range of representable %s-bit integers.",
      prefix,
      bits,
      s,
      bits,
    )
  | FloatLiteralOutOfRange(bits, s) =>
    sprintf(
      "Float%s literal %s exceeds the range of representable %s-bit floats.",
      bits,
      s,
      bits,
    )
  | RationalLiteralOutOfRange(s) =>
    sprintf(
      "Rational literal %s is outside of the rational number range of the Number type.",
      s,
    )
  | InvalidBytesLiteral(s) => s
  | InvalidStringLiteral(s) => s
  | InvalidCharLiteral(s) => s
  | No_module_file(name, None) =>
    sprintf("Missing file for module \"%s\"", name)
  | No_module_file(name, Some(msg)) =>
    sprintf("Missing file for module \"%s\": %s", name, msg);

/** Determines if a given error/warning is active and should be reported. */
let is_active =
  fun
  // Warnings
  | Message.NotPrincipal(_)
  | Message.NameOutOfScope(_, _, _)
  | Message.FragileMatch(_) => false
  | _ => true;

/** A list of active errors. */
let active_errors: ref(list((Location.t, Message.t))) = ref([]);
/** Resets the internal state of the error system. */
let reset = () => {
  active_errors := [];
};
/** Gets the list of all reported errors. */
let get = () => active_errors^;
/** Reports a error. */
let add = (loc: Location.t, err: Message.t) => {
  active_errors := [(loc, err), ...active_errors^];
};

/** Immediately reports and emits an error. */
let print = (loc: Location.t, err: Message.t) =>
  if (is_active(err)) {
    add(loc, err);
    if (Config.print_warnings^) {
      // TODO: Why does ^ point to the start of the line?
      emit(
        ~loc=Asai.Range.of_lex_range((loc.loc_start, loc.loc_end)),
        err,
        get_message(err),
      );
    };
  };

/** Immediatly reports and emits a fatal error. */
let fatal = (loc: Location.t, err: Message.t) => {
  add(loc, err);
  fatal(
    ~loc=Asai.Range.of_lex_range((loc.loc_start, loc.loc_end)),
    err,
    get_message(err),
  );
};
