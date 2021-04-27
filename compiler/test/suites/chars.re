open TestFramework;
open Runner;

describe("chars", ({test}) => {
  let assertSnapshot = makeSnapshotRunner(test);
  let assertCompileError = makeCompileErrorRunner(test);
  let assertRun = makeRunner(test);

  assertRun("char1", "print('A')", "A\n");
  assertSnapshot("char2", "'\\x41'");
  assertSnapshot("char3", "'\\101'");
  assertSnapshot("char4", "'\\u0041'");
  assertSnapshot("char5", "'\\u{41}'");
  assertSnapshot("char6", "'💯'");
  assertSnapshot("char7", "'\\u{1F33E}'");
  assertRun("char_eq1", "print('🌾' == '🌾')", "true\n");
  assertRun("char_eq2", "print('🌾' == '💯')", "false\n");
  assertRun(
    "char_eq3",
    "import Char from \"char\"; print(Char.fromCode(0x1F33E) == '🌾')",
    "true\n",
  );
  assertRun(
    "char_eq4",
    "import Char from \"char\"; print(Char.fromCode(0x1F33E) == '💯')",
    "false\n",
  );
  assertCompileError(
    "char_illegal",
    "'abc'",
    "This character literal contains multiple characters: 'abc'\nDid you mean to create the string \"abc\" instead?",
  );
  assertCompileError(
    "unicode_err1",
    "let x = '\\u{d800}'",
    "Illegal unicode code point",
  );
  assertCompileError(
    "unicode_err2",
    "let x = '\\u{dfff}'",
    "Illegal unicode code point",
  );
  assertCompileError(
    "unicode_err3",
    "let x = '\\u{110000}'",
    "Illegal unicode code point",
  );
});
