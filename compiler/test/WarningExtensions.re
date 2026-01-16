open TestFramework;
open Rely.MatcherTypes;
open Grain_utils;

type warningExtensions = {
  toHaveTriggered: Comp_errors.Message.t => unit,
  toHaveTriggeredNoWarnings: unit => unit,
};

let warningExtensions = ({createMatcher}) => {
  let pass = (() => "", true);
  let createTriggeredMatcher =
    createMatcher(
      ({formatReceived, formatExpected}, actualThunk, expectedThunk) => {
      let expected = expectedThunk();
      let generatedWarnings = Comp_errors.get();
      let warningTriggered =
        List.exists(((_, warn)) => warn == expected, generatedWarnings);

      if (!warningTriggered) {
        let receivedWarnings =
          List.length(generatedWarnings) == 0
            ? ["No warnings."]
            : List.map(
                ((_, warn)) => Comp_errors.get_message(warn),
                generatedWarnings,
              );
        let failureMessage =
          String.concat(
            "\n",
            [
              "Expected warning:",
              Comp_errors.get_message(expected),
              "\nReceived:",
              ...receivedWarnings,
            ],
          );
        (() => failureMessage, false);
      } else {
        pass;
      };
    });
  let createNotTriggeredMatcher =
    createMatcher(
      ({formatReceived, formatExpected}, actualThunk, expectedThunk) => {
      let generatedWarnings = Comp_errors.get();
      let warningTriggered = List.length(generatedWarnings) > 0;

      if (warningTriggered) {
        let receivedWarnings =
          List.map(
            ((_, warn)) => Comp_errors.get_message(warn),
            generatedWarnings,
          );
        let failureMessage =
          String.concat(
            "\n",
            ["Expected no warnings, received:", ...receivedWarnings],
          );
        (() => failureMessage, false);
      } else {
        pass;
      };
    });
  {
    toHaveTriggered: warn => createTriggeredMatcher(() => (), () => warn),
    toHaveTriggeredNoWarnings: () =>
      createNotTriggeredMatcher(() => (), () => ()),
  };
};
