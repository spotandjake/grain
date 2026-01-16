open Lexing;

/** Whether S-expression locations are disabled. */
let sexp_locs_disabled: _ => bool;

// Utilities for working with source locations

/** Defines a source location range. */
[@deriving (sexp, yojson)]
type t = {
  loc_start: position,
  loc_end: position,
  loc_ghost: bool,
};

/** A placeholder dummy location */
let dummy_loc: t;

/** The first position of any program */
let start_pos: string => position;

/** Returns an empty ghost range located in a given file. */
let in_file: string => t;

/** Converts a range of positions to a location. */
let to_loc: ((position, position)) => t;

/** Extracts (file, line, char) */
let get_pos_info: position => (string, int, int);

// Utilities for working with located values
/** A location-tagged value. */
[@deriving (sexp, yojson)]
type loc('a) = {
  /** The value itself. */
  value: 'a,
  /** The location of the value. */
  loc: t,
};

/** Makes a location-tagged value with the given location. */
let mkloc: ('a, t) => loc('a);

/** Makes a location-tagged value with the dummy location. */
let mknoloc: 'a => loc('a);
