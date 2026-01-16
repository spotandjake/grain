open Lexing;
open Sexplib.Conv;

/** Whether S-expression locations are disabled. */
let sexp_locs_disabled = _ => ! Config.sexp_locs_enabled^;

// Position conversions
let sexp_of_position = (p: position) =>
  Sexplib.Sexp.List([
    Sexplib.Sexp.Atom("position"),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("file"),
      Sexplib.Conv.sexp_of_string(p.pos_fname),
    ]),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("line"),
      Sexplib.Conv.sexp_of_int(p.pos_lnum),
    ]),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("col"),
      Sexplib.Conv.sexp_of_int(p.pos_cnum),
    ]),
    Sexplib.Sexp.List([
      Sexplib.Sexp.Atom("bol"),
      Sexplib.Conv.sexp_of_int(p.pos_bol),
    ]),
  ]);

let position_to_yojson = (p: position): Yojson.Safe.t =>
  `Assoc([
    ("file", `String(p.pos_fname)),
    ("line", `Int(p.pos_lnum)),
    ("col", `Int(p.pos_cnum)),
    ("bol", `Int(p.pos_bol)),
  ]);

let position_of_sexp = (sexp: Sexplib.Sexp.t) =>
  Sexplib.Conv.(
    Sexplib.Sexp.(
      switch (sexp) {
      | Atom(str) => of_sexp_error("position_of_sexp: list needed", sexp)
      | List([Atom("position"), ...sexp_fields])
          when List.length(sexp_fields) == 4 =>
        let fields =
          List.map(
            fun
            | List([Atom(str), Atom(_) as value]) => (str, value)
            | sexp => of_sexp_error("position_of_sexp: invalid field", sexp),
            sexp_fields,
          );
        let (pos_fname, pos_lnum, pos_cnum, pos_bol) =
          try((
            string_of_sexp(List.assoc("file", fields)),
            int_of_sexp(List.assoc("line", fields)),
            int_of_sexp(List.assoc("col", fields)),
            int_of_sexp(List.assoc("bol", fields)),
          )) {
          | Not_found =>
            of_sexp_error("position_of_sexp: invalid fields", sexp)
          };
        {
          pos_fname,
          pos_lnum,
          pos_cnum,
          pos_bol,
        };
      | List([Atom("position"), ..._]) =>
        of_sexp_error("position_of_sexp: invalid fields", sexp)
      | List(_) =>
        of_sexp_error("position_of_sexp: invalid s-expression", sexp)
      }
    )
  );

let position_of_yojson = (yj: Yojson.Safe.t): result(position, string) =>
  switch (yj) {
  | `Assoc(contents) =>
    let map = Hashtbl.create(4);
    List.iter(((key, value)) => Hashtbl.add(map, key, value), contents);
    if (!List.for_all(Hashtbl.mem(map), ["file", "line", "col", "bol"])) {
      Result.Error(
        "position_of_yojson: invalid json object: "
        ++ Yojson.Safe.to_string(yj),
      );
    } else {
      let (file, line, col, bol) =
        switch (List.map(Hashtbl.find(map), ["file", "line", "col", "bol"])) {
        | [a, b, c, d] => (a, b, c, d)
        | _ => failwith("impossible")
        };

      let res_map = f => (
        fun
        | Result.Ok(x) => f(x)
        | Result.Error(y) => Result.Error(y)
      );

      switch (file) {
      | `String(pos_fname) =>
        switch (
          List.fold_right(
            ((cur_name, cur), acc) =>
              res_map(
                acc_list =>
                  switch (cur) {
                  | `Int(x) => Result.Ok([x, ...acc_list])
                  | `Intlit(x) => Result.Ok([int_of_string(x), ...acc_list])
                  | _ =>
                    Result.Error(
                      "position_of_yojson '" ++ cur_name ++ "' is not an int",
                    )
                  },
                acc,
              ),
            [("line", line), ("col", col), ("bol", bol)],
            Result.Ok([]),
          )
        ) {
        | Result.Ok([pos_lnum, pos_cnum, pos_bol]) =>
          Result.Ok({
            pos_fname,
            pos_lnum,
            pos_cnum,
            pos_bol,
          })
        | Result.Ok(_) => failwith("position_of_yojson: impossible")
        | Result.Error(x) => Result.Error(x)
        }
      | _ => Result.Error("position_of_yojson: 'file' is not a string")
      };
    };
  | _ =>
    Result.Error(
      "position_of_yojson: invalid json object: " ++ Yojson.Safe.to_string(yj),
    )
  };

// Utilities for working with source locations

/** Defines a source location range. */
[@deriving (sexp, yojson)]
type t = {
  loc_start: position,
  loc_end: position,
  loc_ghost: bool,
};

/** A placeholder dummy location */
let dummy_loc = {
  loc_start: dummy_pos,
  loc_end: dummy_pos,
  loc_ghost: true,
};

let sexp_of_t = loc =>
  if (loc == dummy_loc) {
    Sexplib.Sexp.Atom("dummy_loc");
  } else {
    sexp_of_t(loc);
  };

let t_of_sexp = sexp =>
  switch (sexp) {
  | Sexplib.Sexp.Atom("dummy_loc") => dummy_loc
  | _ => t_of_sexp(sexp)
  };

/** The first position of any program */
let start_pos = name => {
  pos_fname: name,
  pos_lnum: 1,
  pos_cnum: 0,
  pos_bol: 0,
};

/** Returns an empty ghost range located in a given file. */
let in_file = name => {
  let loc = {
    pos_fname: name,
    pos_lnum: 1,
    pos_bol: 0,
    pos_cnum: (-1),
  };
  {
    loc_start: loc,
    loc_end: loc,
    loc_ghost: true,
  };
};

/** Converts a range of positions to a location. */
let to_loc = ((loc_start, loc_end)) => {
  {
    loc_start,
    loc_end,
    loc_ghost: false,
  };
};

/** Extracts (file, line, char) */
let get_pos_info = pos => (
  pos.pos_fname,
  pos.pos_lnum,
  pos.pos_cnum - pos.pos_bol,
);

// Utilities for working with located values
/** A location-tagged value. */
[@deriving (sexp, yojson)]
type loc('a) = {
  /** The value itself. */
  value: 'a,
  /** The location of the value. */
  [@sexp_drop_if sexp_locs_disabled]
  loc: t,
};

/** Makes a location-tagged value with the given location. */
let mkloc = (value, loc) => {
  value,
  loc,
};

/** Makes a location-tagged value with the dummy location. */
let mknoloc = value => mkloc(value, dummy_loc);
