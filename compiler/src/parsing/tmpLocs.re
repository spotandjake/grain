/* This file is mostly copied from OCaml's parsing/location.ml.
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

open Sexplib.Conv;
open Lexing;
open Grain_utils;
open Grain_utils.Location;
/* NOTE: A lot of this file is taken from OCaml's parsing/location.ml.
   Perhaps we should just go ahead and copy the whole thing. */

let input_name = ref("_none_");

/* Terminal info */

let num_loc_lines = ref(0); /* number of lines already printed after input */

let print_updating_num_loc_lines = (ppf, f, arg) => {
  open Format;
  let out_functions = pp_get_formatter_out_functions(ppf, ());
  let out_string = (str, start, len) => {
    let rec count = (i, c) =>
      if (i == start + len) {
        c;
      } else if (str.[i] == '\n') {
        count(succ(i), succ(c));
      } else {
        count(succ(i), c);
      };
    num_loc_lines := num_loc_lines^ + count(start, 0);
    out_functions.out_string(str, start, len);
  };
  pp_set_formatter_out_functions(
    ppf,
    {
      ...out_functions,
      out_string,
    },
  );
  f(ppf, arg);
  pp_print_flush(ppf, ());
  pp_set_formatter_out_functions(ppf, out_functions);
};

open Format;

let (msg_file, msg_line, msg_chars, msg_char, msg_to, msg_colon) = (
  "File \"",
  "\", line ",
  ", characters ",
  ", character ",
  "-",
  ":",
);

let setup_colors = () =>
  Misc.Style.setup @@
  Some(
    if (Grain_utils.Config.color_enabled^) {
      Misc.Color.Auto;
    } else {
      Misc.Color.Never;
    },
  );

let print_filename = (ppf, file) => fprintf(ppf, "%s", file);

let print_loc = (ppf, loc) => {
  setup_colors();
  let (file, line, startchar) = get_pos_info(loc.loc_start);
  let (_, endline, endchar) = get_pos_info(loc.loc_end);
  /*let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in*/
  fprintf(
    ppf,
    "%s@{<loc>%a%s%i",
    msg_file,
    print_filename,
    file,
    msg_line,
    line,
  );
  if (startchar >= 0) {
    if (line == endline) {
      fprintf(ppf, "%s%i%s%i", msg_chars, startchar, msg_to, endchar);
    } else {
      fprintf(
        ppf,
        "%s%i%sline %i%s%i",
        msg_char,
        startchar,
        msg_to,
        endline,
        msg_char,
        endchar,
      );
    };
  };
  fprintf(ppf, "@}");
};

let default_printer = (ppf, loc) => {
  setup_colors();
  fprintf(ppf, "@{<loc>%a@}%s@,", print_loc, loc, msg_colon);
};

let printer = ref(default_printer);
let print = (ppf, loc) => printer^(ppf, loc);

let error_prefix = "Error";

let print_error_prefix = ppf => {
  setup_colors();
  fprintf(ppf, "@{<error>%s@}", error_prefix);
};

let print_compact = (ppf, loc) => {
  let (file, line, startchar) = get_pos_info(loc.loc_start);
  let (_, endline, endchar) = get_pos_info(loc.loc_end);
  fprintf(ppf, "%a:%i", print_filename, file, line);
  if (startchar >= 0) {
    if (line == endline) {
      fprintf(ppf, ",%i--%i", startchar, endchar);
    } else {
      fprintf(ppf, ",%i--%i,%i", startchar, endline, endchar);
    };
  };
};

let print_error = (ppf, loc) =>
  fprintf(ppf, "%a%t:", print, loc, print_error_prefix);

type error = {
  error_loc: Location.t,
  msg: string,
  sub: list(error),
  if_highlight: string /* alternative message if locations are highlighted */
};

let pp_ksprintf = (~before=?, k, fmt) => {
  let buf = Buffer.create(64);
  let ppf = Format.formatter_of_buffer(buf);
  Misc.Style.set_tag_handling(ppf);
  switch (before) {
  | None => ()
  | Some(f) => f(ppf)
  };
  kfprintf(
    _ => {
      pp_print_flush(ppf, ());
      let msg = Buffer.contents(buf);
      k(msg);
    },
    ppf,
    fmt,
  );
};

/* Shift the formatter's offset by the length of the error prefix, which
   is always added by the compiler after the message has been formatted */
let print_phantom_error_prefix = ppf =>
  Format.pp_print_as(ppf, String.length(error_prefix) + 2 /* ": " */, "");

let errorf = (~loc=Location.dummy_loc, ~sub=[], ~if_highlight="", fmt) =>
  pp_ksprintf(
    ~before=print_phantom_error_prefix,
    msg =>
      {
        error_loc: loc,
        msg,
        sub,
        if_highlight,
      },
    fmt,
  );

let error_of_exn: ref(list(exn => option(error))) = (
  ref([]): ref(list(exn => option(error)))
);

let register_error_of_exn = f => error_of_exn := [f, ...error_of_exn^];

exception OldErrors;
exception Already_displayed_error = OldErrors;

let error_of_exn = exn =>
  switch (exn) {
  | Already_displayed_error => Some(`Already_displayed)
  | _ =>
    let rec loop = (
      fun
      | [] => None
      | [f, ...rest] =>
        switch (f(exn)) {
        | Some(error) => Some(`Ok(error))
        | None => loop(rest)
        }
    );

    loop(error_of_exn^);
  };

let rec default_error_reporter = (ppf, {error_loc, msg, sub, if_highlight}) => {
  fprintf(ppf, "@[<v>%a %s", print_error, error_loc, msg);
  List.iter(Format.fprintf(ppf, "@,@[<2>%a@]", default_error_reporter), sub);
  fprintf(ppf, "@]");
};

let error_reporter = ref(default_error_reporter);

let report_error = (ppf, err) =>
  print_updating_num_loc_lines(ppf, error_reporter^, err);

let error_of_printer = (loc, print, x) => errorf(~loc, "%a@?", print, x);

let error_of_printer_file = (print, x) =>
  error_of_printer(Location.in_file(input_name^), print, x);

let () =
  register_error_of_exn(
    fun
    | Sys_error(msg) =>
      Some(errorf(~loc=Location.in_file(input_name^), "I/O error: %s", msg))
    | _ => None,
  );

external reraise: exn => 'a = "%reraise";

let rec report_exception = (ppf, exn) => {
  let rec loop = (n, exn) =>
    switch (error_of_exn(exn)) {
    | None => reraise(exn)
    | Some(`Already_displayed) => ()
    | Some(`Ok(err)) => Format.fprintf(ppf, "@[%a@]@.", report_error, err)
    | exception exn when n > 0 => loop(n - 1, exn)
    };

  loop(10, exn);
};

exception Error(error);

let () =
  register_error_of_exn(
    fun
    | Error(e) => Some(e)
    | _ => None,
  );
