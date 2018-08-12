(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1999 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Compenv
open Parsetree
module String = Misc.Stdlib.String

let ppf = Format.err_formatter
(* Print the dependencies *)

type file_kind = ML | MLI;;

let load_path = ref ([] : (string * string array) list)
let files = ref ([] : (string * file_kind * String.Set.t * string list) list)

(* Fix path to use '/' as directory separator instead of '\'.
   Only under Windows. *)

let fix_slash s =
  if Sys.os_type = "Unix" then s else begin
    String.map (function '\\' -> '/' | c -> c) s
  end

(* Since we reinitialize load_path after reading OCAMLCOMP,
  we must use a cache instead of calling Sys.readdir too often. *)
let dirs = ref String.Map.empty
let readdir dir =
  try
    String.Map.find dir !dirs
  with Not_found ->
    let contents = Sys.readdir dir in
    dirs := String.Map.add dir contents !dirs;
    contents

let add_to_list li s =
  li := s :: !li

let add_to_load_path dir =
  let dir = Misc.expand_directory Config.standard_library dir in
  let contents = readdir dir in
  add_to_list load_path (dir, contents)

let (depends_on, escaped_eol) = (":", " \\\n    ")

let filename_to_string s =
  let s = if !Clflags.force_slash then fix_slash s else s in
  if not (String.contains s ' ') then begin
    s
  end else begin
    let rec count n i =
      if i >= String.length s then n
      else if s.[i] = ' ' then count (n+1) (i+1)
      else count n (i+1)
    in
    let spaces = count 0 0 in
    let result = Bytes.create (String.length s + spaces) in
    let rec loop i j =
      if i >= String.length s then ()
      else if s.[i] = ' ' then begin
        Bytes.set result j '\\';
        Bytes.set result (j+1) ' ';
        loop (i+1) (j+2);
      end else begin
        Bytes.set result j s.[i];
        loop (i+1) (j+1);
      end
    in
    loop 0 0;
    Bytes.unsafe_to_string result
  end
;;

let raw_deps_to_string source_file deps =
  let is_predef dep = (String.length dep > 0) && (match dep.[0] with
    | 'A'..'Z' | '\128'..'\255' -> true
    | _ -> false)
  in
  let defs = String.Set.elements deps |> List.filter is_predef in
  (filename_to_string source_file) ^ depends_on ^ " " ^ String.concat " " defs


(* Process one file *)
let tool_name = "ocamldep"

let read_parse_and_extract parse_function extract_function def ast_kind
    source_file =
  Depend.pp_deps := [];
  Depend.free_structure_names := String.Set.empty;
  let input_file = Pparse.preprocess source_file in
  begin try
    let ast = Pparse.file ~tool_name input_file parse_function ast_kind in
    let bound_vars =
      List.fold_left
        (fun bv modname ->
          Depend.open_module bv (Longident.parse modname))
       String.Map.empty ((* PR#7248 *) List.rev !Clflags.open_modules)
    in
    let r = extract_function bound_vars ast in
    Pparse.remove_preprocessed input_file;
    (!Depend.free_structure_names, r)
  with x ->
    Pparse.remove_preprocessed input_file;
    raise x
  end

let file_deps_to_string (source_file, kind, extracted_deps, pp_deps) =
  raw_deps_to_string source_file extracted_deps

let ml_file_dependencies source_file =
  let parse_use_file_as_impl lexbuf =
    let f x =
      match x with
      | Ptop_def s -> s
      | Ptop_dir _ -> []
    in
    List.flatten (List.map f (Parse.use_file lexbuf))
  in
  let (extracted_deps, ()) =
    read_parse_and_extract parse_use_file_as_impl Depend.add_implementation ()
                           Pparse.Structure source_file
  in
  files := (source_file, ML, extracted_deps, !Depend.pp_deps) :: !files

let mli_file_dependencies source_file =
  let (extracted_deps, ()) =
    read_parse_and_extract Parse.interface Depend.add_signature ()
                           Pparse.Signature source_file
  in
  files := (source_file, MLI, extracted_deps, !Depend.pp_deps) :: !files

let process_file_as process_fun def source_file =
  Compenv.readenv ppf (Before_compile source_file);
  load_path := [];
  List.iter add_to_load_path (
      (!Compenv.last_include_dirs @
       !Clflags.include_dirs @
       !Compenv.first_include_dirs
      ));
  Location.input_name := source_file;
  if Sys.file_exists source_file then process_fun source_file else def

let process_file source_file ~ml_file ~mli_file ~def =
  if Filename.check_suffix source_file ".ml" then
    process_file_as ml_file def source_file
  else if Filename.check_suffix source_file ".mli" then
    process_file_as mli_file def source_file
  else
    def

let file_dependencies source_file =
  process_file source_file ~def:()
    ~ml_file:ml_file_dependencies
    ~mli_file:mli_file_dependencies

let file_dependencies_as kind =
  match kind with
  | ML -> process_file_as ml_file_dependencies ()
  | MLI -> process_file_as mli_file_dependencies ()

let run argv env cwd =
  try
    Sys.chdir cwd;

    Clflags.absname := false;
    Clflags.classic := false;
    Clflags.transparent_modules := false;
    Clflags.include_dirs := [];
    Clflags.open_modules := [];
    Clflags.force_slash := false;
    Clflags.preprocessor := None;
    load_path := [];
    files := [];

    add_to_list first_include_dirs Filename.current_dir_name;

    Compenv.readenv ppf Before_args;

    Clflags.reset_arguments (); (* reset arguments from ocamlc/ocamlopt *)

    Clflags.add_arguments __LOC__ [
       "-modules", Arg.Unit (fun () -> ()),
        " Print module dependencies in raw form (not suitable for make)";
       "-impl", Arg.String (file_dependencies_as ML),
          "<f>  Process <f> as a .ml file";
       "-intf", Arg.String (file_dependencies_as MLI),
          "<f>  Process <f> as a .mli file";
     ];

    Clflags.parse_arguments argv file_dependencies "";

    Compenv.readenv ppf Before_link;

    let sorted_files = List.sort compare !files in
    let deps = String.concat "\n" (List.map file_deps_to_string sorted_files) in
    (Unix.WEXITED(0), deps, "")
  with exc ->
    (* TODO: print exceptions *)
    Array.iter print_endline argv;
    Printexc.print_backtrace stdout;
    Location.report_exception ppf exc;
    (Unix.WEXITED(2), "", "ocamldep error")
