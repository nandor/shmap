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

let tool_name = "ocamldep"

(* Fix path to use '/' as directory separator instead of '\'. Under Windows. *)
let fix_slash s =
  if Sys.os_type = "Unix" then s else begin
    String.map (function '\\' -> '/' | c -> c) s
  end

let add_to_list li s =
  li := s :: !li

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

let file_deps_to_string (source_file, extracted_deps, _) =
  let is_predef dep = (String.length dep > 0) && (match dep.[0] with
    | 'A'..'Z' | '\128'..'\255' -> true
    | _ -> false)
  in
  let defs = String.Set.elements extracted_deps |> List.filter is_predef in
  (filename_to_string source_file) ^ ": " ^ String.concat " " defs

let read_parse_and_extract (type a)
  (extract_function: Depend.bound_map -> a -> unit)
  (ast_kind: a Pparse.ast_kind)
  source_file =

  Depend.pp_deps := [];
  Depend.free_structure_names := String.Set.empty;
  let bound_vars =
    List.fold_left
      (fun bv modname ->
        Depend.open_module bv (Longident.parse modname))
     String.Map.empty ((* PR#7248 *) List.rev !Clflags.open_modules)
  in
  let preprocessor = !Clflags.preprocessor in
  let all_ppx = !Clflags.all_ppx in
  let ast: a = match ast_kind with
  | Pparse.Structure ->
    Cache.parse_impl ~tool_name ~preprocessor ~all_ppx source_file
  | Pparse.Signature ->
    Cache.parse_intf ~tool_name ~preprocessor ~all_ppx source_file
  in
  extract_function bound_vars ast;
  !Depend.free_structure_names

let ml_file_dependencies source_file =
  let extracted_deps = read_parse_and_extract
    Depend.add_implementation
    Pparse.Structure
    source_file
  in
  [(source_file, extracted_deps, !Depend.pp_deps)]

let mli_file_dependencies source_file =
  let extracted_deps = read_parse_and_extract
    Depend.add_signature
    Pparse.Signature
    source_file
  in
  [(source_file, extracted_deps, !Depend.pp_deps)]

let process_file_as ppf process_fun source_file =
  Compenv.readenv ppf (Before_compile source_file);
  Location.input_name := source_file;
  if Sys.file_exists source_file then process_fun source_file else []

let run argv env cwd =
  let ppf = Format.str_formatter in
  Location.formatter_for_warnings := ppf;
  try
    Sys.chdir cwd;

    Clflags.absname := false;
    Clflags.classic := false;
    Clflags.transparent_modules := false;
    Clflags.include_dirs := [];
    Clflags.open_modules := [];
    Clflags.force_slash := false;
    Profile.reset ();

    add_to_list first_include_dirs Filename.current_dir_name;

    Compenv.readenv ppf Before_args;

    let files = match Array.to_list argv with
      | _ :: "-modules" :: "-impl" :: file :: [] ->
        process_file_as ppf ml_file_dependencies file
      | _ :: "-modules" :: "-intf" :: file :: [] ->
        process_file_as ppf mli_file_dependencies file
      | _ ->
        []
    in

    Compenv.readenv ppf Before_link;

    let deps = files
      |> List.sort compare
      |> List.map file_deps_to_string
      |> String.concat "\n"
    in
    (Unix.WEXITED(0), deps, Format.flush_str_formatter ())
  with exc ->
    Location.report_exception ppf exc;
    (Unix.WEXITED(2), "", Format.flush_str_formatter ())
