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
  [(source_file, extracted_deps, !Depend.pp_deps)]

let mli_file_dependencies source_file =
  let (extracted_deps, ()) =
    read_parse_and_extract Parse.interface Depend.add_signature ()
                           Pparse.Signature source_file
  in
  [(source_file, extracted_deps, !Depend.pp_deps)]

let process_file_as process_fun source_file =
  Compenv.readenv ppf (Before_compile source_file);
  Location.input_name := source_file;
  if Sys.file_exists source_file then process_fun source_file else []

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

    add_to_list first_include_dirs Filename.current_dir_name;

    Compenv.readenv ppf Before_args;

    let files = match Array.to_list argv with
      | _ :: "-modules" :: "-impl" :: file :: [] ->
        process_file_as ml_file_dependencies file
      | _ :: "-modules" :: "-intf" :: file :: [] ->
        process_file_as mli_file_dependencies file
      | _ ->
        []
    in

    Compenv.readenv ppf Before_link;

    let deps = files
      |> List.sort compare
      |> List.map file_deps_to_string
      |> String.concat "\n"
    in
    (Unix.WEXITED(0), deps, "")
  with exc ->
    (* TODO: print exceptions *)
    Array.iter print_endline argv;
    Printexc.print_backtrace stdout;
    Location.report_exception ppf exc;
    (Unix.WEXITED(2), "", "ocamldep error")
