
open Compile_common


(* Parse tree cache *)
let impl_cache = Hashtbl.create 128

let parse_impl ~tool_name ~preprocessor ~all_ppx sourcefile =
  try Hashtbl.find impl_cache sourcefile
  with Not_found ->
    let ast =
      Pparse.parse_implementation ~tool_name ~preprocessor ~all_ppx sourcefile
    in
    Hashtbl.add impl_cache sourcefile ast;
    ast

let intf_cache = Hashtbl.create 128

let parse_intf ~tool_name ~preprocessor ~all_ppx sourcefile =
  try Hashtbl.find intf_cache sourcefile
  with Not_found ->
    let ast =
      Pparse.parse_interface ~tool_name ~preprocessor ~all_ppx sourcefile
    in
    Hashtbl.add intf_cache sourcefile ast;
    ast

(* Type information cache *)
let impl_cache = Hashtbl.create 128

let typecheck_impl info ast =
  try
    let typed = Hashtbl.find impl_cache info.sourcefile in
    Hashtbl.remove impl_cache info.sourcefile;
    typed
  with Not_found ->
    let ts, co, sg = Typemod.type_implementation
        info.sourcefile
        info.modulename
        info.env ast
    in
    let tst = (ts, co, sg, Env.imports ()) in
    Hashtbl.add impl_cache info.sourcefile tst;
    tst


(* Cmi cache *)
let cmi_cache = Hashtbl.create 128

let read_signature ~filename =
  try
    Hashtbl.find cmi_cache filename
  with Not_found ->
    let cmi = Cmi_format.read_cmi filename in
    Hashtbl.add cmi_cache filename cmi;
    cmi

let output_cmi_hook filename cmi =
  Hashtbl.replace cmi_cache filename cmi

let () =
  Env.Persistent_signature.read := read_signature;
  Env.output_cmi_hook := output_cmi_hook


(* Cmx cache *)
let cmx_cache = Hashtbl.create 128

let () =
  let old_read_unit_info = !Compilenv.read_unit_info in
  Compilenv.read_unit_info := (fun filename ->
    try Hashtbl.find cmx_cache filename
    with Not_found ->
     let cmx = old_read_unit_info filename in
     Hashtbl.add cmx_cache filename cmx;
     cmx)
