
open Compile_common


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



let cmi_cache = Hashtbl.create 128

let read_signature ~filename =
  try
    Hashtbl.find cmi_cache filename
  with Not_found ->
    let cmi = Cmi_format.read_cmi filename in
    Hashtbl.add cmi_cache filename cmi;
    cmi

let output_cmi_hook filename cmi =
  Hashtbl.add cmi_cache filename cmi

let () =
  Env.Persistent_signature.read := read_signature;
  Env.output_cmi_hook := output_cmi_hook




let typecheck_intf info ast =
  let tsg = ast |> Typemod.type_interface info.sourcefile info.env in
  let sg = tsg.Typedtree.sig_type in
  ignore (Includemod.signatures info.env sg sg);
  Typecore.force_delayed_checks ();
  Warnings.check_fatal ();
  tsg

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

