
open Compile_common


let cache = Hashtbl.create 128

let get bucket key default =
  try
    let value = Hashtbl.find cache (bucket, key) in
    if key = "type-impl" then Hashtbl.remove cache (bucket, key);
    Obj.obj value
  with Not_found ->
    let value = default () in
    Hashtbl.add cache (bucket, key) (Obj.repr value);
    value

let set bucket key value =
  Hashtbl.replace cache (bucket, key) (Obj.repr value)

(* Parse tree cache *)
let parse_impl ~tool_name ~preprocessor ~all_ppx sourcefile =
  get "ast-impl" sourcefile (fun () ->
    Pparse.parse_implementation ~tool_name ~preprocessor ~all_ppx sourcefile)

let parse_intf ~tool_name ~preprocessor ~all_ppx sourcefile =
  get "ast-intf" sourcefile (fun () ->
    Pparse.parse_interface ~tool_name ~preprocessor ~all_ppx sourcefile)

(* Type information cache *)
let typecheck_impl info ast =
  get "type-impl" info.sourcefile (fun () ->
    let structure, coercion, signature = Typemod.type_implementation
        info.sourcefile
        info.modulename
        info.env ast
    in
    { structure; coercion; signature; imports = Env.imports () })

(* Cmi cache *)
let read_signature ~filename =
  get "cmi" filename (fun () -> Cmi_format.read_cmi filename)

let output_cmi_hook filename cmi =
  set "cmi" filename cmi

(* Cmx cache *)
let () =
  let old_read_unit_info = !Compilenv.read_unit_info in
  Compilenv.read_unit_info := (fun filename ->
    get "cmx" filename (fun () -> old_read_unit_info filename))

let () =
  Env.Persistent_signature.read := read_signature;
  Env.output_cmi_hook := output_cmi_hook
