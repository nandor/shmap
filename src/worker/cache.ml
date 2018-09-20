
open Compile_common

let key = 1234
let addr = 1 lsl 40
let keys = 16 * 1024
let size = 4 * 1024   (* 4Gb *)

let cache = Shmap.create ~key ~addr ~keys ~size

let make_key bucket key =
  let digest = Digest.string (bucket ^ key) in
  let rec loop i acc = match i with
    | 0 -> acc
    | x -> loop (x - 1) ((acc lsl 8) + (Char.code (digest.[i])))
  in loop 8 0

let get bucket key default =
  try
    Obj.obj (Shmap.get cache (make_key bucket key))
  with Not_found ->
    let value = default () in
    Shmap.set cache (make_key bucket key) (Obj.repr value);
    value

let set bucket key value =
  Shmap.set cache (make_key bucket key) (Obj.repr value)

(* Parse tree cache *)
let parse_impl ~tool_name ~preprocessor ~all_ppx sourcefile =
  get "ast-impl" sourcefile (fun () ->
    Pparse.parse_implementation ~tool_name ~preprocessor ~all_ppx sourcefile
  )

let parse_intf ~tool_name ~preprocessor ~all_ppx sourcefile =
  get "ast-intf" sourcefile (fun () ->
    Pparse.parse_interface ~tool_name ~preprocessor ~all_ppx sourcefile
  )

(* Type information cache *)
let typecheck_impl info ast =
  get "type-impl" info.sourcefile (fun () ->
    let structure, coercion, signature = Typemod.type_implementation
        info.sourcefile
        info.modulename
        info.env ast
    in
    { structure; coercion; signature; imports = Env.imports () }
  )

(* Cmx cache *)
let () =
  let old_read_unit_info = !Compilenv.read_unit_info in
  Compilenv.read_unit_info := (fun filename ->
    get "cmx" filename (fun () -> old_read_unit_info filename))

(* Cmi cache *)
let read_signature ~filename =
  get "cmi" filename (fun () -> Cmi_format.read_cmi filename)

let output_cmi_hook filename cmi =
  set "cmi" filename cmi

let () =
  Env.Persistent_signature.read := read_signature;
  Env.output_cmi_hook := output_cmi_hook
