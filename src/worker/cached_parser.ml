


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
