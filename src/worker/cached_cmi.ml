
let cmi_cache = Hashtbl.create 128

let to_sig filename cmi =
  Env.Persistent_signature.(Some { filename; cmi })

let load_signature ~unit_name =
  match Misc.find_in_path_uncap !Config.load_path (unit_name ^ ".cmi") with
  | filename ->
    begin try
      Hashtbl.find cmi_cache filename
    with Not_found ->
      let cmi = to_sig filename (Cmi_format.read_cmi filename) in
      Hashtbl.add cmi_cache filename cmi;
      cmi
    end
  | exception Not_found ->
    None

let output_cmi_hook filename cmi =
  Hashtbl.add cmi_cache filename (to_sig filename cmi)

let init () =
  Env.Persistent_signature.load := load_signature;
  Env.output_cmi_hook := output_cmi_hook
