open Clflags
open Compenv

let usage = "Usage: ocamlc <options> <files>\nOptions are:"

let show_config = ref false
let show_config_var = ref None

module Options = Main_args.Make_bytecomp_options (struct
  let set r () = r := true
  let unset r () = r := false
  let _a = set make_archive
  let _absname = set Clflags.absname
  let _annot = set annotations
  let _binannot = set binary_annotations
  let _c = set compile_only
  let _cc s = c_compiler := Some s
  let _cclib s = Compenv.defer (ProcessObjects (Misc.rev_split_words s))
  let _ccopt s = first_ccopts := s :: !first_ccopts
  let _compat_32 = set bytecode_compatible_32
  let _config = set show_config
  let _config_var s = show_config_var := Some s
  let _custom = set custom_runtime
  let _no_check_prims = set no_check_prims
  let _dllib s = defer (ProcessDLLs (Misc.rev_split_words s))
  let _dllpath s = dllpaths := !dllpaths @ [s]
  let _for_pack s = for_package := Some s
  let _g = set debug
  let _i () = print_types := true; compile_only := true
  let _I s = include_dirs := s :: !include_dirs
  let _impl = impl
  let _intf = intf
  let _intf_suffix s = Config.interface_suffix := s
  let _keep_docs = set keep_docs
  let _no_keep_docs = unset keep_docs
  let _keep_locs = set keep_locs
  let _no_keep_locs = unset keep_locs
  let _labels = unset classic
  let _linkall = set link_everything
  let _make_runtime () =
    custom_runtime := true; make_runtime := true; link_everything := true
  let _alias_deps = unset transparent_modules
  let _no_alias_deps = set transparent_modules
  let _app_funct = set applicative_functors
  let _no_app_funct = unset applicative_functors
  let _noassert = set noassert
  let _nolabels = set classic
  let _noautolink = set no_auto_link
  let _nostdlib = set no_std_include
  let _o s = output_name := Some s
  let _opaque = set opaque
  let _open s = open_modules := s :: !open_modules
  let _output_obj () = output_c_object := true; custom_runtime := true
  let _output_complete_obj () =
    output_c_object := true;
    output_complete_object := true;
    custom_runtime := true
  let _pack = set make_package
  let _pp s = preprocessor := Some s
  let _ppx s = first_ppx := s :: !first_ppx
  let _plugin p = Compplugin.load p
  let _principal = set principal
  let _no_principal = unset principal
  let _rectypes = set recursive_types
  let _no_rectypes = unset recursive_types
  let _runtime_variant s = runtime_variant := s
  let _safe_string = unset unsafe_string
  let _short_paths = unset real_paths
  let _strict_sequence = set strict_sequence
  let _no_strict_sequence = unset strict_sequence
  let _strict_formats = set strict_formats
  let _no_strict_formats = unset strict_formats
  let _thread = set use_threads
  let _vmthread = set use_vmthreads
  let _unboxed_types = set unboxed_types
  let _no_unboxed_types = unset unboxed_types
  let _unsafe = set unsafe
  let _unsafe_string = set unsafe_string
  let _use_prims s = use_prims := s
  let _use_runtime s = use_runtime := s
  let _v () = print_version_and_library "compiler"
  let _version = print_version_string
  let _vnum = print_version_string
  let _w = (Warnings.parse_options false)
  let _warn_error = (Warnings.parse_options true)
  let _warn_help = Warnings.help_warnings
  let _color option =
    begin match parse_color_setting option with
          | None -> ()
          | Some setting -> color := Some setting
    end
  let _where = print_standard_library
  let _verbose = set verbose
  let _nopervasives = set nopervasives
  let _match_context_rows n = match_context_rows := n
  let _dump_into_file = set dump_into_file
  let _dno_unique_ids = unset unique_ids
  let _dunique_ids = set unique_ids
  let _dsource = set dump_source
  let _dparsetree = set dump_parsetree
  let _dtypedtree = set dump_typedtree
  let _drawlambda = set dump_rawlambda
  let _dlambda = set dump_lambda
  let _dinstr = set dump_instr
  let _dcamlprimc = set keep_camlprimc_file
  let _dtimings () = profile_columns := [ `Time ]
  let _dprofile () = profile_columns := Profile.all_columns

  let _args = Arg.read_arg
  let _args0 = Arg.read_arg0

  let anonymous = anonymous
end)


let compile ppf =
  begin try
    Compenv.process_deferred_actions
      ( ppf
      , Compile.implementation
          ~frontend:(Some Cache.parse_impl)
          ~typing:(Some Cache.typecheck_impl)
      , Compile.interface
          ~frontend:(Some Cache.parse_intf)
          ~typing:None
      , ".cmo"
      , ".cma"
      );
  with Arg.Bad msg ->
    (* TODO: do not exit *)
    begin
      prerr_endline msg;
      Clflags.print_arguments usage;
      exit 2
    end
  end;

  readenv ppf Before_link;
  if
    List.length
      (List.filter (fun x -> !x)
         [make_archive;make_package;compile_only;output_c_object])
      > 1
  then
    if !print_types then
      fatal "Option -i is incompatible with -pack, -a, -output-obj"
    else
      fatal "Please specify at most one of -pack, -a, -c, -output-obj";

  if !make_archive then begin
    Compmisc.init_path false;

    Bytelibrarian.create_archive
      (Compenv.get_objfiles ~with_ocamlparam:false)
      (extract_output !output_name);
    Warnings.check_fatal ();
  end
  else if !make_package then begin
    Compmisc.init_path false;
    let extracted_output = extract_output !output_name in
    let revd = get_objfiles ~with_ocamlparam:false in
    Compmisc.with_ppf_dump ~fileprefix:extracted_output (fun ppf_dump ->
      Bytepackager.package_files ~ppf_dump (Compmisc.initial_env ())
        revd (extracted_output));
    Warnings.check_fatal ();
  end
  else if not !compile_only && !objfiles <> [] then begin
    let target =
      if !output_c_object then
        let s = extract_output !output_name in
        if (Filename.check_suffix s Config.ext_obj
          || Filename.check_suffix s Config.ext_dll
          || Filename.check_suffix s ".c")
        then s
        else
          fatal
            (Printf.sprintf
               "The extension of the output file must be .c, %s or %s"
               Config.ext_obj Config.ext_dll
            )
      else
        default_output !output_name
    in
    Compmisc.init_path false;
    Bytelink.link (get_objfiles ~with_ocamlparam:true) target;
    Warnings.check_fatal ();
  end

let reset () =
  show_config := false;
  show_config_var := None;
  Clflags.reset_flags ();
  Compenv.reset ();
  Config.load_path := [];
  Config.interface_suffix := ".mli";
  Asmlink.reset ();
  Bytelink.reset ();
  Profile.reset ();
  native_code := false

let run argv env cwd =
  let ppf = Format.str_formatter in
  Location.formatter_for_warnings := ppf;
  try Warnings.context @@ fun () ->
    Sys.chdir cwd;
    reset ();

    readenv ppf Before_args;

    Clflags.reset_arguments ();
    Clflags.add_arguments __LOC__ Options.list;
    Clflags.parse_arguments argv anonymous usage;
    Compmisc.read_color_env ();

    (match !show_config, !show_config_var with
    | true, _ ->
      (Unix.WEXITED(0), Config.config_string ^ "\n", "")
    | _, Some name ->
      (match Config.config_var name with
      | Some var -> (Unix.WEXITED(0), var, "")
      | None -> (Unix.WEXITED(2), "", "")
      )
    | _, None ->
      compile ppf;
      reset ();
      (Unix.WEXITED(0), "", Format.flush_str_formatter ())
    )
  with exc ->
    Location.report_exception ppf exc;
    (Unix.WEXITED(2), "", Format.flush_str_formatter ())
