
type cmd
  = Run of string * string array * string array * string
  | Close


let execute prog args env cwd =
  match Filename.basename prog with
  | "ocamlc.opt" ->
    Worker.Ocamlc.run args env cwd
  | "ocamldep.opt" ->
    Worker.Ocamldep.run args env cwd
  | "ocamlopt.opt" ->
    Worker.Ocamlopt.run args env cwd
  | _ ->
    (* Create a temporary file, open it and then unlink it. *)
    let open_tmp suffix =
      let tmp_path = Filename.temp_file "dune" suffix in
      let tmp_fd = Unix.openfile tmp_path [Unix.O_RDWR] 0 in
      Unix.unlink tmp_path;
      tmp_fd
    in
    (* Reads a temporary file and closes it *)
    let read_tmp fd =
      let size = Unix.((fstat fd).st_size) in
      let buffer = Bytes.create size in
      ignore (Unix.lseek fd 0 Unix.SEEK_SET);
      let rec loop off =
        let read_bytes = Unix.read fd buffer off (size - off) in
        let current = off + read_bytes in
        if current != size then loop current
      in
      loop 0;
      Unix.close fd;
      Bytes.unsafe_to_string buffer
    in

    (* Redirect stdout and stderr *)
    let stdout = open_tmp "stdout" in
    let stderr = open_tmp "stderr" in

    (* Create a pipe and close the write end, killing a potential reader. *)
    let pipe_rd, pipe_wr = Unix.pipe () in
    Unix.close pipe_wr;

    (* Run the process and wait for it to finish. *)
    Sys.chdir cwd;
    let pid = Unix.create_process_env prog args env pipe_rd stdout stderr in
    let _, status = Unix.waitpid [] pid in

    (* Return the status and the output. *)
    (status, read_tmp stdout, read_tmp stderr)


let proc_main chan_rd chan_wr =
  let rec loop () =
    match Marshal.from_channel chan_rd with
    | Close ->
      (* Shutdown request, stop waiting for commands. *)
      ()
    | Run(cmd, args, env, cwd) ->
      (* Run the request to completion, capturing outputs. *)
      let status =
        try execute cmd args env cwd
        with ex -> (Unix.WEXITED(255), "", Printexc.to_string ex)
      in

      (* Write the repsonse back to the parent. *)
      Marshal.to_channel chan_wr status [];
      flush chan_wr;

      (* Keep on waiting for jobs. *)
      loop ()
  in loop ()

let () =
  let chan_rd = Unix.in_channel_of_descr (Obj.magic 100) in
  let chan_wr = Unix.out_channel_of_descr (Obj.magic 101) in
  proc_main chan_rd chan_wr
