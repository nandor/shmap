

type result =
  { status: Unix.process_status
  ; stdout: string
  ; stderr: string
  }


let execute cmd args env cwd = match cmd with
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
    ignore (Unix.read fd buffer 0 size);
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
  let pid = Unix.create_process_env cmd args env pipe_rd stdout stderr in
  let _, status = Unix.waitpid [] pid in

  (* Return the status and the output. *)
  { status; stdout = read_tmp stdout; stderr = read_tmp stderr }
