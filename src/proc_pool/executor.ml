

type result =
  { status: Unix.process_status
  ; output: string
  }


let execute cmd args env = match cmd with
  | _ ->
    (* Create a temporary file, open it and then unlink it. *)
    let tmp_path = Filename.temp_file "dune" "output" in
    let tmp_fd = Unix.openfile tmp_path [Unix.O_RDWR] 0 in
    Unix.unlink tmp_path;

    (* Create a pipe and close the write end, killing a potential reader. *)
    let pipe_rd, pipe_wr = Unix.pipe () in
    Unix.close pipe_wr;

    (* Run the process and wait for it to finish. *)
    let pid = Unix.create_process_env cmd args env pipe_rd tmp_fd tmp_fd in
    let _, status = Unix.waitpid [] pid in

    (* Read the output into a string. *)
    let size = Unix.((fstat tmp_fd).st_size) in
    let buffer = Bytes.create size in
    ignore (Unix.lseek tmp_fd 0 Unix.SEEK_SET);
    ignore (Unix.read tmp_fd buffer 0 size);
    Unix.close tmp_fd;

    (* Return the status and the output. *)
    { status; output = Bytes.unsafe_to_string buffer }
