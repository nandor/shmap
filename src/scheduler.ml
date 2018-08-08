open Import
open Fiber.O


type t =
  { log                       : Log.t
  ; original_cwd              : string
  ; display                   : Config.Display.t
  ; mutable status_line       : string
  ; mutable gen_status_line   : unit -> string option
  ; proc_pool                 : Proc_pool.t
  }

type running_job =
  { job  : Proc_pool.job
  ; ivar : (Unix.process_status * string * string) Fiber.Ivar.t
  }

module Running_jobs : sig
  val add : running_job -> unit
  val wait : t -> running_job * Proc_pool.job_result
  val count : unit -> int
end = struct
  let all = Hashtbl.create 128

  let add job = Hashtbl.add all job.job job

  let wait t =
    let result = Proc_pool.wait_any t.proc_pool in
    let job = Option.value_exn (Hashtbl.find all result.job_id) in
    Hashtbl.remove all result.job_id;
    (job, result)

  let count () = Hashtbl.length all
end

let log t = t.log
let display t = t.display
let hide_status_line s =
  let len = String.length s in
  if len > 0 then Printf.eprintf "\r%*s\r" len ""

let show_status_line s =
  prerr_string s

let print t msg =
  let s = t.status_line in
  hide_status_line s;
  prerr_string msg;
  show_status_line s;
  flush stderr

let t_var : t Fiber.Var.t = Fiber.Var.create ()

let set_status_line_generator f =
  Fiber.Var.get_exn t_var >>| fun t ->
  t.gen_status_line <- f

let set_concurrency n =
  Fiber.return ()

let get_scheduler () =
  Fiber.Var.get_exn t_var

let run t prog args env cwd =
  let dir = Option.(
    map cwd Path.to_absolute_filename |> value ~default:t.original_cwd
  ) in
  let ivar = Fiber.Ivar.create () in
  let job = Proc_pool.start t.proc_pool prog args env dir in
  Running_jobs.add { job; ivar };
  Fiber.Ivar.read ivar

let rec go_rec t =
  Fiber.yield ()
  >>= fun () ->
  let count = Running_jobs.count () in
  if count = 0 then begin
    hide_status_line t.status_line;
    flush stderr;
    Proc_pool.shutdown t.proc_pool;
    Fiber.return ()
  end else begin
    if t.display = Progress then begin
      match t.gen_status_line () with
      | None ->
        if t.status_line <> "" then begin
          hide_status_line t.status_line;
          flush stderr
        end
      | Some status_line ->
        let status_line = sprintf "%s (jobs: %u)" status_line count in
        hide_status_line t.status_line;
        show_status_line   status_line;
        flush stderr;
        t.status_line <- status_line;
    end;
    let job, status = Running_jobs.wait t in
    Fiber.Ivar.fill job.ivar (status.status, status.stdout, status.stderr)
    >>= fun () ->
    go_rec t
  end

let go ?(log=Log.no_log) ?(config=Config.default)
      ?(gen_status_line=fun () -> None) fiber =
  Log.infof log "Workspace root: %s"
    (Path.to_absolute_filename Path.root |> String.maybe_quoted);
  let cwd = Sys.getcwd () in
  if cwd <> initial_cwd then
    Printf.eprintf "Entering directory '%s'\n%!"
      (if Config.inside_dune then
         let descendant_simple p ~of_ =
           match
             String.drop_prefix p ~prefix:of_
           with
           | None | Some "" -> None
           | Some s -> Some (String.sub s ~pos:1 ~len:(String.length s - 1))
         in
         match descendant_simple cwd ~of_:initial_cwd with
         | Some s -> s
         | None ->
           match descendant_simple initial_cwd ~of_:cwd with
           | None -> cwd
           | Some s ->
             let rec loop acc dir =
               if dir = Filename.current_dir_name then
                 acc
               else
                 loop (Filename.concat acc "..") (Filename.dirname dir)
             in
             loop ".." (Filename.dirname s)
       else
         cwd);
  let concurrency = match config.concurrency with Auto -> 1 | Fixed n -> n in
  let t =
    { log
    ; gen_status_line
    ; original_cwd = cwd
    ; display      = config.display
    ; status_line  = ""
    ; proc_pool    = Proc_pool.spawn concurrency
    }
  in
  printer := print t;
  let fiber =
    Fiber.Var.set t_var t
      (Fiber.with_error_handler (fun () -> fiber) ~on_error:Report_error.report)
  in
  Fiber.run
    (Fiber.fork_and_join_unit
       (fun () -> go_rec t)
       (fun () -> fiber))
