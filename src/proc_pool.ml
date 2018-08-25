

exception No_job_running
exception Job_finished


(* External ID tracking a job *)
type job = int

(* Job submitted to a worker. *)
type job_queued =
  { queue_id: int
  ; cmd: string
  ; argv: string array
  ; env: string array
  ; cwd: string
  }

(* Finished job *)
type job_result =
  { job_id: int
  ; status: Unix.process_status
  ; stdout: string
  ; stderr: string
  }


(* Structure tracking a child process. *)
type worker =
  { pid: int
  ; chan_rd: in_channel
  ; chan_wr: out_channel
  }

(* Job bool, tracking workers and queued jobs. *)
type t =
  { mutable workers : worker list
  ; mutable busy : (worker * job) list
  ; mutable pending : job_queued list
  ; mutable finished : job_result list
  ; mutable next_id : int
  }

type cmd
  = Run of string * string array * string array * string
  | Close


let worker_name = "main_worker.exe"
let worker_path = Filename.(concat (dirname Sys.executable_name) worker_name)

let job_send { chan_wr } cmd =
  let buffer = Bytes.unsafe_of_string (Marshal.to_string cmd []) in
  let pipe_wr = Unix.descr_of_out_channel chan_wr in
  ignore (Unix.write pipe_wr buffer 0 (Bytes.length buffer))

let job_next t =
  match t with
  | { pending = [] } -> ()
  | { workers = [] } -> ()
  | { workers = w :: ws; pending = p :: ps } ->
      t.workers <- ws;
      t.busy <- (w, p.queue_id) :: t.busy;
      t.pending <- ps;
      job_send w (Run(p.cmd, p.argv, p.env, p.cwd))

let dequeue_jobs t =
  let fds = List.map (fun ({ chan_rd }, _) ->
    Unix.descr_of_in_channel chan_rd) t.busy
  in
  match Unix.select fds [] [] (-1.) with
  | [], _, _ ->
    ()
  | fds, _, _ ->
    t.busy
      |> List.filter (fun ({ chan_rd }, _) ->
        List.mem (Unix.descr_of_in_channel chan_rd) fds)
      |> List.iter (fun (w, job) ->
        (* Read the response and finish the job. *)
        let { chan_rd } = w in
        let status, stdout, stderr = Marshal.from_channel chan_rd in
        let finished_job = { job_id = job; status; stdout; stderr } in

        t.workers <- w :: t.workers;
        t.busy <- List.filter (fun (w', _) -> w != w') t.busy;
        t.finished <- finished_job :: t.finished;
        (* After the worker is freed, start a job from the queue *)
        job_next t)

(* Creates a new process pool with a given number of threads. *)
let spawn njobs =
  let workers = List.init njobs Unix.(fun _ ->
    let chan_child_rd, chan_wr = pipe ~cloexec:false () in
    let chan_rd, chan_child_wr = pipe ~cloexec:false () in
    match fork () with
    | 0 ->
      (* Child process *)
      close chan_rd;
      close chan_wr;

      dup2 chan_child_rd (Obj.magic 100);
      close chan_child_rd;
      dup2 chan_child_wr (Obj.magic 101);
      close chan_child_wr;

      execv worker_path [|worker_name|]
    | pid ->
      (* Parent process *)
      close chan_child_wr;
      close chan_child_rd;
      { pid
      ; chan_rd = in_channel_of_descr chan_rd
      ; chan_wr = out_channel_of_descr chan_wr
      })
  in { workers; busy = []; pending = []; finished = []; next_id = 0 }

(* Starts a job, delegating it to the process pool. *)
let start t cmd argv env cwd =
  (* Place the job on the queue *)
  let queue_id = t.next_id in
  t.next_id <- queue_id + 1;
  let job = { queue_id; cmd; argv; env; cwd } in
  t.pending <- List.append t.pending [job];
  (* Try to start it if there is a free process in the pool *)
  job_next t;
  (* Return a handle to the job *)
  queue_id

(* Waits for a specific job to finish. *)
let rec wait t job =
  let rec find_item f = function
    | y :: ys when f y -> (Some y, ys)
    | y :: ys -> let (ans, zs) = find_item f ys in (ans, y :: zs)
    | [] -> (None, [])
  in
  let pending = List.exists (fun { queue_id } -> queue_id == job) t.pending in
  let running = List.exists (fun (_, id) -> id == job) t.busy in
  match find_item (fun { job_id } -> job_id == job) t.finished with
  | Some job, rest ->
    (* Remove the job from the list of finished jobs. *)
    t.finished <- rest;
    job
  | None, _ when not (pending || running) ->
    (* Job finished already, should not happen. *)
    raise Job_finished
  | None, _ ->
    (* Wait on all file descriptors for any jobs to finish. *)
    dequeue_jobs t;
    (* The previous step dequeued some jobs - keep checking the list *)
    wait t job

(* Waits for any job to finish, returning it. *)
let rec wait_any t =
  match t.finished, t.pending, t.busy with
  | j :: js, _, _ ->
    t.finished <- js;
    j
  | [], [], [] ->
    raise No_job_running
  | [], _, _ ->
    (* Dequeue any jobs, then return the first one. *)
    dequeue_jobs t;
    wait_any t

(* Stops the pool, without waiting for all jobs to finish. *)
let rec shutdown t =
  t.workers |> List.iter (fun job ->
    job_send job Close;
    ignore (Unix.waitpid [] job.pid)
  )
