

exception No_job_running
exception Job_finished


(* External ID tracking a job *)
type job = int


(* Job submitted to a worker. *)
type job_queued =
  { queue_id: int
  ; cmd: string
  ; args: string array
  ; env: string array
  }

(* Finished job *)
type job_result =
  { job_id: int
  ; status: Unix.process_status
  ; output: string
  }


(* Structure tracking a child process. *)
type worker =
  { pid: int
  ; pipe_rd: Unix.file_descr
  ; chan_rd: in_channel
  ; pipe_wr: Unix.file_descr
  }

(* Job bool, tracking workers and queued jobs. *)
type t =
  { mutable workers : worker list
  ; mutable busy : (worker * job) list
  ; mutable pending : job_queued list
  ; mutable finished : job_result list
  ; mutable next_id : int
  }


type cmd = Run of job_queued | Close


let job_send { pipe_wr } cmd =
  let buffer = Bytes.unsafe_of_string (Marshal.to_string cmd []) in
  ignore (Unix.write pipe_wr buffer 0 (Bytes.length buffer))

let job_next t =
  match t with
  | { pending = [] } -> ()
  | { workers = [] } -> ()
  | { workers = w :: ws; pending = p :: ps } ->
      t.workers <- ws;
      t.busy <- (w, p.queue_id) :: t.busy;
      t.pending <- ps;
      job_send w (Run p)

let proc_main pipe_rd pipe_wr =
  let chan_rd = Unix.in_channel_of_descr pipe_rd in
  let chan_wr = Unix.out_channel_of_descr pipe_wr in
  let rec loop () =
    match Marshal.from_channel chan_rd with
    | Close ->
      (* Shutdown request, stop waiting for commands. *)
      ()
    | Run { cmd; args; env } ->
      (* Run the request to completion, capturing outputs. *)
      let status = Executor.execute cmd args env in

      (* Write the repsonse back to the parent. *)
      Marshal.to_channel chan_wr status [];
      flush chan_wr;

      (* Keep on waiting for jobs. *)
      loop ()
  in loop ()

let dequeue_jobs t =
  let fds = List.map (fun ({ pipe_rd }, _) -> pipe_rd) t.busy in
  match Unix.select fds [] [] (-1.) with
  | [], _, _ ->
    ()
  | fds, _, _ ->
    t.busy
      |> List.filter (fun ({ pipe_rd }, _) -> List.mem pipe_rd fds)
      |> List.iter (fun (w, job) ->
        (* Read the response and finish the job. *)
        let { chan_rd } = w in
        let result: Executor.result = Marshal.from_channel chan_rd in
        let finished_job =
          { job_id = job
          ; status = result.status
          ; output = result.output
          }
        in
        t.workers <- w :: t.workers;
        t.busy <- List.filter (fun (w', _) -> w != w') t.busy;
        t.finished <- finished_job :: t.finished;
        (* After the worker is freed, start a job from the queue *)
        job_next t)


(* Creates a new process pool with a given number of threads. *)
let spawn njobs =
  let workers = List.init njobs (fun _ ->
    let pipe_child_rd, pipe_wr = Unix.pipe () in
    let pipe_rd, pipe_child_wr = Unix.pipe () in
    match Unix.fork () with
    | 0 ->
      proc_main pipe_child_rd pipe_child_wr;
      exit 0
    | pid ->
      { pid
      ; pipe_rd
      ; chan_rd = Unix.in_channel_of_descr pipe_rd
      ; pipe_wr
      })
  in { workers; busy = []; pending = []; finished = []; next_id = 0 }

(* Starts a job, delegating it to the process pool. *)
let start t cmd args env =
  (* Place the job on the queue *)
  let queue_id = t.next_id in
  t.next_id <- queue_id + 1;
  let job = { queue_id; cmd; args; env } in
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
