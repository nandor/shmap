
exception No_job_running
exception Job_finished

type t
type job


type job_result =
  { job_id: int
  ; status: Unix.process_status
  ; output: string
  }

val spawn : int -> t

val start : t -> string -> string array -> string array -> job

val wait : t -> job -> job_result

val wait_any : t -> job_result

val shutdown : t -> unit
