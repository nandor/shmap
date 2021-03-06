(** Scheduling *)

open Stdune

(** [go ?log ?config ?gen_status_line fiber] runs the following fiber until it
    terminates. [gen_status_line] is used to print a status line when [config.display =
    Progress]. *)
val go
  :  ?log:Log.t
  -> ?config:Config.t
  -> ?gen_status_line:(unit -> string option)
  -> 'a Fiber.t
  -> 'a

(** Set the status line generator for the current scheduler *)
val set_status_line_generator : (unit -> string option) -> unit Fiber.t

val set_concurrency : int -> unit Fiber.t

(** Scheduler information *)
type t

(** Wait until less tham [!Clflags.concurrency] external processes are running and return
    the scheduler information. *)
val get_scheduler : unit -> t Fiber.t

(** Logger *)
val log : t -> Log.t

(** Execute the given callback with current directory temporarily changed *)
val run
  :  t
  -> string
  -> string array
  -> string array
  -> Path.t option
  -> (Unix.process_status * string * string) Fiber.t

(** Display mode for this scheduler *)
val display : t -> Config.Display.t

(** Print something to the terminal *)
val print : t -> string -> unit
