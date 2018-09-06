
type cmd
  = Run of string * string array * string array * string
  | Close

type cmd_result
  = Done of Unix.process_status * string * string
