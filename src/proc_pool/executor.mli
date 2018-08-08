

type result =
  { status: Unix.process_status
  ; stdout: string
  ; stderr: string
  }

val execute
  :  string
  -> string array
  -> string array
  -> string
  -> result
