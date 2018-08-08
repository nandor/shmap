

type result =
  { status: Unix.process_status
  ; output: string
  }

val execute : string -> string array -> string array -> result
