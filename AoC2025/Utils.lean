def readToArray (args: List String) (delim: String)  : IO (List String) := do
  match args with
  | [] =>
    IO.println "Error: Provide an Input File"
    return []
  | path :: _ =>
    let file <- IO.FS.readFile path
    return file.splitOn delim

