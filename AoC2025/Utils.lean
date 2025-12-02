def readToArray (path: String) : IO (List String) := do
  let file <- IO.FS.readFile path
  return file.splitOn "\n"


