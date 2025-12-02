import AoC2025.Utils

def main (args : List String) : IO Unit := do
  match args with
  | [] => IO.println "Error: Provide an Input File"
  | path :: _ =>
    let input <- readToArray path

    let mut position : Int := 50
    let mut password := 0

    for line in input do
      if line.length < 2 then continue
      let direction :=  (if line.front == 'R' then 1 else -1)
      let quantity : Int := (line.drop 1).toNat!

      position := (position + (quantity * direction)) % 100

      if position == 0
      then password := password + 1

    IO.println s!"The Password is: {password}"
    return





