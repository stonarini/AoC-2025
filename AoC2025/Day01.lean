import AoC2025.Utils

def main (args : List String) : IO Unit := do
    let input <- readToArray args "\n"

    let mut position := 50
    let mut part1 := 0
    let mut part2 := 0

    for line in input do
      if line.length < 2 then continue
      let direction :=  (if line.front == 'R' then 1 else -1)
      let quantity : Int := (line.drop 1).toNat!

      if direction == 1 then
        part2 := part2 + ((position + quantity) / 100)
      else if quantity >= position then
        part2 := part2 + ((quantity - position) / 100) + (cond (position == 0) 0 1)

      position := (position + (quantity * direction)) % 100

      if position == 0 then part1 := part1 + 1

    IO.println s!"The Password is: {part1}"
    IO.println s!"The Real Password is: {part2}"
    return





