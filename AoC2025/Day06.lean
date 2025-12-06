import AoC2025.Utils

def applyOperation (a : Nat) (b : Nat) (sign : String) : Nat :=
  if sign == "+" then a + b
  else if sign == "*" then (if a > 0 then a else 1) * (if b > 0 then b else 1)
  else 0


def main (args : List String) : IO Unit := do
    let mut input <- readToArray args "\n"

    if input.getLast!.length == 0 then
      input := input.take (input.length-1)

    let signs : List String := (((input.getLast!.splitOn " ").map (String.trim ·)).filter (· != ""))
    input := input.take (input.length-1)

    let mut res := 0
    for i in [:signs.length] do
      res := res + ((input.map (fun l => ((l.splitOn " ").filter (· != ""))[i]!.trim.toNat!)).foldl (applyOperation · · signs[i]!) 0)

    IO.println s!"The Grand Total is: {res}"









