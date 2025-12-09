import AoC2025.Utils

def applyOperation (a : Nat) (b : Nat) (sign : String) : Nat :=
  if sign == "+" then a + b
  else if sign == "*" then (if a > 0 then a else 1) * (if b > 0 then b else 1)
  else 0

def part1 (input : List String) : IO Nat := do
    let mut input := input
    if input.getLast!.length == 0 then
      input := input.take (input.length-1)

    let signs : List String := (((input.getLast!.splitOn " ").map (String.trim ·)).filter (· != ""))
    input := input.take (input.length-1)

    let mut res := 0
    for i in [:signs.length] do
      res := res + ((input.map (fun l => ((l.splitOn " ").filter (· != ""))[i]!.trim.toNat!)).foldl (applyOperation · · signs[i]!) 0)

    return res

def part2 (input : List String) : IO Nat := do
    let matInput : List (List String) := (input.map (fun l => l.toList.map (String.singleton ·)))
    let mut stack : List Nat := []
    let mut currentOperator := ""

    let mut res := 0
    for i in [:(matInput.mergeSort (fun a b => a.length > b.length)).head!.length] do
      let mut s := ""
      for l in matInput do
        let val := if i >= l.length then "" else l[i]!
        if val == "+" || val == "*" then
          if currentOperator == "" then
            currentOperator := val
          else
            res := res + (stack.foldr (applyOperation · · currentOperator) 0)
            stack := []
            currentOperator := val
        else
          s := s ++ val
      if s.trim.length > 0 then
        stack := s.trim.toNat! :: stack

    if currentOperator != "" && !stack.isEmpty then
      res := res + (stack.foldr (applyOperation · · currentOperator) 0)

    return res


def main (args : List String) : IO Unit := do
    let mut input <- readToArray args "\n"

    let res1 <- part1 input
    let res2 <- part2 input

    IO.println s!"The Grand Total is: {res1}"
    IO.println s!"The Real Grand Total is: {res2}"

