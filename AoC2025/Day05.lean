import AoC2025.Utils

def main (args : List String) : IO Unit := do
    let input <- readToArray args "\n"

    let mut ranges : List (List Nat) := []
    let mut ingredients : List Nat := []

    let mut proc_ingr := false
    for line in input do
      if line == "" then
        proc_ingr := true
        continue

      if proc_ingr then
        ingredients := line.toNat! :: ingredients
      else
        ranges := ((line.splitOn "-").map (fun d => d.toNat!)) :: ranges

    let mut freshIngredients := 0

    for ingredient in ingredients do
      for range in ranges do
        if ingredient >= range.head! && ingredient <= range.getLast! then
          freshIngredients := freshIngredients + 1
          break

    ranges := (ranges.mergeSort (fun a b => a.getLast! < b.getLast!)).mergeSort (fun a b => a.head! < b.head!)
    for i in [0:ranges.length] do
      if ranges[i]!.length == 0 then continue
      for j in [i+1:ranges.length] do
        if ranges[j]!.length == 0 then continue
        if ranges[j]!.head! <= ranges[i]!.getLast! then
          if ranges[j]!.getLast! >= ranges[i]!.getLast! then
            ranges := ranges.set i [ranges[i]!.head!, ranges[j]!.getLast!]
          ranges := ranges.set j []

    let ingredient_ids := ((ranges.filter (fun r => r.length > 0)).map (fun r => 1 + r.getLast! - r.head!)).sum

    IO.println s!"The number of fresh ingredients is: {freshIngredients}"
    IO.println s!"The number of ingredients ids is: {ingredient_ids}"








