import AoC2025.Utils

def part1 (input: List String) : IO Nat := do
    let mut max_joltage : List Nat := []

    for bat in input do
      if bat == "" then continue
      let digits := (bat.toList.map String.singleton).map String.toNat!
      let max := ((digits.take (digits.length-1)).max?).get!

      let sec_digits := digits.drop ((digits.idxOf max) + 1)
      let sec_max := ((sec_digits.take sec_digits.length).max?).get!

      max_joltage := (joinVal max sec_max) :: max_joltage
    return max_joltage.sum

def part2 (input: List String) : IO Nat := do
    let mut max_joltage : List Nat := []

    for bat in input do
      if bat == "" then continue
      let mut digits := (bat.toList.map String.singleton).map String.toNat!
      if digits.length < 12 then continue

      let mut ordered_digits : List Nat := []

      while ordered_digits.length < 12 do
        let idx_digits := (List.range digits.length).zip digits
        let val := (((idx_digits.take (idx_digits.length + ordered_digits.length - 11)).mergeSort (fun a b =>if a.2 == b.2 then a.1 > b.1 else a.2 < b.2)).getLast!)
        ordered_digits := val.2 :: ordered_digits
        digits := digits.drop (val.1+1)

      max_joltage := ((ordered_digits.reverse).foldl (joinVal · ·) 0) :: max_joltage


    return max_joltage.sum


def main (args : List String) : IO Unit := do
    let input <- readToArray args "\n"

    let res1 <- part1 input;
    let res2 <- part2 input;

    IO.println s!"Total output joltage: {res1}"
    IO.println s!"Boosted total output joltage: {res2}"

