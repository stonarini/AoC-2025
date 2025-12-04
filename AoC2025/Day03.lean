import AoC2025.Utils

def main (args : List String) : IO Unit := do
    let input <- readToArray args "\n"

    let mut max_joltage : List Nat := []

    for bat in input do
      if bat == "" then continue
      let digits := (bat.toList.map String.singleton).map String.toNat!
      let max := ((digits.take (digits.length-1)).max?).get!

      let sec_digits := digits.drop ((digits.idxOf max) + 1)
      let sec_max := ((sec_digits.take sec_digits.length).max?).get!

      max_joltage := (joinVal max sec_max) :: max_joltage

    IO.println s!"Total output joltage: {max_joltage.sum}"

