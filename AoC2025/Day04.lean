import AoC2025.Utils

def part1 (mat: List (List Char)) : IO Nat := do
    let mut mat := mat
    let mut rolls := 0

    for row in [0:mat.length] do
      for col in [0:(mat[row]!).length] do
        if mat[row]![col]! == '.' then continue
        let mut adc := 0
        for i in [row-1:row+2] do
          if i < 0 || i >= mat.length then continue
          for j in [col-1:col+2] do
            if j < 0 || j >= mat[i]!.length || (col == j && row == i) then continue
            if mat[i]![j]! == '@' then adc := adc + 1

        if adc < 4 then
          rolls := rolls + 1

    return rolls

def part2 (mat: List (List Char)) : IO Nat := do
    let mut mat := mat
    let mut rollsList : List Nat := []

    let mut idx := 0

    while idx == 0 || rollsList[idx-1]! != 0 do
      let mut rolls := 0
      for row in [0:mat.length] do
        for col in [0:(mat[row]!).length] do
          if mat[row]![col]! == '.' then continue
          let mut adc := 0
          for i in [row-1:row+2] do
            if i < 0 || i >= mat.length then continue
            for j in [col-1:col+2] do
              if j < 0 || j >= mat[i]!.length || (col == j && row == i) then continue
              if mat[i]![j]! == '@' then adc := adc + 1

          if adc < 4 then
            rolls := rolls + 1
            mat := mat.set row (mat[row]!.set col '.')

      rollsList := rollsList ++ [rolls]
      idx := idx + 1
    return rollsList.sum

def main (args : List String) : IO Unit := do
    let input <- readToArray args "\n"

    let mut mat : List (List Char) := []
    for line in input do
      if line.length == 0 then continue
      mat := mat ++ [line.toList]

    let res1 <- part1 mat;
    let res2 <- part2 mat;

    IO.println s!"Accessible Rolls of Paper: {res1}"
    IO.println s!"All Accessible Rolls of Paper: {res2}"



