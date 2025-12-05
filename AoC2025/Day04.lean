import AoC2025.Utils

def main (args : List String) : IO Unit := do
    let input <- readToArray args "\n"

    let mut mat : List (List Char) := []
    let mut rolls := 0

    for line in input do
      if line.length == 0 then continue
      mat := mat ++ [line.toList]

    for row in [0:mat.length] do
      for col in [0:(mat[row]!).length] do
        if mat[row]![col]! == '.' then continue
        let mut adc := 0
        for i in [row-1:row+2] do
          if i < 0 || i >= mat.length then continue
          for j in [col-1:col+2] do
            if j < 0 || j >= mat[i]!.length || (col == j && row == i) then continue
            if mat[i]![j]! == '@' then adc := adc + 1

        if adc < 4 then rolls := rolls + 1

    IO.println s!"Accessible Rolls of Paper: {rolls}"



