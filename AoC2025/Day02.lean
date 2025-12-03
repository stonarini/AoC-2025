import AoC2025.Utils

def splitVal (val: String) (bump : Bool) : (Nat × Nat) :=
      if val.length == 1 then
        (0, val.toNat!)
      else
        let rval := if val.length % 2 != 0 && bump then s!"{10 ^ val.length}" else val
        let half := rval.length / 2
        let f := (rval.take half).toNat!
        let s := (rval.drop half).toNat!
        (f, s)

def joinVal (f: Nat) (s: Nat) : Nat := (s!"{f}{s}").toNat!

def main (args : List String) : IO Unit := do
    let input <- readToArray args ","

    let mut invalidCodes : List Nat := []

    for codes in input do
      if codes.length == 0 then continue
      let cd := codes.splitOn "-"
      let min := cd.head!
      let max := cd.getLast!.trim

      if min.length % 2 != 0 && max.length % 2 != 0 then continue

      let mut (f_min, s_min) := splitVal min true
      let mut (f_max, s_max) := splitVal max false

      if f_min > f_max then
        let k := (s!"{s_max}").length
        f_max := (10 ^ (Max.max (k - 1) 1)) - 1
        s_max := (10 ^ k) - 1

      while f_min <= f_max do
       if f_min == s_min || (f_min > s_min && (f_min < f_max || f_min <= s_max)) then
         invalidCodes := (joinVal f_min f_min) :: invalidCodes
       f_min := f_min + 1
       s_min := 0

    IO.println s!"The Sum of Invalid Codes is: {invalidCodes.sum}"
    return
