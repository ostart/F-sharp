let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)

// 50.2.1
let fac_seq = seq {
    let mutable i = 0
    while true do
      yield (factorial i)
      i <- i + 1
       }

// 50.2.2
let seq_seq = seq {
    yield 0
    let mutable i = 1
    while true do
      yield! seq [-i;i]
      i <- i + 1
       }