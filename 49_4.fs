let rec factorial n =
    match n with
    | 0 | 1 -> 1
    | _ -> n * factorial(n-1)

let rec alternate n =
    if(n=0) then 0
    else
      if((n%2)=0) then alternate(n-1) + n
      else alternate(n-1) - n

// 49.5.1
let even_seq = Seq.initInfinite (fun i -> i + i)

// 49.5.2
let fac_seq = Seq.initInfinite (factorial)

// 49.5.3
let seq_seq = Seq.initInfinite(alternate)
