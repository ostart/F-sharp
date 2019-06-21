// 47.4.1
let f n =
  if(n < 2) then 1
  else
    let mutable i = ref 1
    let mutable res = ref 1
    while !i <= n do
      res := !res * !i
      i := !i + 1
    !res

// 47.4.2
let fibo n =
  if(n = 0) then 0
  elif(n = 1) then 1
  else
    let mutable minus2 = ref 0
    let mutable minus1 = ref 1
    let mutable i = ref 2
    let mutable curr = ref 0
    while !i <= n do
      curr := !minus1 + !minus2
      minus2 := !minus1
      minus1 := !curr
      i := !i + 1
    !curr

