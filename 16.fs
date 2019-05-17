// 16.1
let notDivisible (n,m) = m % n = 0

// 16.2
let prime n = 
  let rec iter = function
    | (number, bas) when (bas > (number / 2)) -> true
    | (number, bas) when (number % bas = 0) -> false
    | (number, bas) -> iter (number, (bas + 1))

  iter(n,2)