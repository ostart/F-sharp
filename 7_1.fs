// 7.1.1
let rec fibo = function
  | n when n <= 1 -> 0
  | 2 -> 1
  | n -> fibo(n-2) + fibo(n-1)

// 7.1.2
let rec sum = function
    |1 -> 1
    |n -> n + sum(n-1)

// 7.1.3
let rec sum2 = function 
 | (m,0) -> m 
 | (m,n) -> m + n + sum2(m, n-1)