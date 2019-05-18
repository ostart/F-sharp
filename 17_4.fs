// 17.1
let rec pow = function
  | (s,0) -> ""
  | (s,n) -> s + pow(s,n-1)

// 17.2
let rec isIthChar = function
  | (s,n,c) when n < 0 -> false
  | (s,n,c) when ((String.length s) <= n) -> false
  | (s,n,c) -> (string s).[n] = c

// 17.3
let rec occFromIth (s,n,c) =
  let sub = (string s).[n..]
  let rec iter = function
    | (s,n) when ((String.length s) = 0) -> n
    | (s,n) when s.[0] <> c -> iter(s.[1..],n)
    | (s,n) when s.[0] = c -> iter(s.[1..],(n+1))
    | _ -> 0
 
  iter(sub,0)
 