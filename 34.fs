let rec iterup x acc =
  if (x = 0) then acc
  else iterup (x-1) (x::acc)

let rec itereven x acc = 
  if (x=0) then (x::acc)
  elif (x%2=0) then itereven (x-1) (x::acc)
  else itereven (x-1) (acc)

// // 34.1
let rec upto = function 
  | n -> iterup n []

// // 34.2
let rec dnto = function
   | 0 -> []
   | n -> n :: dnto(n-1)

// // 34.3
let rec evenn = function
  | n -> itereven (2*n-1) []