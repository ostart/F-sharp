// 40.1
let rec sum (p, xs) =
  let rec iter lst acc =
    let head :: tail = lst
    if(tail.Length = 0) then 
          if(p head) then head+acc
          else acc
    else
      if(p head) then iter tail (head+acc)
      else iter tail acc
  
  iter xs 0

// 40.2.1
let rec count (xs, n) =
  let rec iter lst acc =
    let head :: tail = lst
    if(tail.Length = 0) then 
          if(head=n) then acc+1
          else acc
    else
      if(head=n) then iter tail (acc+1)
      elif(head>n) then acc
      else iter tail acc
  
  iter xs 0

// 40.2.2
let rec insert (xs, n) =
  let rec iter lst acc =
    let head :: tail = lst
    if(tail.Length = 0) then 
      if(head>n) then (acc@[n;head])
      else (acc@[head;n])
    else
      if(head>n) then (acc@[n]@lst)
      else iter tail (acc@[head])
  
  iter xs []

// 40.2.3
let rec intersect (xs1, xs2) = ...

// 40.2.4
let rec plus (xs1, xs2) = ...

// 40.2.5
let rec minus (xs1, xs2) = ...

// 40.3.1
let rec smallest = ...

// 40.3.2
let rec delete (n, xs) = ...

// 40.3.3
let rec sort = ...

// 40.4
let rec revrev = ...