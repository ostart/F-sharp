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
let rec intersect (xs1, xs2) =
  let rec iter lst1 lst2 acc =
    let head1 :: tail1 = lst1
    let head2 :: tail2 = lst2
    if(tail1.Length = 0 || tail2.Length = 0) then 
      if(head1=head2) then (acc@[head1])
      else acc
    else
      if(head1>head2) then iter lst1 tail2 acc
      elif(head1<head2) then iter tail1 lst2 acc 
      else iter tail1 tail2 (acc@[head1])

  iter xs1 xs2 []

// 40.2.4
let rec plus (xs1, xs2) =
  let rec iter lst1 lst2 acc =
    let head1 :: tail1 = lst1
    let head2 :: tail2 = lst2
    if(tail1.Length = 0 && tail2.Length = 0) then
      if(head1>head2) then acc@[head2;head1]
      elif(head1<head2) then acc@[head1;head2]
      else acc@[head1;head2]
    elif(tail1.Length = 0) then
      if(head1>head2) then iter lst1 tail2 (acc@[head2])
      elif(head1<head2) then acc@[head1]@lst2
      else acc@[head1;head2]@tail2
    elif(tail2.Length = 0) then
      if(head1>head2) then acc@[head2]@lst1
      elif(head1<head2) then iter tail1 lst2 (acc@[head1]) 
      else acc@[head1;head2]@tail1
    else
      if(head1>head2) then iter lst1 tail2 (acc@[head2])
      elif(head1<head2) then iter tail1 lst2 (acc@[head1]) 
      else iter tail1 tail2 (acc@[head1;head2])

  iter xs1 xs2 []

// 40.2.5
let rec minus (xs1, xs2) =
  let rec iter lst1 lst2 acc =
    let head1 :: tail1 = lst1
    let head2 :: tail2 = lst2
    if(tail1.Length = 0) then
      if(head1=head2) then acc
      else acc@[head1]
    elif(tail2.Length = 0) then
      if(head1=head2) then acc@tail1
      else iter tail1 lst2 (acc@[head1])
    else
      if(head1<head2) then iter tail1 lst2 (acc@[head1])  
      elif(head1>head2) then iter lst1 tail2 acc
      else iter tail1 tail2 acc

  iter xs1 xs2 []

// 40.3.1
let rec smallest = fun xs ->
  let seed :: rest = xs
  let rec iter lst acc =
    let head :: tail = lst
    if(tail.Length = 0) then
      if (head<acc) then Some head
      else Some acc
    else
      if (head<acc) then iter tail head
      else iter tail acc

  iter xs seed

// 40.3.2
let rec delete (n, xs) =
  let rec iter lst acc =
    let head :: tail = lst
    if(tail.Length = 0) then
      if(head=n) then acc
      else acc@[head]
    else
      if(head=n) then acc@tail
      else iter tail (acc@[head])
  
  iter xs []

// 40.3.3
let rec sort = fun xs ->
  let rec iter (lst:'T list) acc =
    if(lst.Length=0) then acc
    else
      let min = smallest lst
      let tail = delete (min.Value, lst)
      iter tail (acc@[min.Value])
  
  iter xs []


// 40.4
let rec revrev = fun xs ->
  let rec iter lst acc =
    let head :: tail = lst
    if(tail.Length = 0) then (List.rev head)::acc
    else iter tail ((List.rev head)::acc)
  
  iter xs []