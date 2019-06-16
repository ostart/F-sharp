// 43.3
let try_find key m =
  let list = Map.toList m
  let rec iter lst = 
    if(List.length lst = 0) then None
    else
      let (k,v) :: tail = lst
      if(k=key) then Some v
      else iter tail
  
  iter list

