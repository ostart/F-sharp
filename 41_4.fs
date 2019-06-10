// 41.4.1
let list_filter f xs =
  let func i acc = if(f i) then (i::acc) else acc 
  List.foldBack func xs []

// 41.4.2
let sum (p, xs) =
  let func acc i = if(p i) then (acc+i) else acc
  List.fold func 0 xs


// 41.4.3
let revrev = fun list ->
  List.fold (fun head tail -> (List.rev tail)::head) [] list