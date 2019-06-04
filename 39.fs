// 39.1
let rec rmodd =  fun list ->
    let rec iter lst acc counter = 
        let head :: tail = lst;
        if(tail.Length = 0) then 
          if(counter % 2 = 1) then head::acc
          else acc
        else
          if(counter % 2 = 1) then iter tail (head::acc) (counter+1)
          else iter tail acc (counter+1)
          
    List.rev(iter list [] 0)
         

// 39.2
let rec del_even = fun list ->
    let rec iter lst acc = 
        let head :: tail = lst;
        if(tail.Length = 0) then 
          if(head % 2 = 1) then head::acc
          else acc
        else
          if(head % 2 = 1) then iter tail (head::acc)
          else iter tail acc
    
    List.rev(iter list [])

// 39.3
let rec multiplicity x xs = 
    let rec iter lst acc =
        let head :: tail = lst;
        if(tail.Length = 0) then 
          if(head = x) then acc+1
          else acc
        else
          if(head = x) then iter tail (acc+1)
          else iter tail acc
    
    iter xs 0

// 39.4
let rec split = fun list ->
    let rec iter lst acc1 acc2 counter = 
        let head :: tail = lst;
        if(tail.Length = 0) then 
          if(counter % 2 = 1) then (acc1,(head::acc2))
          else ((head::acc1), acc2)
        else
          if(counter % 2 = 1) then iter tail acc1 (head::acc2) (counter+1)
          else iter tail (head::acc1) acc2 (counter+1)
          
    let (res1, res2) = iter list [] [] 0
    (List.rev res1, List.rev res2)

// 39.5
let rec zip (xs1,xs2) = 
  let rec iter (lst1:'T list) (lst2:'T list) acc =
    if (lst1.Length <> lst2.Length) then failwith "Not equal length of lists" 
    else
      let head1 :: tail1 = lst1;
      let head2 :: tail2 = lst2;
      if(tail1.Length = 0) then (head1, head2) :: acc
      else iter tail1 tail2 ((head1, head2) :: acc)

  
  List.rev (iter xs1 xs2 [])
  
  
  
  
 

  

  