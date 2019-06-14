// 42.3
let rec allSubsets n k =
  let rec iter arr acc =
    if((List.length arr) = 1) then (arr::acc)
    else
      let filtered = (List.map (fun x -> List.filter (fun y -> y <> x) arr) arr)
      (List.map (fun x -> iter x (arr::acc)) filtered)
        |> Seq.concat
        |> List.ofSeq

  let setOfList = Set.ofList (iter [1..n] [])
  let setOfSet = Set.map (Set.ofList) setOfList
  Set.filter (fun x -> Set.count x = k) setOfSet
