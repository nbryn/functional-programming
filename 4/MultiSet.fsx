module MultiSet
    type MultiSet<'a when 'a : comparison> = M of Map<'a, uint32>

    let empty = M (Map.empty)

    let size (M s) = Map.toList s |> List.fold (fun acc ele -> acc + snd ele) (0u)

    let isEmpty (M s) = size (M s) = 0u

    let contains a (M s) = Map.containsKey a s

    let numItems a (M s) = if contains a (M s) then Map.find a s else (0u) 

    let add a n (M s) = if contains a (M s) 
                        then let currVal = Map.find a s
                             Map.remove a s |> Map.add a (currVal + n) |> M
                        else M (Map.add a n s)

    let addSingle a (M s) = add a (uint32 1) (M s)
    
 (*    if contains a (M s)
                            then let currVal = Map.find a s
                                 Map.remove a s |> Map.add a (currVal + 1u) |> M
                            else add a 1u (M s) *)
    

    let remove a (n : uint32) (M s) = Map.find a s |> fun x -> let newVal = (int (x - n))
                                                               if newVal <= 0 then M (Map.remove a s)
                                                               else Map.remove a s |> fun x -> add a (uint32 newVal) (M x)
                                                               
    let removeSingle a (M s) = remove a (uint32 1) (M s)
    
    
(*     if contains a (M s)
                               then (Map.find a s) - 1u |> fun x -> let t = Map.remove a s
                                                                    add a x (M t)
                               else (M s) *)
                               
    let fold f acc (M s) = Map.toList s |> List.fold (fun acc ele -> f acc (fst ele) (snd ele)) acc  

    let foldBack f (M s) acc = Map.toList s |> fun x -> List.foldBack (fun ele acc -> f (fst ele) (snd ele) acc) x acc   

    let map f (M s) = Map.toList s |> List.map (fun x -> (f (fst x), snd x)) |> List.fold (fun acc ele -> add (fst ele) (snd ele) acc) empty

    let ofList (lst : 'a List) = List.fold (fun acc ele -> addSingle ele acc) empty lst
                                 
    let toList (M s) = Map.toList s |> List.fold (fun acc ele -> List.replicate (int (snd ele)) (fst ele) :: acc) [] |> List.concat |> List.rev     

    let union (M s1) (M s2) = let biggest = if (size (M s1)) > (size (M s2)) then (M s1) else (M s2) 
                              let smallest = if (size (M s1)) < (size (M s2)) then s1 else s2                                                 
                              fold (fun set ele count -> if contains ele (M smallest)
                                                         then Map.find ele smallest |> fun x -> if x > count
                                                                                                then add ele x set
                                                                                                else add ele count set
                                                         else add ele count set
                              ) empty biggest
    
    let sum (M s1) (M s2) = let biggest = if (size (M s1)) > (size (M s2)) then (M s1) else (M s2) 
                            let smallest = if (size (M s1)) < (size (M s2)) then s1 else s2                                                 
                            fold (fun set ele count -> if contains ele (M smallest)
                                                       then Map.find ele smallest |> fun x -> add ele (x + count) set                                                                                             
                                                       else add ele count set
                            ) empty biggest
    
   
    let subtract (M s1) (M s2) = fold (fun set ele count -> if contains ele (M s2)
                                                            then Map.find ele s2 |> fun x -> if contains ele set
                                                                                             then remove ele count set
                                                                                             else add ele (count - x) set                                                                                             
                                                            else add ele count set
                                       ) empty (M s1)
                                            
                                                                                
                                 

    let intersection (M s1) (M s2) = let biggest = if (size (M s1)) > (size (M s2)) then (M s1) else (M s2) 
                                     let smallest = if (size (M s1)) < (size (M s2)) then s1 else s2                                                 
                                     fold (fun set ele _ -> if contains ele (M smallest)
                                                                then Map.find ele smallest |> fun x -> add ele x set                                                                                             
                                                                else set
                                          ) empty biggest


    