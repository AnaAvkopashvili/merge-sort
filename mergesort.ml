let init list = List.map (fun x -> [x]) list

let rec merge (l1, l2) =
  match (l1,l2) with
  |([],[]) -> []
  |(l1, []) -> l1
  |([], l2) -> l2
  |(h1 :: t1, h2 :: t2) -> if h1 < h2 then h1 :: merge (t1, l2) else h2 :: merge (l1, t2);;

let rec merge_list list = 
  match list with
  |[] -> []
  |[x] -> [x]
  |l1 :: l2 :: t -> merge (l1,l2) :: merge_list t

  

  (*solution 2*)

  let rec merge lst1 lst2 = match lst1,lst2 with
  |[],lst2->lst2
  |lst1,[]->lst1
  |h1::tail1,h2::tail2->if h1<h2 then h1::(merge tail1 (h2::tail2)) else h2::(merge (h1::tail1) tail2);;

(*[1;3;7] [4;6] -> 1:: merge [3;7] [4;6] ->1::3:: merge [7] [4;6] -> 1::3::4 merge [7] [6]-> 1::3::4::6::7::[] *)

let split list =
  let rec split_aux1 num lst1 lst2  = if List.length lst1 = num then (List.rev lst1,lst2) else
      match lst1, lst2 with
      |[],[]->(List.rev lst1,lst2)
      |[],h2::tail-> split_aux1 num [h2] tail 
      |h1::tail1,h2::tail2 ->split_aux1 num (h2::h1::tail1) tail2 
      |_->assert false
  in
  split_aux1 ((List.length list)/2) [] list ;;


(* [1;2;3;4]  f 2 [] [1;2;3;4] *)

(* [] [1;4;5;6] -> split_aux1 2 [1] [4;5;6]-> split_aux1 2 [4;1] [5;6]->([1;4],[5;6])*)
(*split [1;4;5;6] -> split_aux1 2 [] [1;4;5;6]-> split_aux1 2 [1] [4;5;6]-> split_aux1 2 [4;1] [5;6] -> ([1;4],[5;6]) *)

let rec mergesort lst = match lst,split lst with
  |[],_->[]
  |[x],_->[x]
  |lst,(lst1,lst2)->merge (mergesort lst1) (mergesort lst2);;

(* mergesort [5;3;4;7]-> merge mergesort [5;3] mergesort [4;7]-> merge mergesort [5] mergesort [3] -> [3;5] 
merge mergesort [4] mergesort [7] -> merge [3;5] [4;7]-> [3;4;5;7] *)


(*[5;3;4;7;6]-> [5;3] [4;7;6]-> [5] [3] [4] [7;6]->[5] [3] [4] [7] [6]->[3;5] [4;7] [6] -> [3;5] [4;6;7] ->[3;4;5;6;7]*)

