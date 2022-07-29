type tree =
  | Empty 
  | Node of int * tree * tree
                            
type command = Left | Right | Up | New of int | Delete | Push | Pop


let rec crawl command_list tree=
  let rec crawl' command_list' tree stack st command_stack newdel upcount =
    
  
    let rt st command_stack newdel  = match st, command_stack, newdel with 
        x::xs, h::t, a::ab   -> match x, h, a with 
          Node(v,l,r), Left, a  -> Node(v,a,r)
        |Node(v,l,r), Right, a  -> Node(v,l,a)
                                     
    in
    
    let left  = function
      | Node(_,l,r)  -> l
      | Empty -> Empty
    in

    let right  = function
      | Node(_,l,r) -> r
      | Empty -> Empty
    in


    let new_x x = function 
      | Node(_,l,r) -> Node(x,Empty,Empty)
      | Empty -> Empty
    in

    let delete = function
      | Node(_,_,_) -> Empty
      | Empty -> Empty
    in

    let push tree stack = tree::stack
    in


    let pop  = function
      | x::xs -> x
      | _ -> failwith ("error")
    in

    let pop_remove = function
      | x::xs -> xs    (*remove from stack*)
      | _ -> failwith("error")
           
    in 
    match command_list' with
      [] -> 
        if List.length (command_stack) = 0 && List.length (upcount)=0  then tree else 
          crawl' [] (rt st command_stack (push tree newdel)) (stack) (pop_remove st) (pop_remove command_stack)(newdel)(pop_remove upcount)
          
    | x::xs -> match x with
        Left -> crawl' xs (left tree) stack (push tree st ) (push Left command_stack) (newdel)(push Up upcount)
      | Right -> crawl' xs (right tree) stack (push tree st ) (push Right command_stack)(newdel)(push Up upcount)
      | New(integer) -> crawl' xs (new_x(integer) tree) (stack) (st) (command_stack)(newdel) (upcount)
      | Delete -> crawl' xs (delete tree) stack st (command_stack)(push tree newdel)(upcount)
      | Push -> crawl' xs  tree (push tree stack) st (command_stack)(newdel)(upcount)
      | Pop -> crawl' xs (pop stack) (pop_remove stack ) st (command_stack)(newdel)(upcount)
      | Up -> crawl' xs (rt st command_stack (push tree newdel)) (stack) (pop_remove st) (pop_remove command_stack)(newdel)(pop_remove upcount)


  in crawl' command_list tree [] [] [] [] [];;
;;
    
