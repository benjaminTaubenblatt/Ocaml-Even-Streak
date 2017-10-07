let even (n : int) : bool = n mod 2 = 0

let even_streak (l : int list) : int =
  let rec count_evens (l, best, curr): int = (* local recursive function *)
    match l with
    | [] -> if best > 0 then best else 0 (* when list is empty at end return best or the longest is 0 *)
    | hd :: tl -> match even hd with (* pattern matching for evens and odds *)
      | true -> if best <= curr then count_evens (tl, best+1, curr+1) else count_evens (tl, best, curr+1)
      (* conditions for incrementing best and curr
       to store our longest sequence *)
      | false -> if best <= curr then count_evens (tl, curr, 0) else count_evens (tl, best, 0)
      (* check if we have a best  *)
  in
  count_evens (l, 0, 0) (* recursive call *)


let ex_2 = even_streak [7; 2; 4; 6; 3; 4; 2; 1]
