open Big_int.BigInt;;;;
let rec string_of_bool lst = match lst with
  |[]-> "\n"
  |x::xs -> let b = if x then "true" else "false" in
            b ^ " " ^ string_of_bool xs;;

let () = 
  (* On test pour 38 et 2¹⁰⁰ *)
  let lst =[|[38L];[0L;68719476736L]|] in 
  let n = Array.length lst in
  for i = 0 to n - 1 do
    let decomp = decomposition lst.(i) in 
    print_string ( 
    (string_of_int(List.length decomp))^" bits:\n" 
    ^"Liste : "^(string_of_bool (decomp))^"\n")
  done;;
