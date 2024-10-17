open Int64;;

(*
Module pour representer un entier supérieur à 64 bits avec 
grande précision  pour cela on utilise une liste d'entiers 
64 bits du module Int64
*)
module BigInt = struct

    (*Structure de donnée du type big_int*)
    type big_int = int64 list
  
    (*
    .Prend une liste de bits représenté en boolean et un entier naturel
    
    .Renvoie une liste tronquée ne contenant que ses n premiers 
    éléments, soit la liste complétée à droite, de taille n, 
    complétée par des valeurs false
       
    *)
    let completion (lst:bool list) (n:int) : bool list =
      let rec aux (lst:bool list) (acc:bool list) (n:int) : bool list =
        match lst with
        | [] -> if n <= 0 then acc else aux [] (acc @ [false]) (n-1)
        | h::t -> if n=0 then acc else aux t (acc @ [h]) (n-1)
      in aux lst [] n;;

    (*
    .Prend une liste de bits représenté en boolean 
    .Renvoie une liste tronquée contenant une taille égale 
    à une puissance de 2 supérieur ou égale à la taille de 
    base de cette liste. 
    *)
    let auto_completion (lst:bool list) : bool list =
      let len = (List.length lst) in 
      let rec find_puissance (p:int) (n:int):int=
        if p >= n then p
        else find_puissance (p * 2) n
      in (completion lst (find_puissance 1 len));;

    (*
    .Prend en parametre un entier de type big_int
    .Renvoie une liste de bits représenté en boolean
    *)
    let rec decomposition(lst: big_int): bool list =
      let rec aux (acc:bool list) (lst:int64): bool list  =
        if lst = 0L then acc
        else 
          let acc = if (logand lst 1L) = 1L then acc @ [true]
          else acc@[false] in aux acc (shift_right_logical lst 1)
      in match lst with
      |[]->[]
      |[x]->(aux [] x)
      |x::xs->(completion (aux [] x) 64)@(decomposition xs);;

    (*
    .Prend en parametre une liste de bits représenté en boolean
    .Renvoie un entier de type big_int
    *)
    let composition(lst: bool list): big_int=
      let rec aux (lst:bool list) (acc:int64) (n:int64) (iter:int): int64 list =
        match lst with
        | [] -> [acc]
        | x::xs -> if iter=64 then acc::(aux xs 0L 1L 1)
        else if x then (aux xs (add acc n) (shift_left n 1) (iter+1))
        else (aux xs acc (shift_left n 1) (iter+1))
      in aux lst 0L 1L 1;;

    (*
    .Prend en parametre un entier x et un entier n
    .Renvoie une liste de bits représenté en boolean de 
    l'entier x représenté en binaire sur n bits
    *)
    let table (x:big_int) (n:int): bool list =
      (completion (decomposition x) n);;

    (*
    .Prend en parametre un entier n
    .Renvoie un entier aléatoire de type big_int sur n bits
    *)
    let gen_alea (n : int) : big_int = 
      let rec aux (n : int) (acc : big_int) = 
        if n <= 1 then acc else (aux (n-1) (Random.bits64()::acc))
      in let l = n/64 in let binf = n mod 64 in (aux l [ (Int64.shift_right_logical (Random.bits64()) (64-binf)) ]);;  
      
  end;;
