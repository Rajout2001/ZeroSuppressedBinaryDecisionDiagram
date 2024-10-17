open Big_int.BigInt;;

module DecisionTree = struct 

  (*QStrucutre de donnée de l'arbre de decision*)
  type btree = 
  Leaf of bool |
  Node of btree * int * btree;;

  (*
  .Prend une liste "lst" de bits représenter en boolean et un entier n
  .Retourne une liste de bits représenter en boolean qui prend la sous liste de droite de l'indice n  de la liste lst
  *)
  let rec sub_list (lst:bool list) (n:int):bool list=
    match lst with
    | [] -> []
    | hd::tl -> if n<=1 then tl else (sub_list tl (n-1));;

  (* construction d'un arbre de decision a partir d'une liste de bool*)
  let cons_arbre(lst:bool list):btree=
    let rec aux (lst:bool list) (depth:int):btree=
      match lst with
      | [] -> failwith "Arbre vide" (*Erreur si la racine de la fonction est un arbre vide sinon on l'auto_complete donc pas d'erreur*)
      | ng::nd::[] -> Node(Leaf ng, depth, Leaf nd) (*Si la liste est de taille 2 alors on a une feuille gauche et une feuille droite*)
      | tl -> let n_leaf = (List.length lst)/2  in
                      let ssg = aux (completion tl n_leaf) (depth+1) in 
                      let ssd = aux (sub_list tl n_leaf) (depth+1) in 
                      Node(ssg, depth, ssd)
  in aux (auto_completion lst) 1 ;;

  (*
  .Prend en parametre l'arbre de decision
  .Retourne une liste de bits représenter en boolean 
  qui sont les feuilles de l'arbre de decision dans 
  l'ordre croissant
  *)
  let rec liste_feuilles (bt:btree):bool list=
    let rec aux (bt:btree) (acc:bool list):bool list=
      match bt with
      | Leaf x -> acc@[x]
      | Node(g,x,d)-> aux g acc @ aux d acc
    in aux bt [];;

end;;
