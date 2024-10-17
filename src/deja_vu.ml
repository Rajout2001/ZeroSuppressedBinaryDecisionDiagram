open Big_int.BigInt;;
open Decision_tree.DecisionTree;;

(*Module Deja_vu qui est utiliser pour la compression zdd*)
module type DejaVu = sig

  (*
  Structure de donnée permettant de stocké une structure de donnée 
  quelconque (par exemple une table de hachage ou un arbre binaire) 
  selon un pointeur d'un arbre de decision   
  *)
  type deja_vu 

  (*
    fonction permettant soit d'inserer un pointeur
    dans la structure de donnée soit de retourner
    ce pointeur si il est deja present
  *)
  val find : deja_vu -> btree -> btree*deja_vu

  (*Utile pour initialiser la structure*)
  val empty : deja_vu
end;;

(*Foncteur utilisant comme structure de donnée 
   une table de hachage ou liste*)
module ListeDejaVu : DejaVu = struct
  
  (*big_int est la clée pour retrouver le pointeur*)
  type deja_vu = (big_int*btree) list

  (*fonction permettant soit d'inserer un pointeur
    dans la table de hachage soit de retourner
    ce pointeur si il est deja present 
  *)
  let rec find (lst:deja_vu)(bt:btree):(btree*deja_vu) = 
    let l = composition (liste_feuilles bt) in 
    match lst with
    | [] -> bt,[(l,bt)]
    | x::xs -> let bi,bt2=x in 
               if bi=l && bt=bt2 then bt2,lst
               else let i,j=(find xs bt) in i,x::j

  (*L'element vide est une liste vide*)
  let empty = []

end;;

(*
  Foncteur utilisant comme structure de donnée 
  un arbre binaire 
*)
module ArbreDejaVu : DejaVu = struct

  (*
  Structure de donnée arborescente permettant de savoir ou se trouve le
  pointeur dans l'arbre binaire en effectuant un parcours
  en profondeur où l'arete gauche est associé à false et l'arete
  droite à true.
  *)
  type deja_vu = 
  | Leaf  
  | Node of deja_vu*btree option*deja_vu

  (*
  Ca permet de creer un arbre si l'arbre btree n'existe pas
  *)
  let rec creer_abr (leaf:bool list)(bt:btree):deja_vu=
  match leaf with
    | [] -> Node (Leaf ,Some bt,Leaf )
    |x::xs -> if (List.hd leaf)=true then  Node(Leaf ,None,creer_abr xs bt)
    else Node((creer_abr xs bt),None,Leaf )

  
  (*fonction permettant soit d'inserer un pointeur
    dans la structure arborescente soit de retourner
    ce pointeur si il est deja present 
  *)    
  let find (lst:deja_vu)(bt:btree):(btree*deja_vu) = 
    let rec aux (l:bool list)(lst:deja_vu)(bt:btree):(btree*deja_vu) =
      match lst with
      | Leaf -> bt,creer_abr l bt
      | Node (a,b,c) -> if l=[] then 
                          match b with 
                          | None-> bt,Node(a,Some bt,c)
                          | Some bt2 -> if bt=bt2 then bt2,lst else bt,lst
                        else if (List.hd l)=true then let i,j=(aux (List.tl l) c bt) in i,Node(a,b,j)
                        else let i,j=(aux (List.tl l) a bt) in i,Node(j,b,c)
      in aux (liste_feuilles bt) lst bt

  (*L'element vide est une feuille*)
  let empty = Leaf

end;;
