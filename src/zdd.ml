open Decision_tree.DecisionTree;;
open Big_int.BigInt;;
open Deja_vu;;


(*Structure Zdd prenant un module DejaVu (cf fichier deja_vu pour l'utilité)*)
module Zdd (DV: DejaVu) = struct

    (*
    Recupere la liste des feuilles d'un arbre de decision
    et ayant comme taille une puissance de 2
    *)
    let recuperation_liste_feuilles(g:btree):bool list = 
      auto_completion (liste_feuilles g);;

    (*
    Retourne vrai si la seconde moitie de la liste est fausse
    *)
    let seconde_moitie_false (lst:bool list):bool =
      let len = (List.length lst) in
      let rec aux (lst:bool list) (i:int) (n:int):bool =
          if  (List.nth lst i)=true then false
          else if i=n then true
          else aux lst (i-1) n
        in aux lst (len-1) (len/2);;

    (*
    Compression d'un arbre de decision selon l'algorithme ZDD
    et selong la structure deja vu DV.
    Voilà les règles de compression de l’arbre :
      — règle-M : Si deux nœuds M et N sont les racines de sous-arbres ayant le même résultat pour
      liste_feuilles, alors les arêtes pointant vers N sont remplacées par des arêtes pointant vers
      M dans toute la structure ; puis le nœud N est supprimé de la structure.
      — règle-Z : si l’enfant droit de N pointe vers false, alors toutes les arêtes pointant vers N sont
      remplacées par des arêtes pointant vers l’enfant gauche de N ; puis le nœud N est supprimé de
      la structure.
    *)
    let compression (g:btree):btree=
      let rec aux (g:btree)(lst:DV.deja_vu):btree*DV.deja_vu=
        match g with
        | Leaf x -> (DV.find lst g) (*regle M*)
        | Node (xg,x,xd) -> let rlf = (recuperation_liste_feuilles g) in 
                            let smf = (seconde_moitie_false rlf) in
                            if smf=true then (aux xg lst) (*regle Z*)
                            else let btl,lstl= (aux xg lst)  in 
                            let btr,lstr=(aux xd lstl) in
                            (DV.find lstr (Node(btl,x,btr))) (*regle M*)
      in let x,y= (aux g (DV.empty)) in x;;
  end;; 

module LST = Zdd(ListeDejaVu);;
let compressionParListe = LST.compression;;

module ABR = Zdd(ArbreDejaVu);;
let compressionParArbre = ABR.compression;;