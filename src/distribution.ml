open Big_int.BigInt;;
open Decision_tree.DecisionTree;;
open Deja_vu;;
open Zdd;;

(*Question 21*)
(* Calcul de la taille d'un arbre de décision en entier*)
let rec tree_size (tree: btree): int =
  match tree with
  | Leaf _ -> 1
  | Node (left, _, right) -> 1 + tree_size left + tree_size right

(* Fonction qui génére une table de vérité aléatoire et construit le ZDD puis va collecter les tailles*)
let aux_distribution (n: int) (acc: int): int array =
  let sizes = Array.make (n * 2) 0 in
  for i = 1 to acc do
    let size = (tree_size (compressionParListe (cons_arbre (table (gen_alea n) n))))-1 in
    sizes.(size - 1) <- sizes.(size - 1) + 1
  done;
  sizes;;

Random.self_init;;

(* Fonction pour afficher la distribution de probabilité dans un fichier *)
let distribution (n: int) (acc: int) =
  let sizes = aux_distribution n acc in
  let file = open_out "distribution.txt" in
  for i = 0 to Array.length sizes-1 do
    let probability = float_of_int sizes.(i) /. float_of_int acc in
    Printf.fprintf file "%d\t%f\n" (i + 1) probability
  done;
  close_out file

let () = distribution 128 500