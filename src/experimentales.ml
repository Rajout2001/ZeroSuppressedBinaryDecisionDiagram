open Big_int.BigInt;;
open Decision_tree.DecisionTree;;
open Deja_vu;;
open Zdd;;
open Sys;;

(*Question 20*)

(*Calcul de la taille d'un arbre de décision en float*)
let rec tree_size (tree: btree): float =
  match tree with
  | Leaf _ -> 0.0
  | Node (left, _, right) -> 1.0 +. tree_size left +. tree_size right
;;

(*Enregistre le temps au début et à la fin puis renvoie la différence pour mesurer le temps écoulé.*)
let diff_temps f =
  let debut = time() in
  let _ = f in
  let fin = time() in
  fin -. debut
;;

(*Sert à générer une liste pour incrémenter dans notre fichier txt*)
let generation_liste (n : int) : int list =
  let rec aux i acc =
    if i <= n then
      aux (i + 1) (i :: acc)
    else
      List.rev acc
  in aux 1 []

(*Sert à écrire les données dans les fichiers*)
let rec ecriture_donnees file taille l1 l2 =
    match (taille, l1, l2) with
    |([], [], [])->()
    |(increment::taille1, x1::list1, x2::list2) ->
        Printf.fprintf file "%d %f %f\n" increment x1 x2 ;
        ecriture_donnees file taille1 list1 list2 ;
    | _ -> failwith "Erreur de taille de liste" 

let compression_test algo1 algo2 n max =
  let rec loop bits acc =
    if bits >= max then List.rev acc
    else
      let total =
        let rec inner nb_test sum =
          if nb_test >= n then sum
          else
            let tree = cons_arbre (table (gen_alea bits) bits) in
            let my_data = algo1 (algo2 tree) in
            inner (nb_test + 1) (sum +. my_data)
        in
        inner 0 0.0
      in
      loop (bits * 2) (total :: acc)
  in
  loop 2 []

(*Appplication*)
let () = 
  let file = open_out "comparaison_vitesse.txt" in
  let l1 = (compression_test diff_temps compressionParArbre 400 2000) in
  let l2 = (compression_test diff_temps compressionParListe 400 2000) in
  let taille = generation_liste (List.length l2) in
    ecriture_donnees file taille l1 l2;
    close_out file;;

let () = 
  let file = open_out "taille_moy.txt" in
  let l1 = (compression_test tree_size compressionParArbre 100 512) in
  let l2 = (compression_test tree_size compressionParListe 100 512) in
  let taille = generation_liste (List.length l2) in
    ecriture_donnees file taille l1 l2;
    close_out file;;