open Decision_tree.DecisionTree;;
open Big_int.BigInt;;
open Dot.Dot;;

let () =
  let arbre = cons_arbre(table [25899L] 16)
  in
  generer_arbre arbre "decision_tree";;
