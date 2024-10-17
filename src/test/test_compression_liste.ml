open Zdd;;
open Decision_tree.DecisionTree;;
open Big_int.BigInt;;
open Dot.Dot;;

let t1 = [38L];;
let t2 = [25899L;38L];;
let t3 = [259L;38L];;
let t4 = [259L;23L;566L];;
let t5 = [259L;23L;965L;44445L;12L;566L];;

let t6 = [25899L];;

let () =
  let entier = decomposition t6 in 
  let liste  = (cons_arbre entier) in 
  let compression = compressionParListe liste 
  in
   generer_arbre compression "compressionParListe";;
  