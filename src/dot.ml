open Decision_tree.DecisionTree;;
open Big_int.BigInt;;
open Printf;;

module Dot = struct
  (*Ajouter un noeud de l'arbre dans le fichier .dot *)
  let print_dot (f:out_channel) (g:btree) = 
    match g with 
    | Leaf (b) -> 
      Printf.fprintf f "\n%d [shape = box, label = %b];" (Obj.magic g) b
    | Node(fg,depth,fd) -> Printf.fprintf f "\n%d [label = %d];" (Obj.magic g) depth ;
      Printf.fprintf f "\n%d -> %d [style=dotted];" (Obj.magic g) (Obj.magic fg);
      Printf.fprintf f "\n%d -> %d;" (Obj.magic g) (Obj.magic fd);
  ;;

  (*
     Generer un arbre sous format .dot 
      @param g : arbre de decision
      @param name : nom du fichier .dot
    
  *)
  let generer_arbre (g:btree) (name:string)  : unit =
    let f = open_out ("./img/"^name^".dot") in 
      let rec print_graphe (graphe : btree ) (bordure : btree  list) (vus : btree  list): unit = 
        if (not (List.memq graphe vus)) then print_dot f graphe ;  
        match (graphe, bordure) with 
          | (Leaf(b),[])-> ()    
          | (Leaf(_),h::tl) -> (print_graphe h tl (graphe::vus))   
          | (Node(fg,_,fd),_) -> (print_graphe fg (fd::bordure) (graphe::vus)) 
        in 
          Printf.fprintf f "digraph %s{\n" name;   
          print_graphe g [] [] ; 
          Printf.fprintf f "}\n";
    close_out f;;

end;;