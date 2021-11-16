
let divise a b =
    b mod a = 0;;
let () = assert (divise 14 42 && not (divise 3 7));; 

let rec somme n = 
    if n = 0 then 0
    else n*n + somme (n - 1);;
let () = assert (somme 3 = 1 + 4 + 9)

let rec u n =
    if n = 0 then 42. (* doit renvoyer un float à cause de la racine *)
    else 3.*.(u (n - 1))**0.5 +. 2.;;

let rec u a n =
    if n = 0 then a
    else let un = u a (n - 1) in
    (un +. a/.un)/.2.;;
u 9. 5;;
u 25. 5;;

(* la limite de u a est racine de a (il s'agit de la suite de Héron) *)

let rec euclide a b = 
    if b = 0 then a  (* le PGCD est le dernier reste non nul *)
    else euclide b (a mod b);;
let () = assert (euclide 21 6 = 3)
