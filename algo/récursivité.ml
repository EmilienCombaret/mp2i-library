
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

let rec fact n = 
    if n = 0 then 1
    else n*fact (n - 1);;

let rec taille l = match l with
    | [] -> 0
    | e::q -> 1 + taille q;;

let rec somme = function
    | [] -> 0.
    | e::q -> e +. somme q;;

let moyenne l = (somme l) /. (float_of_int (taille l));;

let rec oppose = function
    | [] -> []
    | e::q -> (-e)::oppose q;;

let rec positif = function
    | [] -> true
    | e::q -> e >= 0 && positif q;;

let rec dernier = function
    | [] -> failwith "Pas de dernier élément"
    | [e] -> e
    | _::q -> dernier q

let premier n =
    let rec aux d = (* teste s'il existe un d < n divisant n *)
        if d = 1 then false (* on n'a pas trouvé de diviseur *)
        else n mod d = 0 || aux (d - 1) in
    aux (n/2)
    
    let tous_premiers n =
    if n = 1 then []
    else let l = tous_premiers (n - 1) in
         if premier n then n::l
         else l 
        
    let rec bezout a b =
    if b = 0 then (a, 1, 1)  (* a = 1*a + 1*b *)
    else let d, u', v' = bezout b (a mod b) in
         d, v', u' - v'*(a/b) in

let rec fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 -> if e1 < e2 then e1::fusion q1 l2
                        else e2::fusion l1 q2 in
fusion [2; 5; 8] [3; 4; 10];;

let rec kfusion ll = match ll with
    | [] -> []
    | l::q -> fusion l (kfusion q) in
kfusion [[2; 5; 8]; [3; 4; 10]; [0; 13]];;

