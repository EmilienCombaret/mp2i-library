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
        else n mod d = 0 || aux (d - 1);

    let tous_premiers n =
    if n = 1 then []
    else let l = tous_premiers (n - 1) in
         if premier n then n::l
         else l 
