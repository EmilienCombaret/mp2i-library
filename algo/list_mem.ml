List.mem;;

let rec mem l e = match l with
    | [] -> false
    | x::q -> x = e || mem q e;;
    
let l = [1; 5; 2] in
assert ((mem l 5) && not (mem l 3))

let rec iter f l = match l with
    | [] -> ()
    | e::q -> f e; iter f q
    
List.filter (fun x -> x > 0) [1; -2; 3; 0];;

let rec filter f l = match l with
    | [] -> []
    | e::q -> if f e then e::filter f q else filter f q;;
filter (fun x -> x > 0) [1; -2; 3; 0];;

let rec somme = function 
    | [] -> 0
    | e::q -> e + somme q;;

let rec range n = 
    if n = 0 then [0]
    else n::range (n - 1);;
    
List.map;;

let rec map f l = match l with
    | [] -> []
    | e::q -> f e::map f q;;

somme (map (fun x -> x*x*x*x) (range 10))

let positif = List.for_all (fun x -> x >= 0);;
assert ((positif [3; 2; 9]) && not (positif [2; -2; 9]))

let pair = List.exists (fun x -> x mod 2 = 0);;
assert ((pair [3; 2; 9]) && not (pair [3; 1; 9]))
