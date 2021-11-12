let f x y = x**y
let g x y = x (y + 1)
let h x y z = x (3. *. (y z)) + 1

let tanh x =
    let e = exp x in
    (e -. 1./.e)/.(e +. 1./.e);;
tanh 1. (* test *)

let p = (2.1, 3.7) in (* exemple de point *)
let x, y = p in (* récupération des coordonnées *)
x;; (* affichage de x *)

let aire_boule r = 4.*.Float.pi*.r**3./.3. in
aire_boule 1.

let distance (x1, y1) (x2, y2) = (* on peut aussi décomposer un couple dans l'argument *)
    ((x1 -. x2)**2. +. (y1 -. y2)**2.)**0.5;;
distance (0., 0.) (1., 1.)

let polaire (r, theta) = r*.cos theta, r*.sin theta in
polaire (1., Float.pi/.3.);;
3.**0.5/.2.

let milieu (x1, y1) (x2, y2) =
    (x1 +. x2)/.2., (y1 +. y2)/.2. in  (* attention : c'est un + et pas - *)
milieu (0., 0.) (1., 1.)

(* penser à réutiliser la fonction distance précédente *)
let parallelogramme p1 p2 p3 p4 = 
    abs_float ((distance p1 p2) -. (distance p3 p4)) < 0.001 &&
    abs_float ((distance p1 p4) -. (distance p3 p2)) < 0.001;;
parallelogramme (0., 0.) (1., 0.) (2., 1.) (1., 1.);;
parallelogramme (0., 0.) (1., 0.) (2., 1.) (1.5, 1.);;

let rec rand6 () = 
    let r = 5*Random.int 5 + Random.int 5 in
    if r < 7 then r (* on renvoie le résultat s'il est entre 0 et 6 *)
    else rand6 ()  (* sinon on génère à nouveau *)

let count = Array.make 7 0 in
for _ = 0 to 10000 do
    let r = rand6 () in
    count.(r) <- count.(r) + 1
done;
count (* tire 10000 entiers au hasard pour vérifier que la répartition est uniforme *)
