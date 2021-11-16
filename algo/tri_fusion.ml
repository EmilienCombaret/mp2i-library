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
