        
    let rec bezout a b =
    if b = 0 then (a, 1, 1)  (* a = 1*a + 1*b *)
    else let d, u', v' = bezout b (a mod b) in
         d, v', u' - v'*(a/b);
         
