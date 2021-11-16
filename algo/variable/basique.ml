let tanh x =
    let e = exp x in
    (e -. 1./.e)/.(e +. 1./.e);;
tanh 1. (* test *)
