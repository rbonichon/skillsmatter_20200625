type 'a set = 'a list
let empty = []
let singleton e = [e]
let mem = List.mem
let add e s = if mem e s then s else e :: s
let union s1 s2 = List.fold_left (fun s e -> add e s) s1 s2
let intersection s1 s2 =
  List.fold_left (fun s e -> if mem e s1 then e :: s else s) [] s2
