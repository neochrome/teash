(* this module is kept around until the stdlib will include something similar *)

let map f = function None -> None | Some v -> Some (f v)
let is_some = function None -> false | Some _ -> true
let is_none = function None -> true | Some _ -> false
let value = function None -> invalid_arg "Can't get value of None" | Some v -> v
