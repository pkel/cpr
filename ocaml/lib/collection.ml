type 'a entry =
  { it : 'a
  ; info : string
  }

type 'a t = (string * 'a entry) list

let all t = List.map fst t
let get k t = List.assoc_opt k t |> Option.map (fun e -> e.it)
let describe k t = List.assoc_opt k t |> Option.map (fun e -> e.info)
let empty = []
let add ~info key it t = (key, { it; info }) :: t
let iter f = List.iter (fun (key, { it; info }) -> f ~info key it)
let map_to_list f = List.map (fun (key, { it; info }) -> f ~info key it)
