type 'a entry =
  { key : string
  ; info : string
  ; it : 'a
  }

type 'a t = (string * 'a entry) list

let all t = List.map fst t
let get = List.assoc_opt
let empty = []
let add ~info key it t = (key, { it; info; key }) :: t
let iter f = List.iter (fun (_, e) -> f e)

let map f t =
  List.map
    (fun (_, e) ->
      let e = f e in
      e.key, e)
    t
;;

let map_to_list f = List.map (fun (_, e) -> f e)
