type 'a t = ('a, string) result

let map = Result.map
let bind = Result.bind

let get_exn = function
  | Ok x -> x
  | Error s -> failwith s
;;

let errf fmt = Printf.ksprintf (fun s -> Error s) fmt

module Syntax = struct
  let map' t f = map f t
  let ( >|= ) = map'
  let ( >>= ) = bind
  let ( let+ ) = map'
  let ( let* ) = bind

  let pair a b =
    let* a = a in
    let+ b = b in
    a, b
  ;;

  let ( and+ ) = pair
  let ( and* ) = pair
end
