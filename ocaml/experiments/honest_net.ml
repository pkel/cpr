open Models

let networks = [ CliqueUniform10 ]

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  List.concat
    [ [ Nakamoto ]
    ; List.map (fun k -> B_k { k }) k
    ; List.map (fun k -> B_k_lessleadership { k }) k
    ; List.map (fun k -> George { k }) k
    ]
;;

let block_intervals = [ 30.; 60.; 120.; 300.; 600. ]

let tasks ~n_activations =
  List.concat_map
    (fun network ->
      List.concat_map
        (fun protocol ->
          List.map
            (fun block_interval ->
              { network
              ; protocol
              ; activations = n_activations
              ; scenario = AllHonest
              ; strategy = Honest
              ; activation_delay =
                  block_interval /. (pow_per_block protocol |> float_of_int)
              })
            block_intervals)
        protocols)
    networks
;;

let () = Csv_runner.command tasks
