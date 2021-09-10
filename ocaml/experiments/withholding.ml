open Models

let networks =
  [ 0.1; 0.2; 0.25; 0.3; 0.33; 0.4; 0.45; 0.5 ]
  |> List.map (fun alpha -> TwoAgentsZero { alpha })
;;

let protocols =
  let k = [ 1; 2; 4; 8; 16; 32; 64; 128 ] in
  List.concat
    [ List.concat_map
        (fun k ->
          [ ( B_k_lessleadership { k }
            , [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ] )
          ; B_k { k }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
          ; George { k }, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
          ])
        k
    ; [ Nakamoto, [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ] ]
    ]
;;

let block_intervals = [ 600. ]

let tasks ~n_activations =
  List.concat_map
    (fun network ->
      List.concat_map
        (fun (protocol, strategies) ->
          List.concat_map
            (fun strategy ->
              List.map
                (fun block_interval ->
                  { network
                  ; protocol
                  ; activations = n_activations
                  ; scenario = FirstSelfish
                  ; strategy
                  ; activation_delay =
                      block_interval /. (pow_per_block protocol |> float_of_int)
                  })
                block_intervals)
            strategies)
        protocols)
    networks
;;

let () = Csv_runner.command tasks
