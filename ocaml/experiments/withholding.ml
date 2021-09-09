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
            , [ Constant; Block ]
            , [ Honest; SelfishSimple; SelfishAdvanced ] )
          ; ( B_k { k }
            , [ Constant; Block ]
            , [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ] )
          ])
        k
    ; [ Nakamoto, [ Constant ], [ Honest; SelfishAdvanced; NumHonest; NumSelfishAdvanced ]
      ]
      (* ; List.map (fun k -> George { k }, [ Constant; Punish; Discount; Hybrid ]) k *)
    ]
;;

let block_intervals = [ 600. ]

let tasks ~n_activations =
  List.concat_map
    (fun network ->
      List.concat_map
        (fun (protocol, incentive_schemes, strategies) ->
          List.concat_map
            (fun strategy ->
              List.map
                (fun block_interval ->
                  { network
                  ; incentive_schemes
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
