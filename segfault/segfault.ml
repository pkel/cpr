let ()=
  Py.initialize();;
  let layout = Bigarray.C_layout in
  let elttype= Bigarray.Float64 in
  let pythonmodule=Py.import "pythonmodule" in
  let evalbigarray () =
      let res = Py.Module.get_function pythonmodule "predict" [| |] in
      Numpy.to_bigarray elttype layout res
  in
  for _i=0 to 10000 do
     ignore(evalbigarray());
  done;
  ()
