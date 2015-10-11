let rec mulpartition exprs =
  let rec inner es =
    match es with
      | [e] -> [[e],[]; [],[e]]
      | e::es -> 
	  List.flatten (List.map (fun (ins,outs) -> [(e::ins , outs); (ins, e::outs)]) (inner es))
      | _ -> []
  in inner exprs


	  
	      
