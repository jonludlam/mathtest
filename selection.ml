module ME = Maths.Expression
module MQ = Maths.Equation

let is_selection expr = 
  match expr with
    | ME.Uop (ME.Selected,_) -> true
    | _ -> false

let has_selection list =
  List.exists is_selection list

let rec remove_selection expr =
  match expr with
    | ME.Mop(x,ys) -> 
	ME.Mop(x,List.map remove_selection ys)
    | ME.Binop(x,y,z) ->
	ME.Binop(x,remove_selection y, remove_selection z)
    | ME.Uop(ME.Selected,z) -> z
    | ME.Uop(x,z) -> z
    | x -> x

let rec select_up expr =
  match expr with
    | ME.Uop(ME.Selected,z) -> expr
    | ME.Mop(x,ys) ->
	if has_selection ys then 
	  ME.Uop(ME.Selected,
			       ME.Mop(x,List.map remove_selection ys))
	else 
	  ME.Mop(x,List.map select_up ys)
    | ME.Binop(x,y,z) ->
	if is_selection y then
	  ME.Uop(ME.Selected,ME.Binop(x,remove_selection y, z))
	else if is_selection z then
	  ME.Uop(ME.Selected,ME.Binop(x,y,remove_selection z))
	else 
	  ME.Binop(x,select_up y, select_up z)
    | ME.Uop(x,y) ->
	ME.Uop(x,select_up y)
    | e -> e

let rec select_down expr = 
  match expr with
    | ME.Uop(ME.Selected,ME.Mop(x,ys)) ->
	let a::b = ys in
	ME.Mop(x,(ME.Uop(ME.Selected,a))::b)
    | ME.Uop(ME.Selected,ME.Binop(x,y,z)) ->
	ME.Binop(x,ME.Uop(ME.Selected,y),z)
    | ME.Uop(ME.Selected,ME.Uop(x,y)) ->
	ME.Uop(x,ME.Uop(ME.Selected,y)) 
    | ME.Mop(x,ys) -> ME.Mop(x,List.map select_down ys)
    | ME.Binop(x,y,z) -> ME.Binop(x,select_down y, select_down z)
    | e -> e

let find_selected_index exprs =
  let (_,result) = List.fold_left (fun (i, result) expr -> if is_selection expr then (i+1,i) else (i+1, result)) (0,-1) exprs in
  if result = -1 then raise Not_found else result

let select_from_index exprs index =
  let arr = Array.of_list exprs in
  arr.(index) <- ME.Uop(ME.Selected,arr.(index));
  Array.to_list arr
    
let rec select_left e =
  match e with
    | ME.Binop(x,y,ME.Uop(ME.Selected,z)) -> ME.Binop(x,ME.Uop(ME.Selected,y),z)
    | ME.Mop(x,ys) ->
	begin
	  try 
	    let i = find_selected_index ys in
	    if i=0 then e else ME.Mop(x,select_from_index (List.map remove_selection ys) (i-1))
	  with Not_found ->
	    ME.Mop(x,List.map select_left ys)
	end
    | ME.Binop(x,y,z) -> ME.Binop(x,select_left y, select_left z)
    | ME.Uop(ME.Selected, _) -> e
    | ME.Uop(x,y) -> ME.Uop(x,select_left y)
    | e -> e

let rec select_right e =
  match e with
    | ME.Binop(x,ME.Uop(ME.Selected,y),z) -> ME.Binop(x,y,ME.Uop(ME.Selected,z))
    | ME.Mop(x,ys) ->
	begin
	  try 
	    let i = find_selected_index ys in
	    if i=(List.length ys)-1 then e else ME.Mop(x,select_from_index (List.map remove_selection ys) (i+1))
	  with _ ->
	    ME.Mop(x,List.map select_right ys)
	end
    | ME.Binop(x,y,z) -> ME.Binop(x,select_right y, select_right z)
    | ME.Uop(ME.Selected, _) -> e
    | ME.Uop(x,y) -> ME.Uop(x,select_right y)
    | e -> e

let rec expand_selection_right e =
  match e with
    | ME.Mop(x,ys) -> 
	if has_selection ys 
	then begin
	  let rec inner = function
	    | ME.Uop(ME.Selected,y)::z::more ->
		ME.Uop(ME.Selected,ME.Mop(x,[y;z]))::more
	    | e::es -> e::(inner es)
	    | [] -> []
	  in
	  ME.Mop(x,inner ys)
	end else ME.Mop(x,List.map expand_selection_right ys)
    | ME.Binop(x,y,z) -> ME.Binop(x,expand_selection_right y, expand_selection_right z)
    | ME.Uop(ME.Selected,y) -> e
    | ME.Uop(x,y) -> ME.Uop(x,expand_selection_right y)
    | e -> e	

let select_lhs (e1,e2) =
  (ME.Uop(ME.Selected,e1),e2)

let select_rhs (e1,e2) =
  (e1,ME.Uop(ME.Selected,e2))


let select_down_eqn (e1,e2) = (select_down e1, select_down e2)

let select_up_eqn (e1,e2) = (select_up e1, select_up e2)

let select_right_eqn (e1,e2) = (select_right e1, select_right e2)

let select_left_eqn (e1,e2) = (select_left e1, select_left e2)
	  
let expand_selection_right_eqn = MQ.apply expand_selection_right 
(*let expand_selection_left_eqn = MQ.apply expand_selection_left *)

let rec apply_to_selection f e = 
  match e with
  | ME.Binop(x,y,z) -> ME.Binop(x,apply_to_selection f y, apply_to_selection f z)
  | ME.Mop(x,ys) -> ME.Mop(x,List.map (apply_to_selection f) ys)
  | ME.Uop(ME.Selected,e) -> ME.Uop(ME.Selected, f e)
  | ME.Uop(x,y) -> ME.Uop(x,apply_to_selection f y)
  | e -> e
