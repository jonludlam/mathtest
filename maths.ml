(* Maths *)

module Expression = struct 
  type binop = Sub | Div | Deriv
  type mop = Add | Mul
  type uop = Selected | Cursor

  type expr = 
    | Mop of (mop * (expr list))
    | Binop of (binop * expr * expr)
    | Uop of (uop * expr)
    | Const of float
    | Variable of string

  (* Very basic simplification *)
  let rec simp exp =
    let expsort l =
      List.sort (fun a b ->
		   match a, b with
		       Const a, Const b -> compare a b
		     | Const a, _ -> -1
		     | _, Const a -> 1
		     | _, _ -> 0) l in
    match exp with
      | Binop (Sub, Const a, Const b) -> Const (a -. b)
      | Binop (Div, Const a, Const b) -> Const (a /. b)
      | Mop (Add,l) ->
	  let rec flatten l =
	    List.flatten 
	      (List.map (fun l -> 
			  match l with 
			    | Mop (Add,ls) -> flatten ls
			    | _ as e -> [e]) l)
	  in 
	  let l = flatten l in
	  if List.length l = 1 then List.hd l else begin
	    let l = expsort l in
	    let rec combconsts es = 
	      match es with 
		| (Const a) :: (Const b) :: es ->
		    combconsts ((Const (a+.b))::es)
		| _ -> es
	    in
	    let l = combconsts l in
	    Mop (Add,List.map simp (List.filter (fun term -> term <> Const 0.0) l))
	  end
      | Mop (Mul,l) ->
	  let rec flatten l =
	    List.flatten 
	      (List.map (fun l -> 
			  match l with 
			    | Mop (Mul,ls) -> flatten ls
			    | _ as e -> [e]) l)
	  in 
	  let l = flatten l in
	  if List.length l = 1 then List.hd l else begin
	    let l = expsort l in
	    let rec combconsts es = 
	      match es with 
		| (Const a) :: (Const b) :: es ->
		    combconsts ((Const (a*.b))::es)
		| _ -> es
	    in
	    let l = combconsts l in
	    if (List.exists (fun term -> term = Const 0.0) l) 
	    then Const 0.0 
	    else Mop (Mul,(List.map simp (List.filter (fun term -> term <> Const 1.0) l)))
	  end
      | Binop (Sub, a, Const 0.0) -> simp a
      | Binop (Sub, Const 0.0, a) -> simp (Mop (Mul,[Const (-1.0); simp a]))
      | Binop (Sub,x,y) -> if x=y then Const 0.0 else Binop (Sub, simp x, simp y)
      | Binop (Div, e, Const 1.0) -> simp e
      | Binop (Div, e1, e2) -> Binop(Div, simp e1, simp e2)
      | e -> e

  let rec substitute ((variable,sub) as v) expr =
    match expr with
      | Variable v -> if v=variable then sub else expr
      | Mop (ty,es) -> Mop (ty,List.map (substitute v) es)
      | Binop (ty,e1,e2) -> Binop (ty, substitute v e1, substitute v e2)
      | Uop (ty,e) -> Uop (ty, substitute v e)
      | x -> x
	  
  let simplify exp =
    let rec doit e =
      let e2 = simp e in
      if e=e2 then e2 else doit e2
    in doit exp

  let rec prec op =
    match op with
      | Mop(Add,_) -> 1
      | Binop(Sub,_,_) -> 2
      | Mop(Mul,_) -> 4
      | Binop(Div,_,_) -> 3
      | Const _ -> 5
      | Variable _ -> 5
      | Binop(Deriv,_,_) -> 5
      | Uop(_,e) -> prec e
	  
  let canonicalize exp =
    let rec exp_to_num a =
      match a with
	| Const _ -> 0
	| Variable x -> 1
	| Mop(Add,_) -> 2
	| Binop(Sub,_,_) -> 3
	| Mop(Mul,_) -> 4
	| Binop(Div,_,_) -> 5
	| Binop(Deriv,_,_) -> 6
	| Uop(_,e) -> exp_to_num e
    in
    let sort explist =
      List.sort (fun a b ->
		   match a,b with
		     | Variable x, Variable y -> compare x y
		     | _ -> compare (exp_to_num a) (exp_to_num b)) explist
    in
    let rec inner exp =
      let exp = simplify exp in
      match exp with
	| Binop(Sub,a,b) -> inner (Mop(Add,[a; Mop(Mul,[Const (-1.0); b])]))
	| Binop(Div,Binop(Div,a,b),c) ->
	    Binop(Div, inner a, inner (Mop(Mul,[b;c])))
	| Binop(Div, a, Binop (Div,b,c)) ->
	    Binop(Div,inner (Mop(Mul,[a;c])), inner b)
	| Mop(Mul,list) ->
	    if List.exists (function Mop(Add,_) -> true | _ -> false) list then
	      let rec doit xs nonaddterms =
		match xs with
		  | (Mop(Add,adds))::rest -> inner (Mop(Add,List.map (fun e -> Mop(Mul,e::rest @ nonaddterms)) adds))
		  | x::rest -> doit rest (x::nonaddterms)
	      in doit list []
	    else if List.exists (function (Binop(Div,_,_)) -> true | _ -> false) list then
	      let rec doit xs nondivterms =
		match xs with
		  | (Binop(Div,a,b))::rest -> inner (Binop(Div,Mop(Mul,((a::nondivterms)@rest)), b))
		  | x::rest -> doit rest (x::nondivterms)
	      in doit list []
	    else 
	      Mop(Mul,List.map inner (sort list)) 
	| Mop(Add,list) ->
	    Mop(Add,List.map inner (sort list))
	| Binop(Div,Mop(Add,adds),b) ->
	    Mop(Add,List.map (fun e -> Binop(Div,e,b)) adds)
	| Binop(Div,a,b) -> Binop(Div,inner a, inner b)
	| Uop(ty,x) -> Uop(ty,inner x)
	| x -> x
    in
    let rec doit exp =
      let exp2 = inner exp in
      if exp <> exp2 then doit exp2 else exp2 
    in doit exp

  let rec apply_to_exactly_one fn list =
    match list with
      | [x] -> [fn x]
      | x::xs -> (try (fn x)::xs with _ -> x::(apply_to_exactly_one fn xs))
      | _ -> failwith "Empty list"

  let rec apply_to_denominator fn expr =
    match expr with
      | Mop(Add,elist) -> Mop(Add,List.map (apply_to_denominator fn) elist)
      | Binop (Sub,e1,e2) -> Binop(Sub,apply_to_denominator fn e1, apply_to_denominator fn e2)
      | Mop(Mul,elist) ->
	  Mop(Mul,apply_to_exactly_one (apply_to_denominator fn) elist)
      | Binop(Div,e1,e2) ->
	  (try
	    Binop(Div,e1, fn e2)
	  with _ ->
	    Binop(Div,apply_to_denominator fn e1, e2))
      | _ -> failwith "No denominator!"
	    
  let rec to_string exp =
    let myprec = prec exp in
    let maybefence e =
      let s = to_string e in
      if prec e < myprec then
	Printf.sprintf "(%s)" s
      else 
	s
    in
    match exp with
      | Mop(Add,elist) -> Printf.sprintf "%s" (String.concat " + " (List.map maybefence elist))
      | Binop(Sub,e1,e2) -> Printf.sprintf "%s - %s" (maybefence e1) (maybefence e2)
      | Mop(Mul,elist) -> Printf.sprintf "%s" (String.concat " * " (List.map maybefence elist))
      | Binop(Div,e1,e2) -> Printf.sprintf "%s / %s" (maybefence e1) (maybefence e2)
      | Variable v -> v
      | Const n -> Printf.sprintf "%f" n
      | Binop(Deriv,s1,s2) -> Printf.sprintf "d%s/d%s" (maybefence s1) (maybefence s2)
      | Uop(Selected,x) -> Printf.sprintf "[[%s]]" (to_string x)
      | Uop(Cursor,x) -> Printf.sprintf "{{%s}}" (to_string x)

  let rec to_json exp =
    match exp with
      | Mop(Add,elist) -> 
	  Printf.sprintf "{\"ty\":\"Add\",\n\"addlist\":[%s]}" 
	    (String.concat "," (List.map to_json elist))
      | Binop(Sub,e1,e2) ->
	  Printf.sprintf "{\"ty\":\"Sub\",\n\"arg1\":%s,\"arg2\":%s}"
	    (to_json e1) (to_json e2)
      | Mop(Mul,elist) ->
	  Printf.sprintf "{\"ty\":\"Mul\",\n\"mullist\":[%s]}" 
	    (String.concat "," (List.map to_json elist))
      | Binop(Div,e1,e2) ->
	  Printf.sprintf "{\"ty\":\"Div\",\n\"arg1\":%s,\n\"arg2\":%s}"
	    (to_json e1) (to_json e2)
      | Variable v ->
	  Printf.sprintf "{\"ty\":\"Variable\",\n\"name\":\"%s\"}" v
      | Const n ->
	  Printf.sprintf "{\"ty\":\"Const\",\n\"constvalue\":\"%f\"}" n
      | Binop(Deriv,s1,s2) ->
	  Printf.sprintf "{\"ty\":\"Deriv\",\n\"arg1\":\"%s\",\n\"arg2\":\"%s\"}" (to_json s1) (to_json s2)
      | Uop(x,y) -> 
	  Printf.sprintf "{\"ty\":\"Selected\",\n\"arg\":\"%s\"}" (to_json y)

  (** expdivides exp1 exp2 returns true if exp2 divides exp1 exactly (i.e. exp1/exp2 has no remainder) *)
  let rec expdivides exp1 exp2 =
    Printf.printf "expdivides(%s,%s)\n%!" (to_string exp1) (to_string exp2);
    if exp1=exp2 then true else
      match exp1 with
	| Mop(Add,elist) -> List.fold_left (fun acc expr -> acc && (expdivides expr exp2)) true elist
	| Binop(Sub,e1,e2) -> (expdivides e1 exp2) && (expdivides e2 exp2) 
	| Mop(Mul,elist) -> List.fold_left (fun acc expr -> acc || (expdivides expr exp2)) false elist
	| Binop(Div,e1,e2) -> (expdivides e1 exp2)
	| _ -> false

  let rec remove toremove expr =
(*    Printf.printf "In remove: toremove=%s expr=%s\n%!" (to_string 0 toremove) (to_string 0 expr);*)
    if canonicalize expr = canonicalize toremove then Const 1.0 else
      match expr with
	| Mop(Add,elist) -> Mop(Add,List.map (remove toremove) elist)
	| Binop(Sub,e1,e2) -> Binop(Sub,remove toremove e1, remove toremove e2)
	| Mop(Mul,elist) ->
	    Mop(Mul,apply_to_exactly_one (remove toremove) elist)
	| Binop(Div,e1,e2) -> 
	    (try Binop(Div,remove toremove e1, e2) with _ -> Binop(Div, e1, apply_to_denominator (remove toremove) e2))
	| _ -> failwith "Nope"
	    
  let rec divide exp1 exp2 =
    Mop(Mul,[ exp1; Binop(Div,Const 1.0,exp2); ])
  and multiply exp1 exp2 =
    Mop(Mul, [ exp1; exp2 ])

  let rec cancel tocancel expr =
(*    Printf.printf "In cancel: tocancel=%s expr=%s\n%!" (to_string 0 tocancel) (to_string 0 expr);*)
    match expr with
      | Mop(Add,es) -> Mop(Add,List.map (cancel tocancel) es)
      | Binop(Sub,e1,e2) -> Binop(Sub,cancel tocancel e1, cancel tocancel e2)
      | Mop(Mul,es) ->
	  (try
	    let e = apply_to_exactly_one (remove tocancel) es in
	    let e = apply_to_exactly_one (apply_to_denominator (remove tocancel)) e in
	    Mop(Mul,e)
	  with _ ->
	    Mop(Mul,es))
      | Binop(Div,e1,e2) ->
	  (try
	    Binop(Div,remove tocancel e1, remove tocancel e2)
	  with _ ->
	    Binop(Div,cancel tocancel e1, e2))
      | x -> x

  let cancel_and_simplify tocancel expr =
    simplify (cancel tocancel expr)

  let divide_and_cancel e1 e2 =
    simplify (cancel e2 (divide e1 e2))

  let multiply_and_cancel e1 e2 =
    simplify (cancel e2 (multiply e1 e2))

  let expand expr =
    match expr with
      | Mop(Mul,(Mop(Add,es))::rest) ->
	  Mop(Add,List.map (fun e -> Mop(Mul,e::rest)) es)
      | _ -> expr
end

module Equation = struct
  open Expression
  
  type eqn = expr * expr

(*  let logistic = 
    (Deriv ("y","t"), Mul [Variable "beta"; Variable "y"; Div ( Sub (Variable "kappa", Variable "y"), Variable "kappa")])*)

  let divide_and_cancel eqn expr =
    let e1,e2 = eqn in
    (Expression.divide_and_cancel e1 expr, Expression.divide_and_cancel e2 expr)

  let multiply_and_cancel eqn expr =
    let e1,e2 = eqn in
    (Expression.multiply_and_cancel e1 expr, Expression.multiply_and_cancel e2 expr)

  let to_string eqn =
    let e1,e2 = eqn in
    Printf.sprintf "%s = %s" (to_string e1) (to_string e2)

  let apply fn eqn =
    let e1, e2 = eqn in
    (fn e1, fn e2)
end

open Expression
open Equation

let print exp =
  Printf.printf "%s\n" (to_string exp)


