open Maths.Expression
open Maths.Equation

let print exp =
  Printf.printf "%s\n" (to_string exp)

let px s = 
  let lexbuf = Lexing.from_string s in
  Parser.expreof Lex.eqn lexbuf

let pq s =
  let lexbuf = Lexing.from_string s in
  Parser.eqneof Lex.eqn lexbuf

let main () =
  let logistic = pq "\\deriv(y,t)='beta'y('kappa'-y)/'kappa'" in 
  let (lhs,rhs) = logistic in
  Printf.printf "json:\n%s\n" (to_json (simplify rhs));
  let e1 = divide_and_cancel logistic (px "y")  in
  print e1;
  let sel = Selection.select_rhs (apply simplify logistic) in
  print sel;
  let sel2 = Selection.select_down_eqn sel in
  print sel2;
  let sel3 = Selection.select_down_eqn sel2 in
  print sel3;
 let sel4 = Selection.expand_selection_right_eqn sel3 in
  print sel4;
 let sel5 = Selection.expand_selection_right_eqn sel4 in
  print sel5;
  let e2 = divide_and_cancel e1 (px "'kappa'-y") in
  print e2;
  let sel = Selection.select_lhs e2 in
  print sel;
  let sel = Selection.select_down_eqn sel in
  print sel;
  let sel = Selection.select_right_eqn sel in
  print sel;
  let sel = Selection.expand_selection_right_eqn sel in
  print sel;
  let e3 = pq "(1/y)(1/('kappa'-y))=a/y + b/('kappa'-y)" in
  print e3;
  let e4 = multiply_and_cancel e3 (px "y") in
  let e5 = multiply_and_cancel e4 (px "'kappa'-y") in
  print e4;
  print (apply expand e4);
  print (apply (cancel_and_simplify (px "y")) (apply expand e4));
  let e6 = (apply simplify (apply (cancel (px "'kappa'-y")) (apply (cancel (px "y")) (apply expand e5)))) in

  let e7 = (apply simplify (apply (substitute ("y",px "0.0")) e6)) in

  print e7;
  
  let e8 = divide_and_cancel e7 (px "'kappa'") in

  print e8;

  let e9 = (apply simplify (apply (substitute ("y",(px "'kappa'"))) e6)) in
  
  print e9;

  let e10 = divide_and_cancel e9 (px "'kappa'") in

  print e10
    
let _ = Printexc.print main ()
