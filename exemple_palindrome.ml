#use "Test.ml" ;;

(*Fonction rev qui renvoie l'inverse d'une chaine de caractere*)
let rev s =
  let n = String.length s in
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (acc ^ String.make 1 s.[i])
  in aux (n - 1) "";;

(* fonction qui determine si un mot est un palindrome <==> un mot peut se lire dans les deux sens*)
let is_palindrome str =
  let n = String.length str in
  let rec check i j =
    if i >= j then true
    else if str.[i] <> str.[j] then false
    else check (i+1) (j-1) in check 0 (n-1);;

(* Tests si une chaine de caractere est un palindrome *)
let gen_string = (Generator.string (Generator.next(Generator.int 1 10)) (Generator.char)) ;;
let red_string = Reduction.string (Reduction.char );;
let test_palindrome = Test.make_test gen_string red_string;;

(*Construction des tests*)
let palindrome_prop = test_palindrome "List.@ (correct)" (fun (str) -> is_palindrome str = (str = rev str));;
let palindrome_prop_wrong =  test_palindrome "List.@ (wrong)" (fun (str) -> is_palindrome str = (str <> rev str));;

(* Exécution des tests *)
Test.check    10 palindrome_prop       ;;
Test.check    10 palindrome_prop_wrong ;;
Test.fails_at 10 palindrome_prop       ;;
Test.fails_at 10 palindrome_prop_wrong ;;
Test.execute  10 [palindrome_prop ; palindrome_prop_wrong] ;;