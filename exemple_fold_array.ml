#use "Test.ml" ;;
(**Cas de test pour sommer les elements d'un tableau : ne fonctionne pas (erreur de type) mais structure correcte*)

(*
let sum arr = Array.fold_left (+) 0 arr ;;
let gen_array = (Generator.array 10 (Generator.int (-10) 10));;

let red_array = Reduction.array Generator.next (gen_array);;
let test_array = Test.make_test Generator.next (gen_array) [red_array];;

let array_true = test_array "somme tableau (correct)" (fun() -> Reduction.array gen_array 0 ( + ) = sum gen_array);;
let array_wrong = test_array "somme tableau (faux)" (fun() -> Reduction.array gen_array 0 ( * ) = sum gen_array);;


Test.check    10 array_true       ;;
Test.check    10 array_wrong ;;
Test.fails_at 10 array_true       ;;
Test.fails_at 10 array_wrong ;;
Test.execute  10 [array_true ; array_wrong] ;;
**)