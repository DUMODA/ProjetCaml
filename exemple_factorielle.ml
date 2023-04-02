#use "Test.ml" ;;

(*Fonction factoriel pour test*)
let rec factorielle n =
  match n with
  | 0 -> 1
  | _ -> n * factorielle (n - 1)

(* Tests pour la fonction factoriellele *)
let gen_nonneg_int = Generator.int 1 10;;
let red_factorielle = Reduction.int;;
let test_factorielle = Test.make_test gen_nonneg_int red_factorielle;;

(*Construction des tests*)
let test_factorielle_correct = test_factorielle "factorielle n (correct)" (fun n -> factorielle n = List.fold_left ( * ) 1 (List.init n (fun i -> i + 1)));;
let test_factorielle_wrong = test_factorielle "factorielle n (faux)" (fun n -> factorielle n = List.fold_left ( + ) 1 (List.init (n-1) (fun i -> i + 1)));;
  
(* Exécution des tests *)
Test.check 10 test_factorielle_correct ;;
Test.check 10 test_factorielle_wrong ;;
Test.fails_at 10 test_factorielle_correct ;;
Test.fails_at 10 test_factorielle_wrong ;;
Test.execute 10 [test_factorielle_correct ; test_factorielle_wrong] ;;
