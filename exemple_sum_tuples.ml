#use "Test.ml" ;;

(* Tests sur la comparaison de la somme de deux tuples *)
let gen_inttuplecouple =
  let gen_inttuple = Generator.combine (Generator.const 1) (Generator.const (0) ) in
    Generator.combine gen_inttuple gen_inttuple ;;
let red_inttuplecouple =
  let red_inttuple = Reduction.combine Reduction.int Reduction.int in
    Reduction.combine red_inttuple red_inttuple ;;
let test_inttuplecouple = Test.make_test gen_inttuplecouple red_inttuplecouple ;;

(* Construction des tests *)
let test_compare       = test_inttuplecouple "Tuple 1 > Tuple 2 (correct)" (fun ((a, b), (c, d)) -> a + b >= c + d) ;;
let test_compare_wrong = test_inttuplecouple "Tuple 1 > Tuple 2 (faux)"    (fun ((a, b), (c, d)) -> a + b < c + d) ;;

(* Exécution des tests *)
Test.check    100 test_compare       ;;
Test.check    100 test_compare_wrong ;;
Test.fails_at 100 test_compare       ;;
Test.fails_at 100 test_compare_wrong ;;
Test.execute  100 [test_compare ; test_compare_wrong] ;;
