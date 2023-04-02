#use "Test.ml" ;;

(* Tests de la somme de deux flottants. Les tests sont effectués sur des couples de flottants compris entre -100.0 et 100.0 *)
let gen_floatcouple =
  let gen_float = Generator.float (-100.0) 100.0 in
    Generator.combine gen_float gen_float ;;
let red_floatcouple =
  let red_float = Reduction.float in
    Reduction.combine red_float red_float ;;
let test_floatcouple = Test.make_test gen_floatcouple red_floatcouple ;;

(* Construction des tests *)
let test_sum       = test_floatcouple "+ (correct)" (fun (a, b) -> a +. b = b +. a) ;;
let test_sum_wrong = test_floatcouple "+ (faux)"    (fun (a, b) -> a +. b = b -. a) ;;

(* Exécution des tests *)
Test.check    100 test_sum       ;;
Test.check    100 test_sum_wrong ;;
Test.fails_at 100 test_sum       ;;
Test.fails_at 100 test_sum_wrong ;;
Test.execute  100 [test_sum ; test_sum_wrong] ;;