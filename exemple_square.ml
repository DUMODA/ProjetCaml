#use "Test.ml" ;;

let square n = n*n;;
(* Tests pour la fonction carree *)
let gen_int = Generator.int (-4) 4;;
let red_square = Reduction.int;;
let test_square = Test.make_test gen_int red_square;;

(*Construction des test*)
let test_square_correct = test_square "Carrée n*n (correct)" (fun n -> square n = n * n);;
let test_square_wrong = test_square "Carrée n*n (wrong)" (fun n -> square n = n * 2);;

(* Execution of tests *)
Test.check 4 test_square_correct;;
Test.check 4 test_square_wrong;;
Test.fails_at 4 test_square_correct;;
Test.fails_at 4 test_square_wrong;;
Test.execute 4 [test_square_correct; test_square_wrong];;