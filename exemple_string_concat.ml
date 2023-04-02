(* Tests de la fonction String.concat *)
(* Les tests sont effectués sur des listes de chaînes de caractères d'au plus 5 éléments chacune *)
let gen_stringlistcouple =
  let gen_stringlist = Generator.list 5 (Generator.string (Generator.next(Generator.int 0 10)) (Generator.char)) in
    Generator.combine gen_stringlist gen_stringlist ;;
let red_stringlistcouple =
  let red_stringlist = Reduction.list     (fun s -> [s]) in
    Reduction.combine red_stringlist red_stringlist ;;
let test_stringlistcouple = Test.make_test gen_stringlistcouple red_stringlistcouple ;;

(* Construction des tests *)
let test_concat       = test_stringlistcouple "String.concat (correct)" (fun (l1, l2) -> String.length (String.concat "" (l1 @ l2)) = (String.length (String.concat "" l1)) + (String.length (String.concat "" l2))) ;;
let test_concat_wrong = test_stringlistcouple "String.concat (faux)"    (fun (l1, l2) -> String.length (String.concat "" (l1 @ l2)) = (String.length (String.concat "" l1)) * (String.length (String.concat "" l2))) ;;

(* Exécution des tests *)
Test.check    100 test_concat       ;;
Test.check    100 test_concat_wrong ;;
Test.fails_at 100 test_concat       ;;
Test.fails_at 100 test_concat_wrong ;;
Test.execute  100 [test_concat ; test_concat_wrong] ;;