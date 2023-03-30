#use  "Property.ml" ;;
#use "Generator.ml" ;;
#use "Reduction.ml" ;;

module Test :
  sig
    (** Type d'un test portant sur des éléments de type 'a *)
    type 'a t

    (** Construit un test
      * @param gen  générateur pseudo-aléatoire de valeurs de test
      * @param red  stratégie de réduction
      * @param name nom du test
      * @param prop propriété qui fait l'objet du test
      * @return     test créé
      *)
    val make_test : 'a Generator.t -> 'a Reduction.t -> string -> 'a Property.t -> 'a t

    (** Effectue un test
      * @param n    nombre de valeurs à tester
      * @param test test à effectuer
      * @return     `true` si n > 0 et que toutes les valeurs à tester satisfont les conditions
      *)
    val check : int -> 'a t -> bool

    (** Cherche une valeur simple ne vérifiant pas la propriété
      * @param n nombre de valeurs à tester
      * @return  `None` si toutes les valeurs de test générées par `gen` vérifient `prop`,
                 une valeur ne vérifiant pas `prop` (éventuellement en appliquant `red`) sinon
      *)
    val fails_at : int -> 'a t -> 'a option

    (** Exécute plusieurs tests
      * @param n     nombre de valeurs testées par test
      * @param tests liste des tests à vérifier
      * @return      tableau associatif des résultats
      *)
    val execute : int -> ('a t) list -> ('a t * 'a option) list
  end =

    struct
      (* TODO : Implémenter le type et tous les éléments de la signature *)

      type 'a t = {
      gen : 'a Generator.t;
      red : 'a Reduction.t;
      name : string;
      prop : 'a Property.t;
    }

    let make_test gen red name prop =
      {gen = gen; red = red; name = name; prop = prop}

    (* let check n test =
      let rec loop i =
        if i >= n then true
        else match test.prop (test.gen ()) with
          | false -> false
          | true -> loop (i + 1)
      in
      if n <= 0 then false else loop 0 *)

    let check n test =
      let rec loop i =
        if i >= n then true
        else
          let value = test.gen () in
          let reduced = test.red value test.prop in
          match test.prop reduced with
          | false -> false
          | true -> loop (i + 1)
      in
      if n <= 0 then false else loop 0
    
      
  end ;;
