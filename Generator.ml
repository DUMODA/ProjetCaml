module Generator :
  sig
    (** Type du générateur pseudo-aléatoire de données de type 'a *)
    type 'a t

    (** Renvoie une nouvelle valeur aléeatoire
      * @param gen générateur pseudo-aléatoire
      * @return    nouvelle valeur aléatoire en utilisant `gen`
      *)
    val next : 'a t -> 'a

    (** Générateur constant
      * @param x valeur
      * @return  générateur de l'unique valeur `x`
      *)
    val const : 'a -> 'a t

    (* GENERATEURS DE TYPES DE BASE *)
 
    (** Générateur pseudo-aléatoire de booléens
      * @param prob probabilité de la valeur `true`
      * @return     générateur pseudo-aléatoire de valeurs booléennes
      *)
    val bool : float -> bool t

    (** Générateur pseudo-aléatoire d'entiers
      * @param a borne inférieure
      * @param b borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre `a` et `b` inclus
      *)
    val int : int -> int -> int   t

    (** Générateur pseudo-aléatoire d'entiers positifs ou nuls
      * @param n borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre 0 et `n` inclus
      *)
    val int_nonneg : int -> int   t

    (** Générateur pseudo-aléatoire de flottants
      * @param x borne supérieure
      * @param y borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre `x` et `y` inclus
      *)
    val float : float -> float -> float t

    (** Générateur pseudo-aléatoire de flottants positifs ou nuls
      * @param x borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs flottantes entre 0 et `x` inclus
      *)
    val float_nonneg : float -> float t

    (** Générateur pseudo-aléatoire de caractères *)
    val char : char t

    (** Générateur pseudo-aléatoire de caractères alphanumériques *)
    val alphanum : char t

    (* GENERATEURS DE CHAINE DE CARACTERE *)

    (** Générateur de chaînes de caractères
      * @param n   longueur maximale de la chaîne de caractère
      * @param gen générateur pseudo-aléatoire de caractères
      * @return    générateur pseudo-aléatoire de chaînes de caractères dont chaque caractéré est généré avec `gen`
      *)
    val string : int -> char t -> string t

    (* GENERATEURS DE LISTES *)

    (** Générateur de listes
      * @param n   longueur maximale de la liste
      * @param gen générateur pseudo-aléatoire d'éléments
      * @return    générateur pseudo-aléatoire de listes dont chaque élément est généré avec `gen`
      *)
    val list : int -> 'a t -> ('a list) t

    (* TRANSFORMATIONS *)

    (** Générateur pseudo-aléatoire de couples
      * @param fst_gen générateur pseudo-aléatoire de la première coordonnée
      * @param snd_gen générateur pseudo-aléatoire de la deuxième coordonnée
      * @return        générateur pseudo-aléatoire du couple
      *)
    val combine : 'a t -> 'b t -> ('a * 'b) t

    (** Applique un post-traitement à un générateur pseudo-aléatoire
      * @param f   post-traitement à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `f` à chaque valeur générée par `gen`
      *)
    val map : ('a -> 'b) -> 'a t -> 'b t

    (** Applique un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire ne générant des valeurs de `gen` que si elles vérifient `p`
      *)
    val filter : ('a -> bool) -> 'a t -> 'a t

    (** Applique un post-traitement dépendant d'un filtre à un générateur pseudo-aléatoire
      * @param p   filtre à appliquer à chaque valeur générée
      * @param f   couple des post-traitements à appliquer à chaque valeur générée
      * @param gen générateur pseudo-aléatoire
      * @return    générateur pseudo-aléatoire obtenu en appliquant `fst f` pour toute valeur vérifiant `p`
      *                                                          et `snd f` pour toute valeur ne le vérifiant pas
      *)
    val partitioned_map : ('a -> bool) -> (('a -> 'b) * ('a -> 'b)) -> 'a t -> 'b t
  end = 
  struct
    (* TODO : Implémenter le type et tous les éléments de la signature *)

    (* Initialise le générateur de nombres aléatoires *)
    Random.self_init ();;


    (* Type du générateur pseudo-aléatoire de données de type 'a *)
    type 'a t = unit -> 'a ;;


    (* Permet de générer un nombre aléatoire *)
    let next f = f ();;


    (* Générateur constant d'une entité quelconque *)
    let const x = fun () -> x;;  


    (* Générateur pseudo-aléatoire de booléens *)
    let bool prob =
      fun () -> Random.float 1.0 <= prob;;


    (* Générateur pseudo-aléatoire d'entiers sur l'intervalle [a, b] *)
    let int a b =
      let borne_sup = b - a + 1 in
        let gen_int () = a + Random.int borne_sup in
          gen_int;;

    (*
      Si non négatif, mettre le mettre entre paranthèse
      ex : let neg_range = Geneerator.int (-100) (-50);;
    *)
    
    
    (* Générateur pseudo-aléatoire d'entiers sur l'intervalle [0, n] *)
    let int_nonneg n =
      fun () -> Random.int (n + 1);;

    
    (* Générateur pseudo-aléatoire de flottants sur l'intervalle [x, y] *)
    let float x y =
      let borne_sup = y -. x in
        let gen_float () = x +. Random.float borne_sup in gen_float;;

    
    (* Générateur pseudo-aléatoire de flottants sur l'intervalle [0, x] *)
    let float_nonneg x = 
      fun () -> Random.float (x +. 1.);;
    

    (* Générateur pseudo-aléatoire de caractères *)
    let char =
      fun () -> char_of_int (Random.int ((int_of_char 'z') - (int_of_char 'a')) + (int_of_char 'a'));;
    

    (* Générateur pseudo-aléatoire de caractères alphanumériques *)
    let alphanum = 
      fun () -> let alphanum_str = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" in
        let i = Random.int (String.length alphanum_str) in alphanum_str.[i];;

    
    (* Générateur pseudo-aléatoire d'une chaine de caracteres de longueur n *)
    let string n gen =
      let rec gen_string acc = function
        | 0 -> acc
        | n -> gen_string (acc ^ String.make 1 (next gen)) (n - 1) in 
      fun () -> gen_string "" n ;;


    (* Générateur de listes d'éléments de taille n *)
    let list n gen = 
      let rec gen_list acc i =
        if i <= 0 then acc
        else gen_list ((next gen) :: acc) (i-1) in
        fun () -> gen_list [] n;;


    (* Générateur pseudo-aléatoire de couples *)
    let combine fst_gen snd_gen = 
      let f () = (next fst_gen, next snd_gen) in
      f;;


    (* Applique un post-traitement à un générateur pseudo-aléatoire *)
    let map f gen =
      fun() -> f(next gen);;
    
    
    (* Applique un filtre p à un générateur pseudo-aléatoire *)
    let filter p gen = fun () ->
      let rec gen_filter () =
        let x = gen () in
          if p x then x
          else gen_filter ()
      in gen_filter ();;


    let partitioned_map p (fst_fun, snd_fun) gen =
      fun () ->
        let x = gen () in
        if p x then fst_fun x 
        else snd_fun x
  
  (* 
    let partmap = Generator.partitioned_map (fun x -> print_endline (string_of_int x); x mod 2 = 0) ((fun x -> x * 2),(fun x -> x - 1)) (Generator.int 0 100);;
    let test = Generator.next partmap;;

    let partmap = Generator.partitioned_map (fun x -> print_endline (string_of_float x); x > 25.) ((fun x -> x *. 2.),(fun x -> x -. 1.)) (Generator.float 0. 50.);;
    let test = Generator.next partmap;;
  *)

  end ;;
  
  