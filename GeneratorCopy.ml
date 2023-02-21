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
    
     (** Générateur pseudo-aléatoire d'entiers
      * @param a borne inférieure
      * @param b borne supérieure
      * @return  générateur pseudo-aléatoire de valeurs entières entre `a` et `b` inclus
      *)
    val int : int -> int -> int   t
  end =
  struct
    (* TODO : Implémenter le type et tous les éléments de la signature *)

    (*Initialise le générateur de nombres aléatoires*)
    Random.self_init ();;

    type 'a t = unit -> 'a

    (*Permet de générer un nombre aléatoire*)
    let next f = f ();;

    (*Générateur constant d'une entité quelconque*)
    let const x = fun () -> x;;

    (*Générateur pseudo-aléatoire d'entiers*)
    let int a b =
      let borne_sup = b - a + 1 in
        let gen_int () = a + Random.int borne_sup in
          gen_int;;
    

          
   
  end ;;
  
  (*
    APPELS DE METHODES : 
    let gen_int a b = Generator.int a b ;;
    Generator.next gen ;;

  *)


 (* type 'a t = unit -> 'a ;;
    
      let next gen = gen () ;;
    
      let const x = fun () -> x ;;
    
      let bool prob = fun () -> Random.float 1.0 < prob ;;
    
      let int a b = fun () -> a + (Random.int (b - a + 1)) ;;
    
      let int_nonneg n = int 0 n ;;
    
      let float x y = fun () -> x +. (Random.float (y -. x)) ;;
    
      let float_nonneg x = float 0.0 x ;;
    
      let char = fun () -> char_of_int (int 0 255 ()) ;;
    
      let alphanum = fun () -> char_of_int (int 48 122 ()) ;;
     
      let string n char_gen = fun () ->
        let rec make_string i acc =
          if i = n then acc
          else make_string (i+1) (acc ^ (String.make 1 (next char_gen)))
        in
        make_string 0 "" ;;
    
      let list n elem_gen = fun () ->
        let rec make_list i acc =
          if i = 0 then acc
          else make_list (i-1) ((next elem_gen) :: acc)
        in
        make_list n [] ;;
    
      let combine gen1 gen2 = fun () -> (next gen1, next gen2) ;;
    
      let map f gen = fun () -> f (next gen) ;;
    
      let filter p gen =
        let rec filter_gen () =
          let x = next gen in
          if p x then x
          else filter_gen ()
        in
        filter_gen ;;
    
      let partitioned_map p (f1, f2) gen = fun () ->
        let x = next gen in
        if p x then f1 x
        else f2 x ;;

        *) 