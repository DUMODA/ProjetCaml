module Reduction :
sig
  (** Type d'une stratégie de réduction des éléments de type 'a
    * Une stratégie associe à chaque valeur une liste de propositions plus "simples".
    * NB : Les propositions sont ordonnées dans l'ordre croissance de "simplicité"
    *      (i.e. les valeurs les plus "simples" sont en début de liste).
    * IMPORTANT : Les stratégies implémentées respectent les conditions des générateurs correspondants.
    *)
  type 'a t = 'a -> 'a list

  (** La stratégie vide : ne renvoie aucune proposition de réduction *)
  val empty : 'a t

  (* TYPES DE BASE *)

  (** Stratégie de réduction sur les entiers
    * @param n entier
    * @return  liste d'entiers plus "simples" entre `-|n|` et `|n|`
    *)
  val int : int t

  (** Stratégie de réduction sur les entiers positifs
    * @param n entier positif
    * @return  liste d'entiers naturels plus "simples" entre 0 et `n`
    *)
  val int_nonneg : int t

  (** Stratégie de réduction sur les flottants
    * @param x flottant
    * @return  liste de flottants plus "simples" entre `-|x|` et `|x|`
    *)
  val float : float t

  (** Stratégie de réduction sur les flottants positifs
    * @param x flottant positif
    * @return  liste de flottants positifs plus "simples" entre `0` et `x`
    *)
  val float_nonneg : float t

  (** Stratégie de réduction sur les caractères
    * @param c caractère
    * @return  liste de caractères plus "simples"
    *)
  val char : char t

  (** Stratégie de réduction sur les caractères alphanumériques
    * @param c caractère alphanumérique
    * @return  liste de caractères alphanumériques plus "simples"
    *)
  val alphanum : char t

  (* CHAINES DE CARACTERES *)

  (** Stratégie de réduction sur les chaînes de caractères
    * @param red stratégie de réduction à utiliser sur chaque caractère
    * @param s   chaîne de caractères
    * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
    *)
  val string : char t -> string t

  (* LISTES *)

  (** Stratégie de réduction sur les listes
    * @param red stratégie de réduction à utiliser sur chaque élément
    * @param l   liste
    * @return    liste de listes plus "simples" au pire aussi longues que `l`
    *)
  val list : 'a t -> ('a list) t

  (* TRANSFORMATIONS *)

  (** Stratégie de réduction sur les couples
    * @param fst_red stratégie de réduction de la première coordonnée
    * @param snd_red stratégie de réduction de la deuxième coordonnée
    * @return        stratégie de réduction sur les couples correspondants
    *)
  val combine : 'a t -> 'b t -> ('a * 'b) t

  (** Applique un filtre à une stratégie de réduction
    * @param p   filtre à appliquer à chaque réduction
    * @param red stratégie de réduction
    * @return    stratégie de réduction ne contenant que des propositions vérifiant `p`
    *)
  val filter : ('a -> bool) -> 'a t -> 'a t
end =
struct
  Random.self_init ();;


  type 'a t = 'a -> 'a list ;;

  (* La stratégie vide *)
  let empty _ = [];;

  (* Stratégie de réduction sur les entiers *)
  let int n = 
    let rec range a b =
      a :: range (a+1) b in range (-(abs n)) (abs n);; 
  
  (* Stratégie de réduction sur les entiers positiifs *)
  let int_nonneg n = 
    List.filter (fun x -> x >= 0) (int n);;

  let char c =
    let lowercase = Char.lowercase_ascii c in
      let uppercase = Char.uppercase_ascii c in
        if lowercase = c then [c; uppercase]
          else if uppercase = c then [c; lowercase]
            else [c];;
      
  let alphanum c =
    let code = Char.code c in
      let codes =
        let range a b = List.init (b - a + 1) (fun i -> a + i) in
          let is_digit = code >= 48 && code <= 57 in
            let is_uppercase = code >= 65 && code <= 90 in
              let is_lowercase = code >= 97 && code <= 122 in
                match is_digit, is_uppercase, is_lowercase with
                  | true, _, _ -> let digits = range 48 57 in
                    digits @ range (code+1) 57 @ range 48 (code-1)
                  | _, true, _ -> let uppercase_letters = range 65 90 in
                    uppercase_letters @ range (code+1) 90 @ range 65 (code-1)
                  | _, _, true -> let lowercase_letters = range 97 122 in
                    lowercase_letters @ range (code+1) 122 @ range 97 (code-1)
                  | _, _, _ -> []
      in List.map Char.chr codes;;
  
  let string red s =
    let n = String.length s in
    let rec aux acc i =
      if i >= n then acc
      else
        let x = red s.[i] in
          let acc2 = List.map (fun c -> (String.make 1 c)) x in
        aux (acc @ acc2) (i+1)
      in List.rev(aux [s] 0);;
  
    let rec list red l = match l with
      | [] -> [[]]
      | x :: xs ->
        let rest = list red xs in
          let head = red x in
            List.concat (List.map (fun h -> List.map (fun r -> h :: r) rest) head);;
  
    let combine fst_red snd_red (x, y) =
      List.map (fun x' -> (x', y)) (fst_red x) @ List.map (fun y' -> (x, y')) (snd_red y);;
    
    
    let filter p red x =
      let rec aux acc = function
        | [] -> acc
        | hd :: tl ->
          if p hd then aux (hd :: acc) tl
          else aux acc tl
        in List.rev(aux [] (red x));;
      
end ;;
