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


  (*FONCTIONNALTÉS SUPPLEMENTAIRES*)

  (** Applique une réduction similaire à un fold_left sur un tableau initialisé suivant un opérateur
    * @param arr Un tableau de type 'a array à réduire
    * @param init La valeur initiale de type 'b pour l'accumulateur
    * @param op Un opérateur de type ('b -> 'a -> 'b) pour appliquer la réduction
    * @return La valeur finale de type 'b après avoir appliqué l'opérateur de réduction sur tous les éléments du tableau
    *)
  val array : 'a array -> 'b -> ('b -> 'a -> 'b) -> 'b

  (** Stratégie de réduction sur les chaînes de caractères version 2. Cette version n'utilise pas de reducteur de chaine de caractere.
    * @param s   chaîne de caractères
    * @return    liste de chaînes de caractères plus "simples" au pire aussi longues que `s`
    *)
    val string_v2 : char t -> string t

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
   

  (* Stratégie de réduction sur les flottants *)
  let float x =
    let rec range a b =
      if a > b then empty()
      else a :: range (a +. 1.) b in range (-. (abs_float x)) (abs_float x);;


  (* Stratégie de réduction sur les flottants positifs *)
  let float_nonneg x =
    let rec range a b =
      if a > b then empty()
      else a :: range (a +. 1.) b in (range 0. x);; 


 (* Stratégie de réduction sur les caracteres *)
  let rec char c =
    let previous_c = (char_of_int(int_of_char(c) - 1)) in
      if (int_of_char(c) > 65 && int_of_char(c) < 91) then
        List.append (char previous_c) [previous_c]
      else if (int_of_char(c) > 97 && int_of_char(c) < 123) then
        List.append (char previous_c) [previous_c]
      else empty();;
  

  (* Stratégie de réduction sur les caractères alphanumériques *)
  let rec alphanum c =
    let previous_c = (char_of_int(int_of_char(c) - 1)) in
      if (int_of_char(c) > 48 && int_of_char(c) < 58) then
        List.append (alphanum previous_c) [previous_c]
      else char c;;


  (* Stratégie de réduction sur les chaînes de caractères *)
  let string red s =
    let n = String.length s in
    let rec aux acc i =
      if i >= n then acc
      else
        let x = red (String.get s i) in
          let acc2 = List.map (fun c -> (String.make 1 c)) x in
        aux (acc @ acc2) (i+1)
      in List.rev(aux [s] 0);;
  

  (* Stratégie de réduction sur les listes *)
  let rec list red l = match l with
    | [] -> [[]]
    | x :: xs ->
      let rest = list red xs in
        let head = red x in
          List.concat (List.map (fun h -> List.map (fun r -> h :: r) rest) head);;
  

  (* Stratégie de réduction sur les couples *)
  let combine fst_red snd_red (x, y) =
    List.map (fun x2 -> (x2, y)) (fst_red x) @ List.map (fun y2 -> (x, y2)) (snd_red y);;
  

  (* Applique un filtre à une stratégie de réduction *)
  let filter p red x =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
        if p hd then aux (hd :: acc) tl
        else aux acc tl
      in List.rev(aux [] (red x));;
      

  (*Applique une reduction similaire à un fold_left sur un tableau initialisé avec suivant un operateur*)
  let array arr init op =
    let n = Array.length arr in
    let rec reduce i acc =
      if i < n then reduce (i + 1) (op acc arr.(i))
      else acc
    in
    reduce 0 init;;


  (* Stratégie de réduction sur les chaînes de caractères Version 2 -> n'utilise pas de reducteur*)
  let string_v2 red s =
    let n = String.length s in
    if n = 0 then empty()
    else let rec loop acc i =
        if i >= (n-1) then acc
        else loop (String.sub s 0 (n-i-1)::acc) (i+1) in
          loop [String.sub s 0 (n-1)] 1;;
end ;;