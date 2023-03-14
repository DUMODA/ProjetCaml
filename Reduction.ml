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
      if a > b then empty()
      else a :: range (a+1) b in range (-(abs n)) (abs n);;
  
  (* Stratégie de réduction sur les entiers positiifs *)
  let int_nonneg n = 
    let rec range a b =
      if a > b then empty()
      else a :: range (a+1) b in (range 0 n);;  

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

  (* Stratégie de réduction sur les caractères alphanumériques *)
  let rec alphanum c =
    if c = '0' then empty() 
    else let previous_c = (char_of_int(int_of_char(c) - 1)) in
      if ((int_of_char(previous_c) > 57 && int_of_char(previous_c) < 65) ||
          (int_of_char(previous_c) > 90 && int_of_char(previous_c) < 97))
      then alphanum previous_c 
      else List.append (alphanum previous_c) [previous_c];;
      
end ;;
