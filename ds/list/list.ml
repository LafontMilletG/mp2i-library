(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;

(** [indice l e] renvoie la position de e dans le liste l et -1 si e n'appartient pas à l *)
let indice l e =
    let rec inter l e k = match l with 
        | [] -> -1
        | p::q -> if p=e then k else int q e k+1 
    in inter l e 0;;
      
(** [inverse l e] renvoie la liste l inversée. *)      
let inverse l = 
    let rec inter l k = match l with 
        | [] -> k
        | p::q -> inter q p::k
    in inter l [];;
    
(** [croissante l] renvoie true si la liste d'entiers ou de flottants l est croissante et false sinon  *)   
let croissante l = 
    let rec inter l prec = match l with
        | [] -> true
        | e::q -> if prec <= e then inter q e else false
    in inter l min_int
    

   

