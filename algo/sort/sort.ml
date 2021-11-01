(* This file contains searching algorithms *)

(** [swap t i j] exchanges [t.(i)] and [t.(j)] *)
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;

(** [fusion_triée t1 t2] renvoie un tableau trié contenant les élements de t1 et t2, deux tableaux triés*)
let fusion_triée t1 t2 = 
let k1 = ref 0 in
let k2 = ref 0 in 
let f = Array.make lenght t1 + lenght t2 0 in
for i=0 to lenght f-1 do
if k2 = lenght t2-1 then f.(i) <- t1.(k1)
else f.(i)<-min t1.(k1) t2.(k2);;


(** [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then swap t j (j + 1)
      done
  done;;

  
