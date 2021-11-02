(* This file contains searching algorithms *)

(** [swap t i j] exchanges [t.(i)] and [t.(j)] *)
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;
  
(** [fusion_triée t1 t2] renvoie un tableau trié contenant les élements de t1 et t2, deux tableaux triés*)
let fusion_triee t1 t2 = 
let k1 = ref 0 in
let k2 = ref 0 in 
let f = Array.make ((Array.length t1) + (Array.length t2)) 0 in
for i=0 to Array.length f-1 do
if !k1 = Array.length t1 then (f.(i) <- t2.(!k2);k2 := !k2 +1)
else if !k2 = Array.length t2 then (f.(i) <- t1.(!k1);k1 := !k1 +1)
else if t1.(!k1)< t2.(!k2) then( f.(i) <- t1.(!k1);k1 := !k1 +1)
else (f.(i) <- t2.(!k2); k2 := !k2 +1)
done; f;;


(** [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then swap t j (j + 1)
      done
  done;;
  
(** [dicho t] renvoie le tableau t trié en utilisant la méthode dichotomique *)
let dicho t = 
    let rec inter t vb vh =
    if vh-vb=1 then fusion_triee [|t.(vb)|] [|t.(vh)|]
    else if vh-vb=0 then [|t.(vh)|]
    else fusion_triee (inter t vb ((vb+vh)/2)) (inter t ((vh+vb)/2 +1) vh)
    in inter t 0 (Array.length t-1);;
    
    
(** [tri_rapide t] renvoie le tableau t trié en utilisant la méthode de tri rapide *)
let tri_rapide t = 
    let rec inter t vmin vmax= 
        let a=ref vmin in
        if vmax-vmin<=0 then ()
        else 
        if vmax-vmin =1 then (if t.(vmax)<t.(vmin) then swap t vmin vmax)
        else (for i=vmin to vmax-1 do
                (if t.(i)<=t.(vmax) then (swap t i (!a); incr a))
            done;
        swap t vmax (!a);
        inter t vmin (!a-1); inter t (!a+1) vmax;)
    in inter t 0 ((Array.length t)-1);;
