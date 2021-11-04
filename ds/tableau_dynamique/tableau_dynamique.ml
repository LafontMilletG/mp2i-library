type 'a stack_imperative = {
    length : unit -> int;
    append : 'a -> unit;
    get : int -> 'a;
    set : int -> 'a -> unit;
    del : int -> unit;
    conc : 'a list -> unit;
    tolist : unit -> 'a list;
};;


let tableau_dynamique_de_liste l=
let base = ref l in
{
length = (fun () -> List.length !base);
append=(fun e ->base:= List.append !base [e]);
get = (fun n -> let rec inter n base = if n = 0 then List.hd base else inter (n-1) (List.tl base) in inter n !base);
set = (fun n e -> 
let rec inter n e base = if n=0 then e::(List.tl base) else (List.hd base)::(inter (n-1) e (List.tl base)) in base:= (inter n e !base));
del =(fun n  -> 
let rec inter n  base = if n=0 then List.tl base else (List.hd base)::(inter (n-1) (List.tl base)) in base:= (inter n !base));
conc = (fun l -> base:= List.append !base l);
tolist =(fun () -> !base);
};;

