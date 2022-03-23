module F = Format

type encoded_elem = int * string

let rec decode (lst : encoded_elem list) : string list = 
    let rec mk_list (acc: string list) (n:int) (s: string) : string list = 
        if n = 0 then acc
        else mk_list (s :: acc) (n-1) s
    in
    match lst with
    | [] -> []
    | (a, b)::t -> let newlst = mk_list [] a b in
                newlst @ (decode t)


let print_list (lst: string list) : unit = 
  F.printf "[%a]@." 
    (F.pp_print_list ~pp_sep:(fun fmt () -> F.fprintf fmt "; ")
    (fun fmt x -> F.fprintf fmt "%s" x)) lst

let _ =  
  print_list (decode [(1, "a"); (3, "b"); (0, "c")])
