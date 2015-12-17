
let get_bit x i =
  let mask = 1 lsl i in
  x land mask != 0;;

let set_bit x i =
  let mask = 1 lsl i in
  x lor mask;;

let get_int_array a i =
  a.(i mod Array.length a );;

let set_int_array a i x =
  a.(i mod Array.length a ) <- x;;
(* We are assuming 64bit *)
let get_bit_array  a i =
  let index= i lsr 6 in
  let x = get_int_array a index in
  get_bit x (i mod 64);;

let set_bit_array a i =
  let index= i lsr 6 in
  let x = get_int_array a index in
  set_int_array a index (set_bit x (i mod 64))


let getBloomParams p items =
  let slots items p = 
    let s = (ceil ((-.items *. log p )/. ((log 2.0)**2.0 ))) 
    in int_of_float s in
  let hashes s items = 
    let h= ( ceil ( (float s *. log 2.0) /. items)) 
    in int_of_float h in
  let s = slots items p in
  let h = hashes s items in
  (s,h);;

(*
record bloom_filter (p:float) (n:int) (random:bool) ?meaningful ?total =
    (* Unimplemented Methods*)
    (* TODO method reset
     * Clear whats in the array*)
       (*method add Add value to the bloom filter*)
  end;;
*)

type 'a bloom_filter =
  { slots: int;
    hash_count:int;
    hashes:('a ->int) list ;
    data : int array;
  }
let rec get_hashes n =    
      if n >0 then (Hashtbl.seeded_hash n) :: (get_hashes ((-) n 1) )
      else  [];;

let rec hash_value hashes x =
      match hashes with 
      | hd ::tl ->hd  x :: (hash_value (tl )) x 
      | [] -> [];;

let find bf x =
  let a= hash_value bf.hashes x in
  List.for_all (get_bit_array bf.data ) a;;

let add bf x =
  let a= hash_value bf.hashes x in
  List.iter (set_bit_array bf.data ) a;;

let create_bloom_filter p n =

  let (slots,hash_count) = (getBloomParams p n) in
  let l = int_of_float(
      ceil((float slots) /. 64.0)) in
  let data = Array.make l 0 in 
  let hashes = get_hashes hash_count in
  {
    slots;
    hash_count;
    data;
    hashes;
  }
;;


let read filename=
  let ic = open_in filename in
  let slots=input_value  ic  in
  let hash_count=input_value  ic  in
  let hashes=get_hashes hash_count in
  let data = input_value  ic  in
    close_in ic;
    {
      slots;
      hash_count;
      data;
      hashes;
    }
    ;;

let write bf filename =
  let oc = open_out filename in
  output_value  oc bf.slots;
  output_value  oc bf.hash_count;
  output_value  oc bf.data;
  close_out oc;
;;


let create   ?filename ?prob ?elements () =
  let bf = match filename,prob,elements with
    | Some filename,None,None -> read filename 
    | None,Some prob , Some elements -> create_bloom_filter prob elements 
    | _,_,_ -> failwith "No information Passed" in
  bf
;;
