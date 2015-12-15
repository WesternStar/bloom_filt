let average a b = 
  let sum = a +. b in
  sum /. 2.0;;

let getBloomParams p items =
  let slots items p = 
    let s = (ceil ((-.items *. log p )/. ((log 2.0)**2.0 ))) 
    in int_of_float s in
  let hashes s items = 
    let h= ( ceil ( (float s *. log 2.0) /. items)) 
    in int_of_float h in
  let s = slots items p in
  let h = hashes s items in
  (s,h)

List.append(
