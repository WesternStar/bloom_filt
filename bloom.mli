
type ('a) bloom_filter

val create : ?filename:bytes -> ?prob:float -> ?elements:int -> unit -> 'a bloom_filter
val add : 'a bloom_filter -> 'a -> unit 
val find : 'a bloom_filter -> 'a -> bool
val read : bytes -> 'a 
val write : 'a -> bytes -> unit 
