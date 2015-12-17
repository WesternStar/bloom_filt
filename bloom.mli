
type ('a) t

val create : unit -> ('a) t
val add :'a t -> key:'a -> unit
val find : 'a t -> 'a -> bool
val reset :unit -> ('a) t  
