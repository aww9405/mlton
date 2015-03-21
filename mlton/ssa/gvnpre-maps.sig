(*
* MUTABLE MAPS
*)


signature MAP = sig
    type ('a, 'b) t

    new: {equals: 'a * 'a -> bool} -> ('a, 'b) t
    get: ('a, 'b) t * 'a -> 'b option
    put: ('a, 'b) t * 'a * 'b -> unit
    foreach: ('a, 'b) t * ('a * 'b -> unit) -> unit
end

