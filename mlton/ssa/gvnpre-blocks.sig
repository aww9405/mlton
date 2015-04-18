(*
BLOCK STORES
*
* Need a way of keeping track of block modifications and additions.
* 
*)

signature BLOCK_STORE = sig
    type t
    
    structure Ref: sig
        val get: t -> Block.t
        val put: t * Block.t -> unit
        val prepend: t * Statement.t -> unit option
        val append: t * Statement.t -> unit option       
    end
    
    val new: Block.t vector -> t
    val get: t * Label.t -> Ref.t
    val getByBlock: t * Block.t -> Ref.t
    val add: t * Block.t -> Ref.t
    val remove: Ref.t -> unit option
    
    val foreach: t * (Block.t -> unit) -> unit
    val toVector: t -> Block.t vector
end


signature 
