functor IdentitySSATransform (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S

(* program as Program.T {datatypes, functions, globals, main} *)
fun transform (program as Program.T {datatypes, functions, globals, main}) = let val _ = 0 in print "Executed IdentitySSATransform.transform\n"; program end

end

