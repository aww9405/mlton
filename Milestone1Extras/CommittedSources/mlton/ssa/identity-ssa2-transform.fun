functor IdentitySSA2Transform (S: SSA2_TRANSFORM_STRUCTS): SSA2_TRANSFORM = 
struct

open S

(* program as Program.T {datatypes, functions, globals, main} *)
fun transform2 (program as Program.T {datatypes, functions, globals, main}) = let val _ = 0 in print "Executed IdentitySSA2Transform.transform2\n"; program end

end
