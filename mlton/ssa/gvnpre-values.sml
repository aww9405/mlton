(*
* VALUES
*)

structure Value: VALUE = struct
    datatype t = T of int
    (* fun new is intended to generate serial numbers *)
    fun equals (T{i1}, T{i2}) = (i1 = i2)
end


(*
* VALUE TABLES
*)

structure ValueTable:> VALUE_TABLE = struct
    (* was intended to use Trie *)
    datatype t = T of { values: (Expr.t, Value.t) Map.t }
    
    fun Expr_equals e1 e2 =
        Word.equals(Expr.hash(e1), Expr.hash(e2))
        
    fun new () = T {values = Map.new(Expr_equals)};
    
    fun add (T{values}, e, v) =
        Map.put(values, e, v)
        
    fun lookup (T{values}, e) =
        Map.get(values, e)
        
    fun lookup_or_add (T{values}, e) =
        let
            val result = Map.get(values, e)
        in
            case Map.get(values, e) of
                NONE => Map.put(values, e, Value.new()); result
              | SOME(v) => Map.put(values, e, v); result
            end
        end
end


(*
* VALUE REPRESENTATIVE SETS
*)

structure ValueSet:> VALUE_SET = struct
    datatype t = T of { table: ValueTable.t,
                        reps: (Value.t, Expr.t) Map.t }
                        
    fun new (vt) =
        T {table: vt, reps: Map.new(Value.equals)};
    
    fun get (T{table, reps}, v) =
        table.get(v);
        
    fun val_insert (T{table, reps}, e) =
        let
            val v_result = ValueTable.lookup(table, e)
        in
            case v_result of
                NONE => NONE
                SOME(v) =>
                    let
                        val e'_result = Map.get(reps, v)
                    in
                        case e'_result of
                            NONE => Map.put(reps, v, e); SOME(v)
                            SOME(_) => SOME(v)
                        end
                    end
            end
        end 
                        
        
    fun val_replace (T{table, reps}, e) =
        let
            val v_result = ValueTable.lookup(table, e)
        in
            case v_result of
                NONE => NONE
                SOME(v) => Map.put(reps, v, e); SOME(v)
            end
        end 
end

