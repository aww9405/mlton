(*
* MUTABLE MAPS
*)

structure Map:> MAP = struct
    datatype ('a, 'b) t =
      T of {equals: 'a * 'a -> bool,
            entries: ('a, 'b) ref list ref}

    fun new = T{equals = equals, entries=([] ref)}
    
    fun get ({equals, entries, ...}, key) =
        let
            val result = List.find(
              fn valueRef => equals(key, !valueRef), !entries)
        in
            case result of
                NONE => NONE
              | SOME(valueRef) => SOME(!valueRef)
            end
        end
    
    fun put ({equals, entries, ...}, key, value) =
        let
            val result = List.find(
              fn valueRef => equals(key, !valueRef), !entries)
        in
            case result of
                NONE => NONE
                    entries := (key, newValue)::!entries
              | SOME(valueRef) =>
                    valueRef := newValue
            end
        end
        
    fun foreach ({equals, entries, ...}, f) =
        List.app(f, !entries}
end

