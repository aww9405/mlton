functor GVNPRE (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S


fun blocksAreSame ({label = label1, ...}, {label = label2, ...}) =
    Label.equals(label1, label2)
    

fun optimize_Dominator_Tree (bs: Block.t Tree.t): Block.t Tree.t =

    let
        val vt: ValueTable.t =
            ValueTable.new();
    
        val exp_gen: (Block.t, ValueSet.t) Map.t =
            Map.new(blocksAreSame)
        val phi_gen: (Block.t, ValueSet.t) Map.t =
            Map.new(blocksAreSame)
        val tmp_gen: (Block.t, ValueSet.t) Map.t =
            Map.new(blocksAreSame)
        val avail_out: (Block.t, ValueSet.t) Map.t =
            Map.new(blocksAreSame)
        val avail_in: (Block.t, ValueSet.t) Map.t =
            Map.new(blocksAreSame)
        val antic_out: (Block.t, ValueSet.t) Map.t =
            Map.new(blocksAreSame)
        val antic_in: (Block.t, ValueSet.t) Map.t =
            Map.new(blocksAreSame)
            
            
        fun buildSetsPhase1 () =
            let
                fun phiAction(b: Block.t, t: Exp.t) =
                    ValueTable.add(vt, t, Value.new());
                    ValueSet.val_insert(Map.get(phi_gen, b), t)
                    
                fun varAction(b: Block.t, t: Exp.t, t': Exp.t) =
                    let
                        val v = ValueTable.lookup(vt, t')
                    in
                        ValueTable.add(vt, t, v);
                        ValueSet.val_insert(Map.get(exp_gen, b), t');
                        ValueSet.val_insert(Map.get(tmp_gen, b), t)
                    end
                    
                fun opArgsAction(b: Block.t, t: Exp.t, _, ts: Exp.t vector) =
                    let
                        val vs = Vector.map(ts, fn ti =>
                          ValueTable.lookup(vt, ti))
                        (* v := lookup_or_add(op v1 v2 ... vn) *)
                    in
                        (* add(t, v) *)
                        Vector.foreach(ti,
                          fn ti=>ValueTable.lookup(Map.get(exp_gen, b),ti))
                        (* val_insert(EXP_GEN[b], op v1 v2 ... vn) *)
                        ValueSet.val_insert(Map.get(tmp_gen, b), t)
                    end
                    
                fun dotAction(_, t: Exp.t) =
                    let
                        val v = ValueTable.add(t, Value.new());
                    in
                        ValueSet.val_insert(Map.get(avail_out, b), t)
                    end 
            
                fun statementAction (b: Block.t, i: Statement.t) =
                    let Statement.T{t, gamma, ...} = i in
                        case exp of
                            Exp.ConApp{args, ...} =>
                                opArgsAction(Exp.Var(t), gamma,
                                  Vector.map(args, fn arg=>Exp.Var(arg))
                            Exp.PrimApp{args, ...} =>
                                opArgsAction(Exp.Var(t), gamma,
                                  Vector.map(args, fn arg=>Exp.Var(arg))
                            Exp.Var(var') =>
                                varAction(Exp.Var(t), gamma)
                        end;
                        ValueSet.val_insert(Map.get(avail_out, b), t);
                    end
            
                fun statementsLoop (b: Block.t, is: Statement.t vector) =
                    Vector.app(statementAction, is)
            
                fun blocksLoop' (dom_b_option: Block.t option,
                  Tree.T(b, subtrees): Block.t Tree.t) =
                    Map.put(exp_gen, b, ValueSet.new());
                    Map.put(phi_gen, b, ValueSet.new());
                    Map.put(tmp_gen, b, ValueSet.new());
                    case dom_b_option of
                        NONE => ();
                      | SOME(dom_b) =>
                          Map.put(avail_out, b, Map.get(avail_out, dom_b));
                    end;
                    let Block.T{statements} in
                        statementsLoop(statements);
                    end;
                    Tree.Seq.foreach(subtrees,
                      fn (subtree) => blocksLoop(SOME(b), subtree))
                      
                fun blocksLoop () =
                    blocksLoop' (bs, NONE)
            in
                blocksLoop()
            end

        fun buildSetsPhase2 () =
            let
                fun blocksLoop' (dom_b_option: Block.t option,
                  Tree.T(b, subtrees): Block.t Tree.t) =
                  
                    (* insert action of loop over blocks here *)
                  
                    Tree.Seq.foreach(subtrees,
                      fn (subtree) => blocksLoop(SOME(b), subtree))
                      
                fun blocksLoop () =
                    blocksLoop' (bs, NONE)
            in
                blocksLoop()
            end
        
        (* to be implemented later *)    
        fun Insert(bs: Block.t Tree.t): Block.t Tree.t = bs
        
        (* to be implemented later *)    
        fun Eliminate(bs: Block.t Tree.t): Block.t Tree.t = bs

    in
        buildSetsPhase1();
        buildSetsPhase2();
        let
            val bs' = Insert(bs)
            val bs'' = Eliminate(bs')
        in
            return bs''
        end
    end 
        
end   


(*
*   A function is optimized by optimizing the dominator tree of its blocks,
*   and then reconstructing the function from its optimized dominator tree.
*)
fun optimize_Function (f: Function.t): Function.t =
    let
        val {args, mayInline, name, raises, returns, start, ...} =
            Function.dest(f)
        val dt' =
            optimize_Dominator_Tree(Function.dominatorTree(f));
        val blocks =
            Vector.fromList(Tree.foldPre(dt', [], fn(b, bs)=>b::bs))
    in
        Function.new {args=args, blocks=blocks, mayInline=mayInline,
          name=name, raises=raises, returns=returns, start=start};        
    end


(*
*  A program is optimized by optimizing each function,
*  and then reconstructing the program from the optimized functions.
*)
fun optimize_Program (p: Program.t) : Program.t =
    let
        val { datatypes, functions, global, main } = p
        val functions' = Vector.map(optimize, functions)
    in
        Program.T { datatypes=datatypes, functions=functions',
          global=global, main=main }
    end


fun transform (p: Program.t): Program.t =
    optimize_Program(p)

(*
* fun transform (program as Program.T {datatypes, functions, globals, main}) = let val _ = 0 in print "Executed GVNPRE.transform\n"; program end
*)

end

