functor GVNPRE (S: SSA_TRANSFORM_STRUCTS): SSA_TRANSFORM = 
struct

open S


fun blocksAreSame ({label = label1, ...}, {label = label2, ...}) =
    Label.equals(label1, label2)
     
    

fun optimize_Blocks (blockStore: BlockStore.t,
  dominatorTree: Block.t Tree.t, postDominatorTree: Block.t Tree.t): unit =

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
            
            
        (* Not yet implemented *)
        fun phi_translate(vs: ValueSet.t, pred_b: Block.t,
          succ_b: Block.t): ValueSet.t = vs;
          
        (* Not yet implemented *)
        fun clean(vs: ValueSet.t): ValueSet.t = vs;
        
        (* Not yet implemented *)
        fun find_leader(vs: ValueSet.t, e: Exp.t): Var.t option =
          SOME(ID.fromString "default_leader");
          
                      
            
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
                      fn (subtree) => blocksLoop'(SOME(b), subtree))
                      
                fun blocksLoop () =
                    blocksLoop' (bs, NONE)
            in
                blocksLoop()
            end

        fun buildSetsPhase2 () =
            let
                val old = ref ValueSet.new();
                
                fun successorBlocksLoop(succ_b: Block.t Tree.Seq.t): unit =
                
                    let
                        val worklist = WorkList.makelist(succ_b)
                        val first = WorkList.remove(worklist)
                    in
                        antic_out := Map.get(antic_in, first);
                        WorkList.foreach(worklist, fn b =>
                          ValueSet.foreach(!antic_out, fn e =>
                            let
                                val v = ValueSet.lookup(e)
                            in
                                case find_leader(Map.get(antic_in, b), v) of
                                    NONE =>
                                    antic_out := ValueSet.remove(!antic_out, e);
                                    SOME _ => ()
                                end
                            end
                    end
            
                fun blocksLoop (bs as Tree.T{b, subtrees}) =
                    
                        let
                             val succ_b = Tree.Seq.map(subtrees,
                              fn Tree.T{b, _} => b)
                            val length_succ_b = Tree.Seq.fold(succ_b, 0,
                              fn (_, n) => n + 1)
                        in
                            if length_succ_b = 0 then
                                antic_out := ValueSet.new();
                            else if length_succ_b = 1 then
                                Tree.Seq.foreach(succ_b, fn s =>
                                  antic_out := phi_translate(
                                  Map.get(antic_in, b), b, s))
                            else
                                successorBlocksLoop(succ_b)
                        end;
                    
                        let
                            val s = ValueSet.difference(!antic_out,
                                Map.get(tmp_gen, b))
                        in
                            Map.put(antic_in, b,
                              ValueSet.difference(
                                Map.get(exp_gen, b), Map.get(tmp_gen, b)));
                            ValueSet.foreach(vs, fn e =>
                                case find_leader(Map.get(antic_in, b),
                                  ValueTable.lookup(vt, e)) of
                                    NONE =>
                                        ValueSet.val_insert(
                                          Map.get(antic_in, b), e)
                                    SOME (_) => ()
                                end);
                            Map.put(antic_in, b,
                              clean(Map.get(antic_in, b));
                        end;
                    
                        if ValueSet.equals(!old, Map.get(antic_in, b)) then
                            Tree.Seq.foreach(subtrees,
                              fn (subtree) => blocksLoop(subtree))
                        else ()

            in
                blocksLoop(bs)
            end
        
        fun insert(bs: BlockStore.t, dt: Block.t Tree.t): unit =
            let
                
                val new_sets: (Block.t, ValueSet.t) Map.t =
                    Map.new(blocksAreSame)

                fun anticInLoop' (e: Expr.t, dom_b_option: Block.t option,
                  Tree.T{b, subtrees}: Block.t Tree.t): bool =
            
                  
                    let
                        val avail: (Block.t, Expr.t) Map.t =
                          Map.new(blocksAreEqual);
                        val by_some = ref false
                        val all_same = ref true
                        val first_s_option: Expr.t option ref = ref NONE
                        val pred_b: Block.t Tree.Seq.t = 

                        fun predecessorBlocksLoop1 (b': Block.t): unit =
                          
                          let
                            val e' = phi_translate(e, b', b)
                            val v' = ValueSet.lookup(e')
                            val e''_option = find_leader(Map.get(
                               avail_out, b'), v')
                          in
                            case e''_option of
                                NONE =>
                                    Map.put(avail, b', e');
                                    all_same := false;
                                SOME(e'') =>
                                    Map.put(avail, b', e'');
                                    by_some := true;
                                    case !first_s_option of
                                        NONE =>
                                            first_s_option := SOME(e'')
                                        SOME(_) =>
                                            all_same := false
                                    end
                            end
                          end
            
                
                        fun predecessorBlocksLoop2 (b': Block.t): unit =
                          
                          let
                            val e' = Map.get(avail, b')
                            fun f(ty, args) : unit =
                              let
                                val s_exprs = Vector.map(fn v =>
                                    Exp.Var(find_leader(
                                      Map.get(avail_out, b'), v)))
                                val t = get_fresh_temp();
                                val hoistedStatement = Statement.T{var=SOME(t),
                                  ty=ty, args=s_exprs}
                                val v = ValueTable.lookup_or_add(vt, e')
                              in
                                BlockStore.append(bs, hoistedStatement);
                                ValueTable.add(vt, v, t);
                                ValueSet.val_insert(Map.get(avail_out, b'), t);
                                Map.put(avail, b', t);
                              end
                          in
                            case e' of
                                ConApp {ty, args, ...} => f(ty, args)
                                PrimApp {ty, args, ...} => f(ty, args)
                                _ => ()
                            end
                          end

                    in
                        Tree.Seq.foreach(pred_b, fn b' =>
                            predecessorBlocksLoop1(b');
                    
                        if not all_same and by_some then
                        
                            Tree.Seq.foreach(pred_b, fn b' =>
                                predecessorBlocksLoop1(b'));
                            
                            let
                                val t = get_fresh_temp()
                            in
                                ValueTable.add(vt, t,
                                  ValueTable.lookup(vt, e));
                                ValueTable.val_insert(Map.get(
                                  avail_out, b), t);
                                (* insert a phi function instruction *)
                                ValueTable.val_insert(Map.get(
                                  new_sets, b), t)
                            end
                        
                        else ()
                    end
                    
                fun anticInLoop (dom_b_option: Block.t option,
                  Tree.T{b, subtrees}: Block.t Tree.t): bool =
                  
                    let
                        worklist = WorkList.makelist(
                          Map.get(antic_in, b));
                    in
                        worklist.fold(worklist, false, fn(e, new_stuff) =>
                          let
                            val new_stuff' = anticInLoop'(e, dom_b_option, dt)
                          in
                            new_stuff' orelse new_stuff
                          end)
                    end
            
                fun blocksLoop' (dom_b_option: Block.t option,
                  dt as Tree.T(b, subtrees): Block.t Tree.t) =
                    Map.put(new_sets, b, ValueSet.new());                
                    case dom_b_option of
                        NONE => ();
                      | SOME(dom_b) =>
                          ValueSet.foreach(Map.get(new_sets, dom_b),
                            fn e =>
                                ValueSet.val_insert(
                                  Map.get(new_sets, b), e);
                                ValueSet.val_replace(
                                  Map.get(avail_out, b), e));
                        if (* pred(b) > 1 *) then
                            let
                                val new_stuff = anticInLoop(dom_b, dt)
                            in
                                if new_stuff then
                                    Tree.Seq.foreach(subtrees,
                                      fn (subtree) =>
                                        blocksLoop'(SOME(dom_b), subtree))
                                else ()
                            end
                        else ()
                    end;
                        

                      
                fun blocksLoop () =
                    blocksLoop' (bs, NONE)
                                     
            in
                blocksLoop(dt)
            end
          
        
        fun eliminate(bs: BlockStore.t): unit =
        
            let
            
                fun changedStatement (b: Block.t, i: Statement.t,
                  t: Var.t): Statement.t =
                    let
                        val s'_option = find_leader(Map.get(avail_out, b),
                          ValueTable.lookup(vt, t))
                    in
                        case s'_option of
                            SOME(s') =>
                                if ID.equals(s', t) then
                                    let {ty, ...} = i in
                                     Statement.T{var=t, ty=ty, exp=Exp.Var(s')}
                                    end
                                else i
                            NONE => i
                        end
                    end
            
            in
        
                BlockStore.foreach(bs, fn b as {args, label, statements,
                  transfer} =>
                let 
                    val changedStatements =
                        Vector.map(statements, fn i as {t, exp, ...} =>
                            case exp of
                                Exp.ConApp _ => changedStatement(b, i, t)
                                Exp.PrimApp _ => changedStatement(b, i, t)
                            end)
                    val changedBlock = Block.T{args=args, label=label,
                      statements=newStatements, transfer=transfer}
                in
                    BlockStore.Ref.put(BlockStore.getByBlock(bs, b),
                      changedBlock)
                end
                
            end
                    
    in
        buildSetsPhase1(dominatorTree);
        buildSetsPhase2(postDominatorTree);
        insert(blockStore, dominatorTree);
        eliminate(blockStore);
    end 
        
end   


(*
*   A function is optimized by optimizing its control-flow graph of blocks,
*   then reconstructing the function from the optimized control-flow
*   graph.  New blocks must be constructed for the optimized graph.
*
*   The blocks are represented by the block store;
*   the control-flow graph is represented by the dominator and
*   post-dominator trees that are derived from it.
* 
*)
fun optimize_Function (f: Function.t): Function.t =
    let
        val {args, blocks, mayInline, name, raises, returns, start} =
            Function.dest(f)
        val blockStore = BlockStore.new(blocks);
        val dominatorTree = Function.dominatorTree(f);
        (* Incorrect; need to compute the post-dominator tree instead *)
        val postDominatorTree = Function.dominatorTree(f);
    in
        optimize_Blocks(blockStore, dominatorTree, postDominatorTree);
        Function.new {args=args, blocks=blockStore.toVector(),
          mayInline=mayInline, name=name, raises=raises,
          returns=returns, start=start};        
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

