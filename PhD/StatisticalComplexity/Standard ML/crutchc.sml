(* ------------------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)
(* Software to compute Jim Crutchfield's "Statistical Complexity"      *)
(* by Jordi Delgado (jdelgado@lsi.upc.es, LSI, FIB, UPC) 1995          *)
(*     This program assumes the existence of another C program         *)
(*     called BTree, that builds the tree from Data File faster        *)
(*     than the previous Standard ML version of the algorithm          *)
(*                          Versio 2.5                                 *)
(* ------------------------------------------------------------------- *)
(* ------------------------------------------------------------------- *)


(* Software from the book "Automatas Finitos con Programacion Funcional" *)
(* by Antonio Blanco, Gilberto Perez and Jose Santiago Jorge             *)
(* Universidade da Corunya, Servicio da Publicacions, Setembro 1993      *)
(* ------------------------------------------------------------------- *)
(*load "automata"*); 
open automata;
(* ------------------------------------------------------------------- *)

(* I need some functions from Moscow ML library *)
(* ------------------------------------------------------------------- *)
(*load "Mosml";*)
open Mosml; 
(* ------------------------------------------------------------------- *)

(*load "Random";*) (* I assume that random number generator is good enough *)
open Random;   

val gene = newgen();

open List;

(* ----------------- epsilon-Machine Reconstruction ------------------ *)

(* Miscellanea *)

fun max (a : int) b = if a < b then b else a;

(* Tree building functions, with labelling according to Morphs *)

datatype BTree = nul | Node of STATE * real * BTree * BTree;


local
   fun BTInsertString (c, nul,              nil)    = Node(State "", c, nul, nul)
    |  BTInsertString (c, Node(x, n, a, b), nil)    = Node(x, n, a, b)
    |  BTInsertString (c, nul     ,      (x::s))    = 
                        if x="0" then 
                           Node(State "", 0.0, BTInsertString (c, nul, s), nul)
                        else   
                           Node(State "", 0.0, nul, BTInsertString (c, nul, s))
    |  BTInsertString (c, Node(x, n, a, b), (y::s)) = 
                        if y="0" then 
                           Node(x, 0.0, BTInsertString (c, a, s), b)
                        else
                           Node(x, 0.0, a, BTInsertString (c, b, s));

   fun BTMakeTree(file_in,T) = 
              if not (end_of_stream file_in) then
                 let val Temp1 = explode(input_line(file_in))
                     val DtaLst = take(Temp1,length(Temp1)-1)
                     val Temp2 = explode(input_line(file_in))
                     val Cnt = real(BuildNumber(implode(take(Temp2,length(Temp2)-1))))
                 in
                     BTMakeTree(file_in, BTInsertString(Cnt,T,DtaLst))
                 end
              else
                 T
in
   fun BTConstructInitialTree(file_in) =
       let val fil_in = open_in(file_in) in
            BTMakeTree(fil_in,nul)
       end
end;


fun BTMakeCounters (Node (S, c, nul, nul)) = Node (S, c, nul, nul)
 |  BTMakeCounters (Node (S, c, nul, a))   = 
                        let val Node(S1, C1, a1, b1) = BTMakeCounters(a) in
                            Node(S,C1,nul,Node(S1,C1,a1,b1))
                        end
 |  BTMakeCounters (Node (S, c, a, nul))   =
                        let val Node(S1, C1, a1, b1) = BTMakeCounters(a) in
                            Node(S,C1,Node(S1,C1,a1,b1),nul)
                        end
 |  BTMakeCounters (Node (S, c, a, b))     =
                        let val Node(S1, C1, a1, b1) = BTMakeCounters(a) 
                            val Node(S2, C2, a2, b2) = BTMakeCounters(b) in
                            Node(S,C1+C2,Node(S1,C1,a1,b1),Node(S2,C2,a2,b2))
                        end;


(* We will assume that the tree is already constructed in a file *)

fun BTConstructTree (file_in) = BTMakeCounters(BTConstructInitialTree(file_in));


fun BTDepth (nul)                = 0
 |  BTDepth (Node (_,_,nul,nul)) = 0
 |  BTDepth (Node (s, i, a, b))  = 1 + max (BTDepth a) (BTDepth b);


fun BTMorphExtract (l, nul)            = nul
 |  BTMorphExtract (l, Node (s,i,a,b)) = 
                        if l = 0 then
                           Node (s,i,nul,nul)
                        else
                           Node (s,i,BTMorphExtract(l-1,a),
                                     BTMorphExtract(l-1,b));


fun BTCompareTopol (nul, nul)                      = true
 |  BTCompareTopol (nul, Node(_,_,_,_))            = false
 |  BTCompareTopol (Node(_,_,_,_),   nul)          = false 
 |  BTCompareTopol (Node(_,_,a,b), Node(_,_,c,d))  = BTCompareTopol(a,c) 
                                                      andalso 
                                                     BTCompareTopol(b,d);

(* This function assumes topological equality between the Morphs *)

local
   local
     fun Gen' (nil)   = nil
      |  Gen' (x::xs) = if x = "0" then
                           "1"::xs
                        else
                           "0"::Gen' xs
   in
      fun Gen (l) = if exists (fn n => n = "0") l then
                        rev (Gen' (rev l))
                    else
                        l
   end

   fun LastCounter (l, nul)                  = 0.0
    |  LastCounter (nil, Node (s,f,a,b))     = f
    |  LastCounter (x::xs, Node(s,f,a,b))    = if x = "0" then
                                                  LastCounter (xs, a)
                                               else
                                                  LastCounter (xs, b)
   
   fun Prob (w, Node(s,f,a,b)) = if (f <> 0.0) then
                                    LastCounter (w,Node(s,f,a,b))/f
                                 else 
                                    0.0

   fun CompareProb(delta, w, m1, m2) = 
                                (abs(Prob(w,m1)-Prob(w,m2)) < delta) andalso
                                (if  exists (fn n => n = "0") w then
                                     CompareProb(delta, Gen(w), m1, m2)
                                 else
                                     true
                                )
   
in
  fun BTCompareProb (delta, m1, m2) = 
             CompareProb(delta, tabulate(BTDepth m1, (fn n => "0")), m1, m2)
end;


fun BTCompare (delta, M1, M2) = let val T = BTCompareTopol(M1, M2) in
                                    if T then
                                       BTCompareProb(delta, M1, M2)
                                    else
                                       false
                                end;


fun BTMember (_, _, nil)     = false
 |  BTMember (delta, A, x::xs)   = BTCompare(delta, A, x) orelse 
                                   BTMember(delta, A, xs);

 
fun BTMorphLabel (delta, m, nil)   = 1
 |  BTMorphLabel (delta, m, x::xs) = if BTCompare(delta, m, x) then
                                        1
                                     else
                                        1 + BTMorphLabel (delta, m, xs);


(* An Input Tree for MakeAutom requires a labelled Tree *)
                                        
fun BTTreeLabel (N, L, D, delta, nul, l)            = nul
 |  BTTreeLabel (N, L, D, delta, Node(s,i,a,b), l)  = 
                   let val m' = BTMorphExtract (D, Node(s,i,a,b)) 
                       val l' = if BTMember(delta, m',l) then
                                   l
                                else
                                   l @ (m'::nil)
                       val s' = State (BuildString(BTMorphLabel(delta,m',l')))
                       val t = N - L + 1
                   in
                       if BTDepth m' = D then
                          Node(s', i/(real t), BTTreeLabel(N,L,D,delta,a,l'), 
                                               BTTreeLabel(N,L,D,delta,b,l'))
                       else
                          nul
                   end;



(* Common standard values *)
                           
val zero = Input "0" 
and one  = Input "1";

val INPUTS = Union (Single zero) (Single one);

val INITIAL = State "1";


(* Functions to build finite automata from the Morph Labelled Tree *)

local 
   fun BTMkListStates (nul)           = nil
    |  BTMkListStates (Node(s,i,a,b)) = [s] @ BTMkListStates(a) 
                                            @ BTMkListStates(b)

   fun BTBuildStates A = MkSet (RemDupl (BTMkListStates(A)))

   fun BTMkListTrans (nul)                           = nil
    |  BTMkListTrans (Node(s,i,nul,nul))             = nil
    |  BTMkListTrans (Node(s1,j,Node(s2,i,a,b),nul)) = 
                               (s1,zero,i/j,s2)::BTMkListTrans(Node(s2,i,a,b))
    |  BTMkListTrans (Node(s1,j,nul,Node(s2,i,a,b))) = 
                               (s1,one,i/j,s2)::BTMkListTrans(Node(s2,i,a,b))
    |  BTMkListTrans (Node(s,j,Node(s1,i1,a1,b1),Node(s2,i2,a2,b2))) = 
                                          [(s,zero,i1/j,s1),(s,one,i2/j,s2)] @
                                          BTMkListTrans(Node(s1,i1,a1,b1)) @
                                          BTMkListTrans(Node(s2,i2,a2,b2))

   local
      fun Mem' nil _                               = false
       |  Mem' ((s1,i,p,s2) :: xs) (s1',i',p',s2') = ((s1 = s1') andalso
                                                      (s2 = s2') andalso
                                                      (i = i')) orelse
                                                      Mem' xs (s1',i',p',s2')

      fun RemDupl' nil       = nil
       |  RemDupl' (x :: xs) = if Mem' xs x then
                                 RemDupl' xs
                               else
                                 x :: RemDupl' xs
   in
     fun BTBuildTransitions A = MkSet (RemDupl' (rev (BTMkListTrans (A))))
   end
in

   (* This function assumes that the tree A is already labelled *)

   fun BTBuildAutomaton A = let val S = BTBuildStates A in
                                  MakeAutom
                                  ( S,
                                    INPUTS,
                                    BTBuildTransitions A,
                                    INITIAL,
                                    S
                                  )
                              end
end;


local 
   fun IndetTransitions ((s1,n,_,s2), (e1,m,_,e2)) = 
                        (s1 = e1) andalso (n = m) andalso (s2 <> e2) 
   fun IndetList (x, nil)    = false
    |  IndetList (x, y::ys)  = IndetTransitions(x, y) orelse
                               IndetList(x, ys)
   fun Indet (nil)   = false
    |  Indet (x::xs) = IndetList (x, xs) orelse Indet (xs) 
in
   fun Indeterminist A = let val (_,_,T,_,_) = Components A in
                             Indet (MkList' T)   
                         end
end


(* -------------- Markov Matrix Construction -----------------------*)

local
   local
      fun ModifyMatrix (0, r, x::xs) = r::xs
       |  ModifyMatrix (p, r, x::xs) = x::ModifyMatrix(p-1,r,xs)
   in
       fun ConstructMatrix (Siz, nil,                          L) = L
        |  ConstructMatrix (Siz, (State p, w, r, State q)::xs, L) =
            let val L' = ModifyMatrix (((BuildNumber p)-1)*Siz+
                                       ((BuildNumber q)-1), r, L) in
                ConstructMatrix (Siz, xs, L')
            end
   end
in
   fun TransitionMatrix A = 
            let val (Q,_,T,_,_) = Components A 
                val S = Cardinality Q 
            in
                (ConstructMatrix (S,
                                  MkList' T,
                                  tabulate(S*S, (fn n => 0.0))),
                 S)
               
            end
end 


local 
   fun Transition (z, i, j, (M,S)) = 
        let val t = nth(M,i*S+j) in
            if z < t then
               j
            else
               Transition (z-t, i, j+1, (M,S))
        end

   fun StateTransition(NMoves, i, M, Pr) =
          let val j = Transition(random gene, i, 0, M) in
               if NMoves = 0 then
                  Pr
               else
                  StateTransition (NMoves-1, j, M, 
                                   take(Pr, j) @ [nth(Pr, j) + 1.0] 
                                               @ drop(Pr, j + 1))
          end
in
   fun ProbDist (NMoves,(M,S)) = 
        let val P = StateTransition (NMoves,0,(M,S),
                                     [1.0] @ tabulate(S-1,(fn n => 0.0))) in
            map (fn x => x / (real NMoves)) P
        end
end;


fun Entropy V =
    foldl (fn (x, y : real) => x + y) 0.0 
              (map (fn x => ~x * ln x / ln 2.0) (filter (fn x => x <> 0.0) V));


local
   fun TreeProbDist (nul)                  = nil
    |  TreeProbDist (Node(_, n, nul, nul)) = [n]
    |  TreeProbDist (Node(_, _, a, b))     = TreeProbDist a @
                                             TreeProbDist b
in
   fun MetricEntropy (L, N, Tree) =
           (Entropy (map (fn x => x / (real N)) (TreeProbDist Tree))) / (real L)
end;           


fun StatisticalComplexity (StochAutom, NMoves) =
      let val Matrix = TransitionMatrix StochAutom in
             if Indeterminist(StochAutom) then
                ~1.0
             else
                Entropy(ProbDist(NMoves,Matrix)) 
      end;


fun SaveMachine (file_out, L, D, delta, NMoves, A, iA, MetEnt, StatComp) =
         let val StringToPrint = "PARAMETERS:\n" ^ 
                                 "L = " ^ makestring L ^ "\n" ^ 
                                 "D = " ^ makestring D ^ "\n" ^
                                 "d = " ^ makestring delta ^ "\n\n" ^
                                 "AUTOMATON:" ^ "\n" ^
                                 "Indeterminstic --> " ^ 
                                 (if iA then "Yes" else "No") ^ "\n" ^
                                 PrintAutom A ^ "\n\n" ^
                                 "RESULTS:" ^ "\n" ^
                                 "Metric Entropy = " ^
                                 makestring MetEnt ^ "\n"
         in 
             if (StatComp = ~1.0) then
                output(file_out, StringToPrint ^ 
                                 "Statistical Complexity Not Defined")
             else
                output(file_out, StringToPrint ^ 
                                 "Statistical Complexity = " ^
                                 makestring StatComp)
         end;



(*--------------------------------------------------------------------------- 
 User's Manual:

 1.- Use a fast C Program "BTree" to create a Tree:

     BTree <DataFile> <L> <TreeFile>

 2.- Use a Standard ML "crutchc" program:

     crutchc <TreeFile> <delta> <NMoves> <OutputFile>

 (we assume D := L div 2 by default)
--------------------------------------------------------------------------*)



val commandLine = argv();

val Tree = BTConstructTree(nth(commandLine,1));

val L = BTDepth Tree;
val D = L div 2;
val N = let val Node(_,c,_,_) = Tree in (floor c)+L-1 end;
val delta = real(BuildNumber(nth(commandLine,2))) 
val NMoves = BuildNumber(nth(commandLine,3));

val file_out = open_out(nth(commandLine,4));

val LabeledTree = BTTreeLabel (N, L, D, delta, Tree, nil);
val StochAutom = BTBuildAutomaton LabeledTree; 
val MetricEnt = MetricEntropy(L,N,Tree);
val iA = Indeterminist(StochAutom);
val StatCompl = let val (Q,_,_,_,_) = Components StochAutom in
                    if ((iA) orelse (Cardinality Q = 1)) then
                       ~1.0
                    else
                       StatisticalComplexity(StochAutom,NMoves);
                end;
                
val Stat = SaveMachine(file_out,L,D,delta,NMoves,
                       StochAutom,iA,MetricEnt,StatCompl);

val EndOfAll = close(file_out);
