
(* Software from the book "Automatas Finitos con Programacion Funcional" *)
(* by Antonio Blanco, Gilberto Perez and Jose Santiago Jorge             *)
(* Universidade da Corunya, Servicio da Publicacions, Setembro 1993      *)


fun Len nil        = 0
 |  Len (_ :: xs)  = 1 + Len xs;

fun Map f nil       = nil
 |  Map f (x :: xs) = f x :: Map f xs;

fun Filter p nil           = nil
 |  Filter p (x :: xs)     = if p x then
                                x :: Filter p xs
                             else
                                Filter p xs;

fun RedLeft f u nil        = u
 |  RedLeft f u (x :: xs)  = RedLeft f (f u x) xs;

fun FromTo (i, s) = if i > s then
                       nil
                    else
                       i :: FromTo (i+1, s);

fun Mem nil _       = false
 |  Mem (x :: xs) y = x = y orelse Mem xs y;

fun RemDupl nil       = nil
 |  RemDupl (x :: xs) = if Mem xs x then
                           RemDupl xs
                        else
                           x :: RemDupl xs;

local 
  fun Part (x: string) nil = (nil, nil)
   |  Part x (v :: vs)     =
        let val (us, ws) = Part x vs
        in if x < v then
              (us, v :: ws)
           else
              (v :: us, ws)
        end
in
  fun Sort nil        = nil
   |  Sort (x :: xs)  = 
        let val (us, ws) = Part x xs
        in (Sort us) @ [x] @ (Sort ws)
        end
end;

fun Exists p nil        = false
 |  Exists p (x :: xs)  = p x orelse Exists p xs;

fun ForAll p = not o Exists (not o p);

fun Digit c =
    c = "0" orelse c = "1" orelse c = "2"
            orelse c = "3" orelse c = "4"
            orelse c = "5" orelse c = "6"
            orelse c = "7" orelse c = "8"
            orelse c = "9";

local
  exception DigitValError
in
  fun DigitVal c = if Digit c then
                      ord c - ord "0"
                   else
                      raise DigitValError
end;

local
  fun BuildNumber' (i, nil)       = i
   |  BuildNumber' (i, (x :: xs)) = BuildNumber' (i * 10 + DigitVal x, xs)
in
  fun BuildNumber w = BuildNumber' (0, explode w)
end;

fun BuildString n = if n > 9 then
                       BuildString (n div 10) ^ chr ((n mod 10) + ord "0")
                    else
                       chr (n + ord "0");

abstype 'a SET = Set of 'a list
 with
   val EmptySet                 = Set nil
   fun Single x                 = Set [x]
   fun Union (Set xs) (Set ys)  = Set (xs @ ys)
   fun IsEmptySet (Set nil)     = true
    |  IsEmptySet _             = false
   fun MkList' (Set xs)         = xs
   fun Cardinality (Set xs)     = Len (RemDupl xs)
   fun SetMap f (Set xs)        = Set (Map f xs)
   fun SetFilter p (Set xs)     = Set (Filter p xs)
   fun Member (Set xs) x        = Mem xs x
   fun SetExists p (Set xs)     = Exists p xs
   fun SetForAll p (Set xs)     = ForAll p xs
   fun PowerSet (Set nil)       = Single (Set nil)
    |  PowerSet (Set (x :: xs)) = let val A = PowerSet (Set xs) in
                                      Union A (SetMap (Union (Set [x])) A)
                                  end
   fun SetFlat (Set nil)        = Set nil
    |  SetFlat (Set (X :: Xs))  = Union X (SetFlat (Set Xs))
end;

fun MkSet nil          = EmptySet
 |  MkSet (x :: xs)    = Union (Single x) (MkSet xs);

val MkList = RemDupl o MkList';

fun Intersection A B = SetFilter (Member A) B;

fun Difference A B = SetFilter (not o Member B) A;

fun SubSet A B = SetForAll (Member B) A;

fun EqSet A B = SubSet A B andalso SubSet B A;

datatype STATE = State of string;

datatype INPUT = Input of string;

abstype AUTOM = Autom of (STATE SET) * (INPUT SET) * 
                         ((STATE * INPUT * real * STATE) SET) * STATE * (STATE SET)
 with
   local exception AutomError in
     fun MakeAutom (Q, S, T, q, F) =
         if SetForAll (fn (State _) => true
                        | _         => false) Q andalso
            SetForAll (fn (Input _) => true
                        | _         => false) S andalso
            Member Q q andalso
            SubSet F Q andalso
            SetForAll (fn (p, w, r, q) => Member Q p andalso Member Q q andalso
                                       (w = Input "" orelse Member S w) andalso
                                       (0.0 <= r) andalso (r <= 1.0)
                      ) T 
         then
           Autom (Q, S, T, q, F)           
         else
           raise AutomError
   end
   fun Components (Autom M) = M
end;

local
   local
      fun SetToString' nil            = "}"
       |  SetToString' (q :: nil)     = q ^ "}"
       |  SetToString' (p :: q :: qs) = p ^ ", " ^ SetToString' (q :: qs)
   in
      fun SetToString A = "{" ^ SetToString' (Sort (MkList A))
   end
   fun StateToString (State x) = x
   fun InputToString (Input x) = x
   fun States M = let val (Q, _, _, _, _) = Components M in
                      "States = " ^ SetToString (SetMap StateToString Q)
                  end
   fun Alphabet M = let val (_, S, _, _, _) = Components M in
                        "\nAlphabet = " ^ SetToString (SetMap InputToString S)
                    end
   fun Transitions M = let val (_, _, D, _, _) = Components M
                           val D' = SetFilter (fn (x, y, w, z) =>
                                      not (x = z andalso y = Input "")) D
                           val D'' = SetMap (fn (x, y, w, z) =>
                                      "(" ^ StateToString x ^ ", " ^
                                      (if y = Input "" then "lambda"
                                                       else InputToString y) ^ 
                                      "/" ^ makestring w ^ ", " ^ StateToString z ^ ")" ) D'
                       in
                           "\nTransitions = " ^ SetToString D''
                       end
   fun InitialState M = let val (_, _, _, q, _) = Components M in
                            "\nInitialState = " ^ StateToString q
                        end
   fun FinalStates M = let val (_, _, _, _, F) = Components M in
                           "\nFinal States = " ^
                                 SetToString (SetMap StateToString F)
                       end
in
   fun PrintAutom M = States M ^ Alphabet M ^ Transitions M ^ 
                       InitialState M ^ FinalStates M
end;

(* --------------------------------------------------------------------
   -------------------------------------------------------------------- *)
