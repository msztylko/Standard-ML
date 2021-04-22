(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

fun only_capitals strings =
    List.filter (fn x => Char.isUpper(String.sub(x,0))) strings

fun longest_string1 strings =
    List.foldl (fn (a,b) => if String.size a >= String.size b then a  else b ) "" strings

fun longest_string2 strings =
    List.foldl (fn (a,b) => if String.size a > String.size b then a  else b ) "" strings

fun longest_string_helper f strings =
    List.foldl (fn (s1, s2) => if f(String.size s1, String.size s2) then s1 else s2) "" strings

val longest_string3 = longest_string_helper (fn (x,y) => x > y)
val longest_string4 = longest_string_helper (fn (x,y) => x >= y)

fun longest_capitalized strings =
    (longest_string3 o only_capitals) strings

fun rev_string str =
    (String.implode o List.rev o String.explode) str

fun first_answer f xs =
    case xs of
        [] => raise NoAnswer
    |   x::xs' => case f x of
                        NONE => first_answer f xs'
                    |   SOME v => v

fun all_answers f xs =
    let fun helper(acc, xs) =
        case xs of
            [] => SOME acc
        |   x::xs' => case f x of 
                            NONE => NONE
                        |   SOME v => helper(v @ acc, xs')
    in
        helper([], xs)
    end

fun count_wildcards p = 
    g (fn () => 1) (fn x => 0) p

fun count_wild_and_variable_lengths p = 
    g (fn () => 1) (fn x => String.size x) p

fun count_some_var(s, p) = 
    g (fn () => 0) (fn x => if x = s then 1 else 0) p

fun check_pat p =
    let fun get_string_list p =
            case p of
              Variable s => [s]
            | TupleP pattern_list => List.foldl (fn (p,i) => (get_string_list p) @ i) [] pattern_list
            | ConstructorP(s, pt) => get_string_list pt
            | _ => []

        fun exist_repetitions list_string =
            case list_string of
              [] => false
            | head::tail => List.exists (fn s => s = head) tail orelse (exist_repetitions tail)
    in
        not((exist_repetitions o get_string_list) p)
    end

fun match(v, p) =
    case (v, p) of
      (_, Wildcard) => SOME []
    | (v, Variable s) => SOME [(s, v)]
    | (Unit, UnitP) => SOME []
    | (Const x, ConstP y) => if x = y then SOME [] else NONE
    | (Tuple valu_list, TupleP pattern_list) => if length valu_list = length pattern_list
        then all_answers match (ListPair.zip(valu_list, pattern_list)) else NONE
    | (Constructor(s1, v), ConstructorP(s2, p)) => if s1 = s2 then match(v,p) else NONE
    | (_, _) => NONE

fun first_match v pattern_list =
    SOME(first_answer(fn p => match(v, p)) pattern_list) handle NoAnswer => NONE
