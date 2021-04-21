(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)

fun same_string(s1 : string, s2 : string) =
    s1 = s2

fun all_except_option (str, str_list) =
    case str_list of
        [] => NONE
      | x::xs => if same_string(str, x)
                 then SOME xs
                 else all_except_option(str, xs)

fun get_substitutions1 (substitutions, str) =
    case substitutions of
        [] => []
      | x::xs => let 
                    val rest = get_substitutions1(xs, str) 
                 in
                    case all_except_option(str, x) of
                        NONE => rest
                      | SOME str_list => str_list @ rest
                end

fun get_substitutions2(substitutions, str) =
    let fun helper(substitutions, acc) =
            case substitutions of
              [] => acc
            | x::xs => case all_except_option(str, x) of
                                NONE => helper(xs, acc)
                              | SOME list_string => helper(xs, acc @ list_string)
    in
        helper(substitutions, [])
    end

fun similar_names (substitutions,name) =
    let 
        val {first=f, middle=m, last=l} = name
          fun make_names xs =
             case xs of
                   [] => []
               | x::xs' => {first=x, middle=m, last=l}::(make_names(xs'))
    in
          name::make_names(get_substitutions2(substitutions,f))
    end

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

fun card_color c =
    case c of
        (Clubs, _) => Black
      | (Spades, _) => Black
      | (Diamonds, _) => Red
      | (Hearts, _) => Red

fun card_value c =
    case c of
        (_, Num n) => n
      | (_, Ace) => 11
      | _ => 10

fun remove_card (cs, c, e) =
    case cs of
        [] => raise e
      | x::xs => if x = c
                 then xs
                 else remove_card(xs, c, e)

fun all_same_color cs = 
    case cs of
        [] => true
      | [_] => true
      | head::neck::tail => card_color head = card_color neck 
                andalso all_same_color(neck::tail)

fun sum_cards cs =
    let
        fun helper(cs,acc) =
            case cs of
                [] => acc
              | x::xs => helper(xs, acc + card_value(x))
    in
       helper(cs, 0) 
    end

fun score (held_cards, goal) =
    let 
        val sum = sum_cards held_cards
        fun preliminary_score sum = if sum > goal
                                    then 3 * (sum - goal)
                                    else goal - sum
    in
        case (all_same_color held_cards) of
          true  => preliminary_score(sum) div 2
        | false => preliminary_score(sum)
    end

fun  officiate (cs, ms, goal) =
    let 
        fun helper(cs, hcs, ms) =
        case ms of
          [] => score(hcs, goal)
        | (Discard c)::tail_move => (case hcs of
                                      [] => raise IllegalMove
                                    | _  => helper(cs, remove_card(hcs, c, IllegalMove), tail_move))
        | Draw::tail_move => case cs of
                               []      => score(hcs, goal)
                             | head::_ => if sum_cards(head::hcs) > goal
                                          then score(head::hcs, goal)
                                          else helper(remove_card(cs, head , IllegalMove), head::hcs, tail_move)
    in
        helper(cs, [], ms)
    end
