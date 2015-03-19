module PLA where

-- Basic Data
import List
import Array as Ar
import String (fromChar, filter, toList, fromList, cons, join)
import Result (..)

-- Parser Library
import Parser (or, (<$>), (*>), (<*), symbol, recursively,
               Parser, parse, end, and, some, separatedBy)
import Parser.Char (lower, parenthesized)
import Parser.Number (digit)


------------------------------------------------------------------------------
-- Predicate Logic with Anaphora (Dekker 1994)
------------------------------------------------------------------------------

-- Object Language Types
------------------------------------------------------------------------------

type Term = Con Int | Var Char | Pro Int
type Formula = Pred Char Term
             | Rel String (List Term)
             | Neg Formula
             | Exists Term Formula
             | Conj Formula Formula

showTerm : Term -> String
showTerm x =
  case x of
    Con n -> "[.Con " ++ toString n ++ " ]"
    Var v -> "[.Var " ++ fromChar v ++ " ]"
    Pro p -> "[.Pro p" ++ toString p ++ " ]"
    -- Con n -> toString n
    -- Var v -> fromChar v
    -- Pro p -> toString p

showFormula : Formula -> String
showFormula x =
  case x of
    Pred a b ->
      "[.Pred " ++ fromChar a ++ " " ++ showTerm b ++ " ]"
    Rel a bs ->
      "[.Rel " ++ a ++ " " ++ "{(" ++ join "," (List.map showTerm bs) ++ ")} ]"
    Neg f ->
      "[.Neg " ++ showFormula f ++ " ]"
    Exists v f ->
      "[.Exists " ++ showTerm v ++ " " ++ showFormula f ++ " ]"
    Conj f1 f2 ->
      "[.Conj " ++ showFormula f1 ++ " " ++ showFormula f2 ++ " ]"
    -- Pred a b -> fromChar a ++ "(" ++ showTerm b ++ ")"
    -- Rel a bs -> a ++ "(" ++ join "," (List.map showTerm bs) ++ ")"
    -- Neg f -> "~" ++ showFormula f
    -- Exists v f -> "E" ++ showTerm v ++ " " ++ showFormula f
    -- Conj f1 f2 -> "(" ++ showFormula f1 ++ " & " ++ showFormula f2 ++ ")"


-- Grammar
------------------------------------------------------------------------------

-- variables
var : Parser Term
var = Var <$> lower

-- constants
con : Parser Term
con = Con <$> digit

-- pronouns
pro : Parser Term
pro = symbol 'p' *> (Pro <$> digit)

-- terms
term : Parser Term
term = pro `or` var `or` con

-- given a parser for "blah", this parses "(blah)"
-- parens : Parser a -> Parser a
-- parens = parenthesized

-- formulae
form : Parser Formula
form =
  let self  = recursively <| \() -> form -- Elm is not lazy :/
      pred  = Pred <$> lower `and` parenthesized term
      rel   = Rel <$> (fromList <$> some lower) `and`
                parenthesized (separatedBy term (symbol ','))
      neg   = symbol '~' *> (Neg <$> self)
      quant = Exists <$> (symbol 'E' *> var) `and` self
      conj  = parenthesized <| Conj <$> self <* symbol '&' `and` self
  in pred `or` rel `or` neg `or` quant `or` conj

parseForm : String -> Result String Formula
parseForm expr = parse (form <* end) <| filter (\x -> x /= ' ') expr


-- Semantics
------------------------------------------------------------------------------

-- Meta Language Types
type alias Env   = Char -> Result String Int
type alias Stack = Ar.Array Int
type alias Prop  = Stack -> Result String (List Stack)

-- The Model
domain : List Int
domain = [1..4]

-- update assignment function
switch : Env -> Char -> Int -> Env
switch e var x = \u -> if u == var then Ok x else e u

-- One-Place Lexicon
predDict : Char -> Result String (Int -> Bool)
predDict predId =
  case predId of
    'e' -> Ok <| \x -> x % 2 == 0
    'o' -> Ok <| \x -> x % 2 == 1
    _   -> Err <| "No predicate: " ++ fromChar predId

-- Two-Place Lexicon
relDict : String -> Result String (Int -> Int -> Bool)
relDict relId =
  case relId of
    "eq" -> Ok <| \x y -> x == y
    _    -> Err <| "No relation: " ++ relId

-- Resolve variables, constants, and pronouns
evalTerm : Term -> Env -> Stack -> Result String Int
evalTerm t e s = case t of
  Con a -> Ok a
  Var v -> e v
  Pro n ->
    fromMaybe ("pro" ++ toString n ++ "?") <| Ar.get (Ar.length s - n - 1) s

-- Interpretation function
eval : Formula -> Env -> Prop
eval formula e s = case formula of
  Pred a b ->
    let predPLA f n s = if f n then [s] else []
    in map3 predPLA (predDict a) (evalTerm b e s) (Ok s)
  Rel a [b,c] ->
    let relPLA f n m s = if f n m then [s] else []
    in map4 relPLA (relDict a) (evalTerm b e s) (evalTerm c e s) (Ok s)
  Neg f ->
    let negPLA ss s = if List.isEmpty ss then [s] else []
    in map2 negPLA (eval f e s) (Ok s)
  Conj f1 f2 ->
    let mplus m m' = m `andThen` \xs -> m' `andThen` \ys -> Ok (xs ++ ys)
    in case eval f1 e s of
         Err msg -> Err msg
         Ok  ls  -> List.foldr mplus (Ok []) <| List.map (eval f2 e) ls
  Exists (Var v) f ->
    let scope = \x -> eval f (switch e v x)
        mplus (m,x) m' =
          m `andThen` \xs -> m' `andThen` \ys ->
            Ok (List.map (Ar.push x) xs ++ ys)
    in List.foldr mplus (Ok []) <| List.map (\x -> (scope x s, x)) domain
