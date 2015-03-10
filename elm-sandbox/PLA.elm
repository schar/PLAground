import Parser (or, (<$>), (*>), (<*), symbol, recursively, Parser, parse, end, and)
import Array as Ar
import Parser.Char (lower, parenthesized)
import Parser.Number (digit)
import Graphics.Input.Field as Field
import Signal as Sig
import Graphics.Element (..)
import Text (plainText, asText)
import List
import String (fromChar, filter, toList, fromList, cons)
import Result (..)


-- open a reactive channel to text contents
content : Sig.Channel Field.Content
content =
  let startText = "(Ex (e(x) & o(x)))"
      startChan = Field.Content startText (Field.Selection 0 0 Field.Forward)
  in Sig.channel startChan

-- handler to process the reactive field contents at any given moment
scene : Field.Content -> Element
scene fieldContent =
  let clean = filter (\x -> x /= ' ')
      -- default text for the field when empty
      defText = "enter formula here"
      -- clean and attempt to parse the field contents
      result = parse (form <* end) (clean fieldContent.string) 
  -- stack two boxes on top of one another; the first a text input box hooked
  -- up to our reactive content channel, the second a box with the parse
  -- results
  in flow down
     [ Field.field Field.defaultStyle (Sig.send content) defText fieldContent
     , asText (map (show showF) result)
     , asText (map interpret result)
     ]

-- process the channel reactively (send current text contents through handler)
main : Signal Element
main = Sig.map scene (Sig.subscribe content)


-- Object Language Types
------------------------------------------------------------------------------

type Term = Con Int | Var Char | Pro Int
type Formula = Pred Char Term
             -- | Rel String Term Term
             | Neg Formula
             | Exists Term Formula
             | Conj Formula Formula

-- simulating a Show typeclass, just for kicks
type alias Show a = {show : a -> String}
showT : Show Term
showT = {
  show x = case x of
    Con n -> "[.Con " ++ toString n ++ " ]"
    Var v -> "[.Var " ++ fromChar v ++ " ]"
    Pro p -> "[.Pro p" ++ toString p ++ " ]"
  }
showF : Show Formula
showF = {
  show x = case x of
    Pred a b ->
      "[.Pred " ++ fromChar a ++ " " ++ show showT b ++ " ]"
    -- Rel a b c ->
    --   "[.Rel " ++ a ++ " " ++ "{(" ++ show showT b ++ ", " ++ show showT c ++ ")} ]"
    Neg f ->
      "[.Neg " ++ show showF f ++ " ]"
    Exists v f ->
      "[.Exists " ++ show showT v ++ " " ++ show showF f ++ " ]"
    Conj f1 f2 ->
      "[.Conj " ++ show showF f1 ++ " " ++ show showF f2 ++ " ]"
  }
show : Show a -> a -> String
show showInstance item = showInstance.show item


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
parens : Parser a -> Parser a
parens = parenthesized

-- formulae
form : Parser Formula
form =
  let self  = recursively <| \() -> form -- Elm is not lazy :/
      pred  = Pred <$> (symbol 'e' `or` symbol 'o') `and` parens term
      neg   = parens <| symbol '~' *> (Neg <$> self)
      quant = parens <| Exists <$> (symbol 'E' *> var) `and` self
      conj  = parens <| Conj <$> self <* symbol '&' `and` self
  in pred `or` neg `or` quant `or` conj


-- Semantics
------------------------------------------------------------------------------

-- Auxiliary Funcs
-- update assignment function
switch : Env -> Char -> Int -> Env
switch e var x = \u -> if u == var then Ok x else e u

-- run a formula at default context
interpret : Formula -> List (Result String Stack)
interpret input = eval input (\_ -> Ok -666) Ar.empty


-- Meta Language Types
type alias Env   = Char -> Result String Int
type alias Stack = Ar.Array Int
type alias Prop  = Stack -> List (Result String Stack)


-- The Model
domain : List Int
domain = [1..4]


-- Essential helper funcs encoding semantics of subformulas
predP : Char -> Int -> Prop
predP predId x s =
  let even = \x -> x % 2 == 0
      odd  = \x -> not <| even x
      f    = if predId == 'e' then even else odd
  in if f x then [Ok s] else []

-- eqP :: Int -> Int -> Prop
-- eqP x y s = if x == y then [s] else []

negP : Prop -> Prop
negP p s = case p s of
  [] -> [Ok s]
  _  -> []

andP : Prop -> Prop -> Prop
andP l r s =
  let blah ms' = case map r ms' of
    Err x -> [Err x]
    Ok ss -> ss
  in List.concatMap blah <| l s

exP : (Int -> Prop) -> Prop
exP p s = List.concatMap (\x -> p x (Ar.push x s)) domain


-- Main interpretation functions
evalTerm : Term -> Env -> Stack -> Result String Int
evalTerm t e s = case t of
  Con a -> Ok a
  Var v -> e v
  Pro n -> fromMaybe ("whoops: pro" ++ toString n) <| Ar.get n s

eval : Formula -> Env -> Prop
eval formula e s = case formula of
  Pred a b ->
    case evalTerm b e s of
      Err x -> [Err x]
      Ok x  -> predP a x s
  -- Rel(a, (b, c)) ->
  --   eqP (evalTerm b e s) (evalTerm c e s) s
  Neg f ->
    negP (eval f e) s
  Conj f1 f2 ->
    andP (eval f1 e) (eval f2 e) s
  Exists (Var v) f ->
    exP (\x -> eval f (switch e v x)) s
