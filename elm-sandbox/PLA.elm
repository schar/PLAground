-- Basic Data
import List
import Array as Ar
import String (fromChar, filter, toList, fromList, cons)
import Result (..)

-- Parser Library
import Parser (or, (<$>), (*>), (<*), symbol, recursively, Parser, parse, end, and)
import Parser.Char (lower, parenthesized)
import Parser.Number (digit)

-- Reactive Graphics
import Signal as Sig
import Graphics.Input.Field as Field
import Graphics.Element (..)
import Text (plainText, asText)
import Window


-- open a reactive channel to text contents
content : Sig.Channel Field.Content
content =
  let startText = "(Ex (e(x) & o(x)))"
      startChan = Field.Content startText (Field.Selection 0 0 Field.Forward)
  in Sig.channel startChan

-- handler to process the reactive field contents at any given moment
scene : Field.Content -> (Int,Int) -> Element
scene fieldContent (w,h) =
  let clean = filter (\x -> x /= ' ')
      -- default text for the field when empty
      defText = "enter formula here"
      -- clean and attempt to parse the field contents
      result = parse (form <* end) (clean fieldContent.string) 
  -- stack two boxes on top of one another; the first a text input box hooked
  -- up to our reactive content channel, the second a box with the parse
  -- results
  in -- position everything in the top middle of the window
     container w h midTop <|
     -- pad 50pts at the top
     above (spacer 50 50) <|
     -- flow the elements vertically
     flow down <|
     -- put 50pts of space between each element
     List.intersperse (spacer 50 50)
     -- here's the content
     [ -- text input box
       Field.field Field.defaultStyle (Sig.send content) defText fieldContent
       -- display the LF of the formula, or an error message
     , renderSyn result
       -- display the meaning of the formula, or an error message
     , renderSem (map interpret result)
     ]

-- just showF plus some error handling
renderSyn : Result String Formula -> Element
renderSyn pars =
  case pars of
    Err msg -> plainText msg
    Ok  lf   -> plainText <| show showF lf

-- just interpret plus some error handling
renderSem : Result String (List (Result String Stack)) -> Element
renderSem meaning =
  case meaning of
    Err msg -> empty
    Ok  xs  ->
      let disp = \x -> case x of
                         Err msg -> plainText ("[" ++ msg ++ "]")
                         Ok s    -> asText (Ar.toList s)
          outs = List.map disp xs
      in if List.isEmpty outs
            then plainText "Impossible!"
            else flow right <| List.intersperse (spacer 10 10) outs

-- process the channel reactively (send current text contents through handler)
main : Signal Element
main = Sig.map2 scene (Sig.subscribe content) (Window.dimensions)


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
      pred  = Pred <$> lower `and` parens term
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
interpret input = eval input (always <| Err "-666") Ar.empty


-- Meta Language Types
type alias Env   = Char -> Result String Int
type alias Stack = Ar.Array Int
type alias Prop  = Stack -> List (Result String Stack)


-- The Model
domain : List Int
domain = [1..4]


-- Essential helper funcs encoding semantics of subformulas
predP : Char -> Result String Int -> Prop
predP predId term s =
  let lookup predId = case predId of
                        'e' -> Ok <| \x -> x % 2 == 0
                        'o' -> Ok <| \x -> x % 2 == 1
                        _   -> Err <| "No predicate: " ++ fromChar predId
  in case term of
       Err msg -> [Err msg]
       Ok  n   -> case lookup predId of
                    Err msg -> [Err msg]
                    Ok  f   -> if f n then [Ok s] else []

-- eqP :: Result Int -> Int -> Prop
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
    predP a (evalTerm b e s) s
  -- Rel a b c ->
  --   eqP (evalTerm b e s) (evalTerm c e s) s
  Neg f ->
    negP (eval f e) s
  Conj f1 f2 ->
    andP (eval f1 e) (eval f2 e) s
  Exists (Var v) f ->
    exP (\x -> eval f (switch e v x)) s
