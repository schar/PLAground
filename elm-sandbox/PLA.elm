import Parser (..)
import Parser.Char (lower, parenthesized)
import Parser.Number (digit)
import Graphics.Input.Field as Field
import Signal as Sig
import Graphics.Element (..)
import Text (plainText)
import List (concatMap)
import String (fromChar, filter, toList, fromList, cons)


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
      resultText =
        case parse (form <* end) (clean fieldContent.string) of
          Err x -> "oops"
          Ok x  -> x
  -- stack two boxes on top of one another; the first a text input box hooked
  -- up to our reactive content channel, the second a box with the parse
  -- results
  in flow down
     [ Field.field Field.defaultStyle (Sig.send content) defText fieldContent
     , plainText resultText
     ]

-- process the channel reactively (send current text contents through handler)
main : Signal Element
main = Sig.map scene (Sig.subscribe content)


-- Grammar
------------------------------------------------------------------------------

-- variables
var : Parser String
var = map fromChar lower

-- pronouns
pro : Parser String
pro = map cons (symbol 'p') `and` map toString digit

-- terms
term : Parser String
term = pro `or` var

showpred = \x y -> "(Pred " ++ fromChar x ++ " " ++ y ++ ")"
showneg = \x -> "(Neg " ++ x ++ ")"
showquant = \x y -> "(" ++ x ++ " " ++ y ++ ")"
showconj = \x y -> "(Conj " ++ x ++ " " ++ y ++ ")"

pars : Parser String -> Parser String
pars = parenthesized

-- formulae
form : Parser String
-- form = pred `or` quant `or` neg `or` conj
form =
  let self  = recursively <| \() -> form
      pred  = map showpred (symbol 'e' `or` symbol 'o') `and` pars term
      neg   = pars <| symbol '~' *> map showneg self
      quant = pars <| map showquant (map cons (symbol 'E') `and` var) `and` self
      conj  = pars <| map showconj self <* symbol '&' `and` self
  in pred `or` neg `or` quant `or` conj


