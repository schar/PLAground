
import Graphics.Input.Field as Field
import Signal (..)
import Graphics.Element (..)
import Text (plainText)
import List (concatMap)
import String (fromChar, filter, toList, fromList)


main : Signal Element
main = map scene (subscribe content)
-- main = putStrLn . gimme $ parse "  (~  (Ex((~e( x) ) & o ( x  ) ) ))"

content : Channel Field.Content
content =
  let startText = "(Ex (e(x) & o(x)))"
      startChan = Field.Content startText (Field.Selection 0 0 Field.Forward)
  in channel startChan


scene : Field.Content -> Element
scene fieldContent =
  let defaultText = "(Ex (e(x) & o(x)) )"
  in flow down
     [ Field.field Field.defaultStyle (send content) defaultText fieldContent
     , plainText <| gimme <| parse fieldContent.string
     ]

-- monadic bits
type alias M a = String -> List (a, String)


ret : a -> M a
ret a = \s -> [(a, s)]

bind : M a -> (a -> M b) -> M b
m `bind` f = \s -> concatMap (\(a, s') -> f a s') <| m s

-- zero and plus
zero : M a
zero x = []

plus : M a -> M a -> M a
m `plus` n = \s -> m s ++ n s

-- item
item : M Char
item s =
  let sss = toList s
  in case sss of
    [] -> [(' ', "")]
    (x::xs) -> [(x, fromList xs)]

-- filtering
filt : M a -> (a -> Bool) -> M a
m `filt` p = m `bind` \a -> 
          if p a
          then ret a
          else zero

-- literals
lit : Char -> M Char
lit c = item `filt` \a -> a == c

-- variables
var : M String
var =
  let isAsciiLower c = c >= 'a' && c <= 'z'
  in (item `filt` \a -> isAsciiLower a) `bind` \a -> ret (fromChar a)

-- pronouns
pro : M String
pro =
  let isDigit c = c >= '0' && c <= '9'
  in lit 'p' `bind` \a -> 
       (item `filt` \a -> isDigit a) `bind` \b ->
         ret <| fromChar a ++ fromChar b

-- terms
term : M String
term = pro `plus` var

-- predicates
onePlace : M Char
onePlace = lit 'e' `plus` lit 'o'

-- conjunction
conj : M Char
conj = lit '&'

-- negation
neg : M Char
neg = lit '~'
 
-- quantifiers
quant : M String
quant = lit 'E' `bind` \a -> 
          var `bind` \b ->
            ret <| fromChar a ++ b

-- formulae
form : M String
form =
  (onePlace `bind` \a ->
    lit '(' `bind` \_ ->
      term `bind` \b -> 
        lit ')' `bind` \_ ->
          ret <| "(Pred " ++ fromChar a ++ " " ++ b ++ ")")
  `plus` 
  (lit '(' `bind` \_ ->
    quant `bind` \a -> 
      form `bind` \b ->
        lit ')' `bind` \_ ->
          ret <| "(" ++ a ++ " " ++ b ++ ")")
  `plus`
  (lit '(' `bind` \_ ->
    neg `bind` \_ ->
      form `bind` \a -> 
         lit ')' `bind` \_ ->
           ret <| "(Neg " ++ a ++ ")")
  `plus`
  (lit '(' `bind` \_ ->
    form `bind` \a ->
       conj `bind` \_ ->
         form `bind` \b -> 
           lit ')' `bind` \_ ->
             ret <| "(Conj " ++ a ++ " " ++ b ++ ")")

clean : String -> String
clean = filter (\x -> x /= ' ')

parse : M String
parse = form << clean

-- gimme
gimme : List (String, String) -> String
gimme l =
  case l of
    [] -> "oops"
    ((x,y)::xs) -> x
