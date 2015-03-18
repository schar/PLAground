module Utils where

import Signal as Sig
import Html (..)
import Html.Attributes (..)
import Html.Events (..)
import Json.Decode as Json
import Result
-- Parser Library
import Parser (..)
import Parser.Char (lower, parenthesized, bracketed)
import Parser.Number (digit)
import List
import Array as Ar

import PLA (..)

onEnter : Sig.Message -> Attribute
onEnter message =
    on "keydown" (Json.customDecoder keyCode is13) (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

-- empty assignment function
emptyEnv : Env
emptyEnv = always <| Err "-666"

-- assignment function parser
envP : Parser Env
envP =
  let addVar : Env -> Parser Env
      addVar env = switch env <$> lower <* token " -> " `and` digit
  in  addVar emptyEnv <* end `or`
        (addVar emptyEnv <* token "\n") `andThen` addVar

parseEnv : String -> Result String Env
parseEnv s = Result.map (List.head << List.reverse) <| parseAll envP s

inpP : Parser Stack
inpP = Ar.fromList <$> (separatedBy digit (token ","))

parseInp : String -> Result String Stack
parseInp = parse inpP

chunks : Int -> List a -> List (List a)
chunks n xs =
  case xs of
    [] -> []
    _  -> List.take n xs :: chunks n (List.drop n xs)
