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
import String (fromChar)

import PLA (..)

flip2 f b c a = f a b c

onEnter : Sig.Message -> Attribute
onEnter message =
    on "keydown" (Json.customDecoder keyCode is13) (always message)

is13 : Int -> Result String ()
is13 code =
  if code == 13 then Ok () else Err "not the right key code"

onSlash : Sig.Message -> Attribute
onSlash message =
  on "keydown" (Json.customDecoder keyCode is44) (always message)

is44 : Int -> Result String ()
is44 code =
  if code == 44 then Ok () else Err "not the right key code"

-- empty assignment function
emptyEnv : Env
emptyEnv v = Err <| fromChar v ++ "?"

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
inpP = Ar.fromList <$> many digit

parseInp : String -> Result String Stack
parseInp = parse inpP

chunks : Int -> List a -> List (List a)
chunks n xs =
  case xs of
    [] -> []
    _  -> List.take n xs :: chunks n (List.drop n xs)

evals : List Formula -> Env -> Stack -> Result String (List (List Stack))
evals lfs env s =
  let cs = List.scanl1 (flip Conj) lfs
      xxs = List.map (\lf -> eval lf env s) cs
      seq m m' = m `Result.andThen`
                 \xs -> m' `Result.andThen`
                 \yys -> Ok (xs :: yys)
  in  List.foldr seq (Ok []) xxs

