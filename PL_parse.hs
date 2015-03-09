module Main (main) where
import Data.List
import Data.Char

-- monadic bits
type M a = String -> [(a, String)]

ret :: a -> M a
ret a = \s -> [(a, s)]

bind :: M a -> (a -> M b) -> M b
m `bind` f = \s -> concatMap (\(a, s') -> f a s') $ m s

-- zero and plus
zero :: M a
zero x = []

plus :: M a -> M a -> M a
m `plus` n = \s -> m s ++ n s

-- item
item :: M Char
item (x:xs) = [(x, xs)]

-- filtering
filt :: M a -> (a -> Bool) -> M a
m `filt` p = m `bind` \a -> 
          if p a
          then ret a
          else zero

-- literals
lit :: Char -> M Char
lit c = item `filt` \a -> a == c

-- variables
var :: M String
var = (item `filt` \a -> isAsciiLower a) `bind` \a ->
      ret [a]

-- pronouns
pro :: M String
pro = lit 'p' `bind` \a -> 
        (item `filt` \a -> isDigit a) `bind` \b ->
          ret $ [a] ++ [b]

-- terms
term :: M String
term = pro `plus` var

-- predicates
onePlace :: M Char
onePlace = lit 'e' `plus` lit 'o'

-- conjunction
conj :: M Char
conj = lit '&'

-- negation
neg :: M Char
neg = lit '~'
 
-- quantifiers
quant :: M String
quant = lit 'E' `bind` \a -> 
          var `bind` \b ->
            ret $ [a] ++ b

-- formulae
form :: M String
form = (onePlace `bind` \a ->
       lit '(' `bind` \_ ->
       term `bind` \b -> 
       lit ')' `bind` \_ ->
       ret $ "(Pred " ++ [a] ++ " " ++ b ++ ")")
       `plus` 
       (lit '(' `bind` \_ ->
       quant `bind` \a -> 
       form `bind` \b ->
       lit ')' `bind` \_ ->
       ret $ "(" ++ a ++ " " ++ b ++ ")")
       `plus`
       (lit '(' `bind` \_ ->
       neg `bind` \_ ->
       form `bind` \a -> 
       lit ')' `bind` \_ ->
       ret $ "(Neg " ++ a ++ ")")
       `plus`
       (lit '(' `bind` \_ ->
       form `bind` \a ->
       conj `bind` \_ ->
       form `bind` \b -> 
       lit ')' `bind` \_ ->
       ret $ "(Conj " ++ a ++ " " ++ b ++ ")")

clean :: String -> String
clean = (filter (/=' '))

parse :: M String
parse = form . clean

-- gimme
gimme :: [(String, String)] -> String
gimme [] = "oops"
gimme ((x,y):xs) = x

main = putStrLn . gimme $ parse "  (~  (Ex((~e( x) ) & o ( x  ) ) ))"
