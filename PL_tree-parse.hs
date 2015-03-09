module Main (main) where
import           Data.Char
import           Data.List

data Term = Con Int | Var Char | Pro String
data Formula = Pred(Char, Term)
             | Neg Formula
             | Exists(Term, Formula)
             | Conj(Formula, Formula)

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
item "" = []
item (x:xs) = [(x, xs)]

-- filtering
filt :: M a -> (a -> Bool) -> M a
m `filt` p =
  m `bind` \a ->
    if p a
    then ret a
    else zero

-- literals
lit :: Char -> M Char
lit c = item `filt` \a -> a == c

-- predicates
onePlace :: M Char
onePlace = lit 'e' `plus` lit 'o'

-- conjunction
conj :: M Char
conj = lit '&'

-- negation
neg :: M Char
neg = lit '~'

-- variables
var :: M Term
var =
  (item `filt` \a -> isAsciiLower a) `bind` \a ->
    ret $ Var a

-- pronouns
pro :: M Term
pro =
  lit 'p' `bind` \a ->
    (item `filt` \a -> isDigit a) `bind` \b ->
      ret $ Pro $ [a] ++ [b]

-- terms
term :: M Term
term = pro `plus` var

-- formulae
form :: M Formula
form =
  (onePlace `bind` \a ->
    lit '(' `bind` \_ ->
      term `bind` \b ->
        lit ')' `bind` \_ ->
          ret $ Pred(a, b))
  `plus`
  (lit '(' `bind` \_ ->
    form `bind` \a ->
      conj `bind` \_ ->
        form `bind` \b ->
          lit ')' `bind` \_ ->
            ret $ Conj(a, b))
  `plus`
  (lit '(' `bind` \_ ->
    neg `bind` \_ ->
      form `bind` \a ->
        lit ')' `bind` \_ ->
          ret $ Neg a)
  `plus`
  (lit '(' `bind` \_ ->
    lit 'E' `bind` \_ ->
      var `bind` \v ->
        form `bind` \a ->
          lit ')' `bind` \_ ->
            ret $ Exists(v, a))

-- pretty printing
data Node = Term | Formula

showTerm :: Term -> String
showTerm (Con n) = show n
showTerm (Var v) = [v]
showTerm (Pro p) = p

showFormula :: Formula -> String
showFormula (Pred(a, b)) =
  "[.Pred " ++ [a] ++ " " ++ showTerm b ++ " ]"
showFormula (Neg f) =
  "[.Neg " ++ showFormula f ++ " ]"
showFormula (Exists(var, f)) =
  "[.Exists " ++ showTerm var ++ " " ++ showFormula f ++ " ]"
showFormula (Conj(f1, f2)) =
  "[.Conj " ++ showFormula f1 ++ " " ++ showFormula f2 ++ " ]"

-- IO
-- nice test case: (~  ((Ex (~e( x) )) & o ( x  ) ))
clean :: String -> String
clean = filter (/=' ')

parse :: M Formula
parse = form . clean

gimme :: [(Formula, String)] -> String
gimme [] = "oops"
gimme ((x, y):xs) = "\\Tree " ++ showFormula x

main :: IO ()
main = do
  putStrLn "gimme PLA"
  input <- getLine
  putStrLn . gimme $ parse input
