module Main (main) where
import           Data.Char
import           Data.List

main :: IO ()
main = do
  putStrLn "\ngimme PLA\n"
  input <- getLine
  putStrLn $ "\n**Parse**\n" ++ (showFormula $ gimme $ parse input)
    ++ "\n\n"
    ++ "**Meaning**\n{\n" ++ (showState . interpret) input ++ "\n}\n"

showState :: [Stack] -> String
showState [] = ""
showState (x:[]) = show x
showState (x:xs) = show x ++ "\n" ++ showState xs

data Term = Con Char | Var Char | Pro String
data Formula = Pred(Char, Term)
             | Rel(String, (Term, Term))
             | Neg Formula
             | Exists(Term, Formula)
             | Conj(Formula, Formula)
             | None

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

-- equality
eq :: M String
eq = lit 'e' `bind` \a ->
      lit 'q' `bind` \b ->
        ret $ "eq"

-- constants (i.e. integers)
con :: M Term
con =
  (item `filt` \a -> isDigit a) `bind` \a ->
    ret $ Con a

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
term = con `plus` var `plus` pro

-- formulae
form :: M Formula
form =
  (onePlace `bind` \a ->
    lit '(' `bind` \_ ->
      term `bind` \b ->
        lit ')' `bind` \_ ->
          ret $ Pred(a, b))
  `plus`
  (eq `bind` \a ->
    lit '(' `bind` \_ ->
      term `bind` \b ->
        lit ',' `bind` \_ ->
          term `bind` \c ->
            lit ')' `bind` \_ ->
              ret $ Rel(a, (b, c)))
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

-- fix: atm, any bad thing after a legit parse will just be ignored
-- e.g. if you don't consume every string, you fail....

-- pretty printing
showTerm :: Term -> String
showTerm x = case x of
  Con n -> "[.Con " ++ [n] ++ " ]"
  Var v -> "[.Var " ++ [v] ++ " ]"
  Pro p -> "[.Pro " ++ p ++ " ]"

showFormula :: Formula -> String
showFormula x = case x of
  Pred(a, b) ->
    "[.Pred " ++ [a] ++ " " ++ showTerm b ++ " ]"
  Rel(a, (b, c)) ->
    "[.Rel " ++ a ++ " [.Pair " ++ showTerm b ++ " " ++ showTerm c ++ " ] ]"
  Neg f ->
    "[.Neg " ++ showFormula f ++ " ]"
  Exists(var, f) ->
    "[.Exists " ++ showTerm var ++ " " ++ showFormula f ++ " ]"
  Conj(f1, f2) ->
    "[.Conj " ++ showFormula f1 ++ " " ++ showFormula f2 ++ " ]"
  None -> "SORRY, couldn't parse that!! :("

-- IO
-- nice test case: (~  ((Ex (~e( x) )) & o ( p0  ) ))
clean :: String -> String
clean = filter (/=' ')

parse :: M Formula
parse = form . clean

gimme :: [(Formula, String)] -> Formula
gimme [] = None
gimme ((x, y):xs) = x

-- interpreter
type Env = Char -> Int
type Stack = [Int]
type Prop = Stack -> [Stack]

domain :: [Int]
domain = [1..4]

evalTerm :: Term -> Env -> Stack -> Int
evalTerm t e s = case t of
  Con a -> read [a]
  Var v -> e v
  Pro (p:n) -> reverse s !! read n

switch :: Env -> Int -> Char -> Char -> Int
switch e x var u = if u == var then x else e u

eval :: Formula -> Env -> Prop
eval x e s = case x of
  Pred(a, b) ->
    if a == 'e'
    then (liftP even) t s
    else (liftP odd) t s
      where t = evalTerm b e s
  Rel(a, (b, c)) ->
    eqP (evalTerm b e s) (evalTerm c e s) s
  Neg f ->
    (negP $ eval f e) s
  Conj(f1, f2) ->
    andP (eval f1 e) (eval f2 e) s
  Exists(Var v, f) ->
    exP (\x -> eval f (switch e x v)) s
  None ->
    [[666]]

liftP :: (Int -> Bool) -> Int -> Prop
liftP f x s = if f x then [s] else []

eqP :: Int -> Int -> Prop
eqP x y s = if x == y then [s] else []

negP :: Prop -> Prop
negP p s = case p s of
  [] -> [s]
  _  -> []

andP :: Prop -> Prop -> Prop
andP l r s = concat [r s' | s' <- l s]

exP :: (Int -> Prop) -> Prop
exP p s = concat [p x (s ++ [x]) | x <- domain]

interpret :: String -> [Stack]
interpret input = (eval $ gimme $ parse input) (\_ -> -666) []
