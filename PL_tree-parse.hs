module Main (main) where

import           Data.Char
import           Data.Tree
--import           Data.Tree.Pretty
import           System.Console.Haskeline

main :: IO ()
main = do
  putStrLn "\ESC[2Jstarting fresh.... [\"bye\" to stop, \"reset\" to reset]\n"
  runInputT defaultSettings $ helper [[]]

brake :: String
brake = "\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"

helper :: [Stack] -> InputT IO ()
helper incomingState = do
  outputStrLn $ "\n....gimme some PLA\n" ++ brake
  minput <- getInputLine ""
  case minput of
    Nothing -> return ()
    Just "bye" -> return ()
    Just "reset" -> do
      outputStrLn "\ESC[2Jstarting fresh.... [\"bye\" to stop, \"reset\" to reset]\n"
      helper [[]]
    Just input -> do
      outputStrLn $ "\ESC[2J"
        ++ "**Input**\n" ++ brake ++ "\n" ++ input
        ++ "\n\n**Parse**\n" ++ brake ++ "\n"
        ++ parseToTree input
        ++ "\n\n"
        ++ "**Meaning**\n" ++ brake ++ "\n{\n"
        ++ showState (concat [interpret input s | s <- incomingState])
        ++ "\n}\n\n"
      helper (concat [interpret input s | s <- incomingState])

showState :: [Stack] -> String
showState []     = ""
showState [x]    = " " ++ show x
showState (x:xs) = " " ++ show x ++ "\n" ++ showState xs

data Term = Con Char | Var Char | Pro String
data Formula = Pred(Char, Term)
             | Rel(String, (Term, Term))
             | Neg Formula
             | Exists(Term, Formula)
             | Conj(Formula, Formula)
             | Disj(Formula, Formula)
             | None

-- pretty printing
instance Show Term where
  show x = case x of
    Con n -> "[.Con " ++ [n] ++ " ]"
    Var v -> "[.Var " ++ [v] ++ " ]"
    Pro p -> "[.Pro " ++ p ++ " ]"

instance Show Formula where
  show x = case x of
    Pred(a, b) ->
      "[.Pred " ++ [a] ++ " " ++ show b ++ " ]"
    Rel(a, (b, c)) ->
      "[.Rel " ++ a ++ " [.Pair " ++ show b ++ " " ++ show c ++ " ] ]"
    Neg f ->
      "[.Neg " ++ show f ++ " ]"
    Exists(v, f) ->
      "[.Exists " ++ show v ++ " " ++ show f ++ " ]"
    Conj(f1, f2) ->
      "[.Conj " ++ show f1 ++ " " ++ show f2 ++ " ]"
    Disj(f1, f2) ->
      "[.Disj " ++ show f1 ++ " " ++ show f2 ++ " ]"
    None -> "SORRY, couldn't parse that!! (╯°□°）╯︵ ┻━┻"


-- monadic bits
type M a = String -> [(a, String)]

ret :: a -> M a
ret a s = [(a, s)]

bind :: M a -> (a -> M b) -> M b
m `bind` f = concatMap (uncurry f) . m

-- zero and plus
zero :: M a
zero _ = []

plus :: M a -> M a -> M a
m `plus` n = \s -> m s ++ n s

-- item
item :: M Char
item ""     = []
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
eq = lit 'e' `bind` \_ ->
      lit 'q' `bind` \_ ->
        ret "eq"

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
    (item `filt` isDigit) `bind` \b ->
      ret $ Pro $ a : [b]

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
    form `bind` \a ->
      lit '|' `bind` \_ ->
        form `bind` \b ->
          lit ')' `bind` \_ ->
            ret $ Disj(a, b))
  `plus`
  (neg `bind` \_ ->
    form `bind` \a ->
        ret $ Neg a)
  `plus`
  (lit 'E' `bind` \_ ->
    var `bind` \v ->
      form `bind` \a ->
          ret $ Exists(v, a))

-- fix: atm, any bad thing after a legit parse will just be ignored
-- e.g. if you don't consume every string, you fail....

-- IO
-- nice test case: (~  ((Ex (~e( x) )) & o ( p0  ) ))
clean :: String -> String
clean = filter (/=' ')

parse :: M Formula
parse = form . clean

gimme :: [(Formula, String)] -> Formula
gimme []        = None
gimme ((x,_):_) = x

-- interpreter
type Env = Char -> Int
type Stack = [Int]
type Prop = Stack -> [Stack]

domain :: [Int]
domain = [0..9]

evalTerm :: Term -> Env -> Stack -> Int
evalTerm t e s = case t of
  Con a     -> read [a]
  Var v     -> e v
  Pro (_:n) -> reverse s !! read n
  Pro []    -> 666

switch :: Env -> Int -> Char -> Char -> Int
switch e x v u = if u == v then x else e u

eval :: Formula -> Env -> Prop
eval x e s = case x of
  Pred(a, b) ->
    if a == 'e'
    then liftP even t s
    else liftP odd t s
      where t = evalTerm b e s
  Rel(_, (b, c)) ->
    eqP (evalTerm b e s) (evalTerm c e s) s
  Neg f ->
    (negP $ eval f e) s
  Conj(f1, f2) ->
    andP (eval f1 e) (eval f2 e) s
  Disj(f1, f2) ->
    eval f1 e s ++ eval f2 e s
  Exists(Var v, f) ->
    exP (\n -> eval f (switch e n v)) s
  Exists(_,_) ->
    [[666]]
  None ->
    [[666]]

liftP :: (Int -> Bool) -> Int -> Prop
liftP f x s = [s | f x]

eqP :: Int -> Int -> Prop
eqP x y s = [s | x == y]

negP :: Prop -> Prop
negP p s = case p s of
  [] -> [s]
  _  -> []

andP :: Prop -> Prop -> Prop
andP l r s = concat [r s' | s' <- l s]

exP :: (Int -> Prop) -> Prop
exP p s = [s' ++ [x] | x <- domain, s' <- p x s]

interpret :: String -> Stack -> [Stack]
interpret input = (eval $ gimme $ parse input) (\_ -> -666)

-- tree pretty-printing
-- first parse into string tree
termTree :: Term -> Tree String
termTree t = case t of
  Con x -> Node [x] []
  Var x -> Node [x] []
  Pro x -> Node x []

toStringTree :: Formula -> Tree String
toStringTree tree = case tree of
  Pred(a, b) -> Node "Formula" [ Node [a] []
                               , termTree b ]
  Rel(_, (b, c)) -> Node "Formula" [ Node "eq" []
                                   , Node "Pair" [ termTree b
                                                 , termTree c ] ]
  Neg f -> Node "Formula" [ Node "~" []
                          , toStringTree f ]
  Conj(f1, f2) -> Node "Formula" [ toStringTree f1
                                 , Node "&" []
                                 , toStringTree f2 ]
  Disj(f1, f2) -> Node "Formula" [ toStringTree f1
                                 , Node "v" []
                                 , toStringTree f2 ]
  Exists(Var v, f) -> Node "Formula" [ termTree . Pro $ "Ǝ" ++ [v]
                                     , toStringTree f] -- hack here, using
                                                       -- Pro constructor
                                                       -- ¯\_(ツ)_/¯
  _ -> Node [] []

parseToTree :: String -> String
parseToTree input = drawTree . toStringTree . gimme . parse $ input
