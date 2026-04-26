module Solution where
import Data.Map (Map)
import qualified Data.Map as M
import Control.Monad (foldM, guard)
import Data.List (permutations)
import Control.Monad.Trans.Writer

-- EXERCISE 1
type Pos = (Int, Int)
data Dir = N | S | E | W deriving (Eq, Ord, Show)
type Maze = Map Pos (Map Dir Pos)

-- (a)
move :: Maze -> Pos -> Dir -> Maybe Pos
move maze pos dir = do
  dirs <- M.lookup pos maze
  M.lookup dir dirs

-- (b)
followPath :: Maze -> Pos -> [Dir] -> Maybe Pos
followPath maze start = foldM (move maze) start

-- (c)
safePath :: Maze -> Pos -> [Dir] -> Maybe [Pos]
safePath maze start [] = Just [start]
safePath maze start (d:ds) = do
  next <- move maze start d
  rest <- safePath maze next ds
  return (start : rest)

-- EXERCISE 2
type Key = Map Char Char

decrypt :: Key -> String -> Maybe String
decrypt key = traverse (`M.lookup` key)

decryptWords :: Key -> [String] -> Maybe [String]
decryptWords key = traverse (decrypt key)

-- EXERCISE 3
type Guest = String
type Conflict = (Guest, Guest)

seatings :: [Guest] -> [Conflict] -> [[Guest]]
seatings guests conflicts = do
  p <- permutations guests
  let pairs = zip p (tail p ++ [head p])
  guard $ not $ any (\(a, b) -> (a, b) `elem` conflicts || (b, a) `elem` conflicts) pairs
  return p

-- EXERCISE 4
data Result a = Failure String | Success a [String] deriving (Show, Eq)

-- (a)
instance Functor Result where
  fmap _ (Failure e) = Failure e
  fmap f (Success a ws) = Success (f a) ws

instance Applicative Result where
  pure a = Success a []
  Failure e <*> _ = Failure e
  Success _ _ <*> Failure e = Failure e
  Success f w1 <*> Success a w2 = Success (f a) (w1 ++ w2)

instance Monad Result where
  return = pure
  Failure e >>= _ = Failure e
  Success a w1 >>= f = case f a of
    Failure e -> Failure e
    Success b w2 -> Success b (w1 ++ w2)

-- (b)
warn :: String -> Result ()
warn msg = Success () [msg]

failure :: String -> Result a
failure = Failure

-- (c)
validateAge :: Int -> Result Int
validateAge age
  | age < 0   = failure "Negative age"
  | age > 150 = warn "Age above 150" >> return age
  | otherwise = return age

validateAges :: [Int] -> Result [Int]
validateAges = mapM validateAge

-- EXERCISE 5
data Expr = Lit Int | Add Expr Expr | Mul Expr Expr | Neg Expr deriving (Show, Eq)

simplify :: Expr -> Writer [String] Expr
simplify (Lit n) = return (Lit n)
simplify (Add e1 e2) = do
  s1 <- simplify e1
  s2 <- simplify e2
  case (s1, s2) of
    (Lit 0, e) -> tell ["Add identity: 0 + e -> e"] >> return e
    (e, Lit 0) -> tell ["Add identity: e + 0 -> e"] >> return e
    (Lit a, Lit b) -> tell ["Constant folding: a + b -> a+b"] >> return (Lit (a + b))
    _ -> return (Add s1 s2)
simplify (Mul e1 e2) = do
  s1 <- simplify e1
  s2 <- simplify e2
  case (s1, s2) of
    (Lit 1, e) -> tell ["Mul identity: 1 * e -> e"] >> return e
    (e, Lit 1) -> tell ["Mul identity: e * 1 -> e"] >> return e
    (Lit 0, _) -> tell ["Zero absorption: 0 * e -> 0"] >> return (Lit 0)
    (_, Lit 0) -> tell ["Zero absorption: e * 0 -> 0"] >> return (Lit 0)
    (Lit a, Lit b) -> tell ["Constant folding: a * b -> a*b"] >> return (Lit (a * b))
    _ -> return (Mul s1 s2)
simplify (Neg e) = do
  s <- simplify e
  case s of
    Neg e' -> tell ["Double negation: -(-e) -> e"] >> return e'
    _ -> return (Neg s)