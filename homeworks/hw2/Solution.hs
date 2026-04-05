module Solution where
import Data.Foldable (toList)

data Sequence a = Empty | Single a | Append (Sequence a) (Sequence a) deriving (Show, Eq)

-- EXERCISE 1
instance Functor Sequence where
    fmap _ Empty = Empty
    fmap f (Single x) = Single (f x)
    fmap f (Append l r) = Append (fmap f l) (fmap f r)

-- EXERCISE 2
instance Foldable Sequence where
    foldMap _ Empty = mempty
    foldMap f (Single x) = f x
    foldMap f (Append l r) = foldMap f l <> foldMap f r

seqToList :: Sequence a -> [a]
seqToList = toList

seqLength :: Sequence a -> Int
seqLength = length

-- EXERCISE 3
instance Semigroup (Sequence a) where
    Empty <> x = x
    x <> Empty = x
    l <> r = Append l r

instance Monoid (Sequence a) where
    mempty = Empty

-- EXERCISE 4
tailElem :: Eq a => a -> Sequence a -> Bool
tailElem target seq = go [seq]
  where
    go [] = False
    go (Empty : rest) = go rest
    go (Single x : rest)
      | x == target = True
      | otherwise   = go rest
    go (Append l r : rest) = go (l : r : rest)

-- EXERCISE 5
tailToList :: Sequence a -> [a]
tailToList seq = go [seq] []
  where
    go [] acc = acc
    go (Empty : rest) acc = go rest acc
    go (Single x : rest) acc = go rest (x : acc)
    go (Append l r : rest) acc = go (r : l : rest) acc

-- EXERCISE 6
data Token = TNum Int | TAdd | TSub | TMul | TDiv
  deriving (Show, Eq)

tailRPN :: [Token] -> Maybe Int
tailRPN tokens = go tokens []
  where
    go [] [result] = Just result
    go [] _        = Nothing
    
    go (TNum n : ts) stack = go ts (n : stack)
    go (TAdd : ts) (y : x : stack) = go ts ((x + y) : stack)
    go (TSub : ts) (y : x : stack) = go ts ((x - y) : stack)
    go (TMul : ts) (y : x : stack) = go ts ((x * y) : stack)
    go (TDiv : ts) (0 : x : stack) = Nothing
    go (TDiv : ts) (y : x : stack) = go ts ((x `div` y) : stack)
    
    go _ _ = Nothing

-- EXERCISE 7
-- (a)
myReverse :: [a] -> [a]
myReverse xs = foldl (\acc x -> x : acc) [] xs

-- (b)
myTakeWhile :: (a -> Bool) -> [a] -> [a]
myTakeWhile p xs = foldr (\x acc -> if p x then x : acc else []) [] xs

-- (c)
decimal :: [Int] -> Int
decimal xs = foldl (\acc x -> acc * 10 + x) 0 xs

-- EXERCISE 8
-- (a)
encode :: Eq a => [a] -> [(a, Int)]
encode xs = foldr step [] xs
  where
    step x [] = [(x, 1)]
    step x ((y, n) : acc)
      | x == y    = (y, n + 1) : acc
      | otherwise = (x, 1) : (y, n) : acc

-- (b)
decode :: [(a, Int)] -> [a]
decode pairs = foldr (\(x, n) acc -> replicate n x ++ acc) [] pairs