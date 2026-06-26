module Types where

data Size = Px Int | Pct Double deriving (Show, Eq)
data Direction = Row | Col deriving (Show, Eq)

data Props = Props
  { width  :: Size
  , height :: Size
  , dir    :: Direction
  , color  :: Maybe String
  } deriving (Show, Eq)

data Layout = Box Props [Layout] deriving (Show, Eq)

data Window = Window String Int Int Layout deriving (Show, Eq)

data Resolved = Resolved
  { rx, ry, rw, rh :: Int
  , rColor         :: Maybe String
  , rChildren      :: [Resolved]
  } deriving (Show, Eq)