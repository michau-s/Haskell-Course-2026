module Solution where

newtype Reader r a = Reader { runReader :: r -> a }

-- EXERCISE 1
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ \env -> f (ra env)

instance Applicative (Reader r) where
  pure   :: a -> Reader r a
  pure x = Reader $ \_env -> x

  liftA2 :: (a -> b -> c) -> Reader r a -> Reader r b -> Reader r c
  liftA2 f (Reader ra) (Reader rb) = Reader $ \env -> f (ra env) (rb env)

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= f = Reader $ \env -> runReader (f (ra env)) env

-- EXERCISE 2
ask :: Reader r r
ask = Reader $ \env -> env

asks :: (r -> a) -> Reader r a
asks f = Reader $ \env -> f env

local :: (r -> r) -> Reader r a -> Reader r a
local modifier (Reader ra) = Reader $ \env -> ra (modifier env)

-- EXERCISE 3
data BankConfig = BankConfig
  { interestRate   :: Double
  , transactionFee :: Int
  , minimumBalance :: Int
  } deriving (Show)

data Account = Account
  { accountId :: String
  , balance   :: Int
  } deriving (Show)

calculateInterest :: Account -> Reader BankConfig Int
calculateInterest acc = do
  rate <- asks interestRate
  return $ round (fromIntegral (balance acc) * rate)

applyTransactionFee :: Account -> Reader BankConfig Account
applyTransactionFee acc = do
  fee <- asks transactionFee
  return $ acc { balance = balance acc - fee }

checkMinimumBalance :: Account -> Reader BankConfig Bool
checkMinimumBalance acc = do
  minBal <- asks minimumBalance
  return $ balance acc >= minBal

processAccount :: Account -> Reader BankConfig (Account, Int, Bool)
processAccount acc = do
  acc'     <- applyTransactionFee acc
  interest <- calculateInterest acc
  isMinMet <- checkMinimumBalance acc
  return (acc', interest, isMinMet)