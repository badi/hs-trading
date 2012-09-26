{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wall #-}

import Control.Applicative
import Control.Monad.State

type Time = Double

data Tick a = MkTick {
      time :: Time
    , datum :: a
    }

type Stream a = [Tick a]


newtype Money = MkMoney Double deriving Num

data Stock

data TradeState t = MkTradeState {
      startTime   :: Time
    , currentTime :: Time
    , investment  :: Money
    , gain        :: Money
    , stock       :: Stock
    , stream      :: Stream t
    }

newTradeState :: Money -> Stock -> Stream t -> TradeState t
newTradeState m s t = MkTradeState {
                        startTime   = 0
                      , currentTime = 0
                      , investment  = m
                      , gain        = MkMoney 0
                      , stock       = s
                      , stream      = t
                      }


newtype Trade t a = MkTrade {
      runTrade :: StateT (TradeState t) IO a
    } deriving (Functor, Monad, MonadState (TradeState t), MonadIO)



getCurrentTime :: IO Time
getCurrentTime = undefined



start :: Trade t ()
start = do
  t <- liftIO getCurrentTime
  modify (\s -> s { startTime = t, currentTime = t })

tickToMoney :: Tick t -> Money
tickToMoney = undefined

tick :: Monad m => Stream a -> m (Tick a)
tick = undefined


getCurrentStockValue :: Trade t Money
getCurrentStockValue = do
  s <- liftIO =<< tick . stream <$> get
  return $ tickToMoney s


getReturn :: Trade t Money
getReturn = do
  m0 <- investment <$> get
  m1 <- getCurrentStockValue
  let g = m1 - m0
  modify (\s -> s { gain = g })
  return g

data Decision = Continue | Stop deriving Eq

decide :: Trade t Decision
decide = do
  MkMoney r <- getReturn
  return $ case compare r 0 of
             GT -> Continue
             EQ -> Stop
             LT -> Stop

stop :: Trade t ()
stop = undefined

loop :: Trade t ()
loop = do
  d <- decide
  case d of
    Stop     -> stop
    Continue -> loop


run :: TradeState t -> Trade t a -> IO a
run s t =  fst <$> flip runStateT s (runTrade t)



main :: IO Money
main =
    let input  = undefined :: Money
        stk    = undefined :: Stock
        ticker = undefined :: Stream t
        strt  = newTradeState input stk ticker
    in run strt  $ do
      start
      loop
      gain <$> get
  
