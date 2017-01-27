module Main where

import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Sequence as S

data Request = Request Int (TVar Bool)

data ServeState = ServeState
  { _activeThreads :: Int
  , _currentRequests :: Int
  , _lastRequest :: Int
  , _queue :: S.Seq Request
  }

data Servicer = Servicer Int (TVar ServeState)

newRequest :: Int -> IO Request
newRequest n = atomically $ do
  popped <- newTVar False
  return $ Request n popped

newServicer :: Int -> Int -> IO Servicer
newServicer n threads = atomically $ do
  st <- newTVar $ ServeState threads 0 0 S.empty
  return $ Servicer n st

issueRequest :: Int -> Servicer -> IO ()
issueRequest n (Servicer cap tv)= do
  req@(Request disk tvpopped) <- newRequest n
  atomically $ do
    ServeState active current last queue <- readTVar tv
    check $ current < cap
    writeTVar tv $ ServeState active (current+1) last (queue S.|> req)
  atomically $ do
    popped <- readTVar tvpopped
    check popped


main :: IO ()
main = do
  putStrLn "hello world"
