module Main where

import Control.Concurrent.STM
import Control.Concurrent
import qualified Data.Sequence as S
import Control.Monad (unless)
import System.Environment (getArgs)
import System.IO (readFile)

data Request = Request
  { _disk :: Int
  , _requester :: Int
  , _issued :: (TVar Bool)
  }

data ServeState = ServeState
  { _activeThreads :: Int
  , _currentRequests :: Int
  , _lastRequest :: Int
  , _queue :: S.Seq Request
  }

data Servicer = Servicer Int (TVar ServeState)

newRequest :: Int -> Int -> IO Request
newRequest n r = atomically $ do
  popped <- newTVar False
  return $ Request n r popped

newServicer :: Int -> Int -> IO Servicer
newServicer n threads = atomically $ do
  st <- newTVar $ ServeState threads 0 0 S.empty
  return $ Servicer n st

issueRequest :: Int -> Int -> Servicer -> IO ()
issueRequest n r (Servicer cap tv)= do
  req@(Request disk requester tvpopped) <- newRequest n r
  atomically $ do
    ServeState active current last queue <- readTVar tv
    check $ current < cap
    writeTVar tv $ ServeState active (current+1) last (queue S.|> req)
  putStrLn $ "requester " ++ show r ++ " track " ++ show n
  atomically $ do
    popped <- readTVar tvpopped
    check popped

removeRequester :: Servicer -> IO ()
removeRequester (Servicer cap tv) = do
  atomically $ do
    ServeState active current last queue <- readTVar tv
    writeTVar tv $ ServeState (active-1) current last queue

serviceRequest :: Servicer -> IO ()
serviceRequest (Servicer cap tv) = do
  (track,req) <- atomically $ do
    ServeState active current last queue <- readTVar tv
    check $ (current >= cap) || (current >= active)
    let go i' a (i,b,min) = if (abs (_disk a - _disk b) < min)
                          then (i', a, abs (_disk a - _disk b))
                          else (i, b, min)
        (ind, Request t r iss, _) = S.foldrWithIndex go (0, S.index queue 0, maxBound :: Int) queue
    writeTVar iss False
    writeTVar tv $ ServeState active (current-1) t (S.deleteAt ind queue)
    return (t,r)
  putStrLn $ "service requester " ++ show req ++ " track " ++ show track

runServicer :: Servicer -> IO ()
runServicer s@(Servicer cap tv) = do
  active <- atomically $ fmap _activeThreads $ readTVar tv
  unless (active == 0) $ do
    serviceRequest s
    runServicer s

runRequester :: String -> Int -> Servicer -> IO ()
runRequester f req_numb server = do
  disk_numbs <- fmap (map read . lines) $ readFile f
  mapM_ (\n -> issueRequest n req_numb server) disk_numbs
  removeRequester server

main :: IO ()
main = do
  (capacity:filenames) <- getArgs
  server <- newServicer (read capacity) (length filenames)
  mapM_ (forkIO . \(f, n) -> runRequester f n server) (zip filenames [0..])
  runServicer server
