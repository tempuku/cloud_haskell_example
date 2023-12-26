{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (forM, forM_)
import Data.Typeable
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters, defaultTCPAddr)


data Result a = Result a deriving Show

showResult :: Result a -> Process ()
showResult a = say $ show a

addNumbers :: Int -> Int -> Process Int
addNumbers x y = return (x + y)

stringTransform :: String -> Process String
stringTransform str = return (reverse str)

stringTransformWrapper :: Int -> Int -> Process String
stringTransformWrapper one two = stringTransform $ show one <> show two

remotable ['addNumbers, 'stringTransformWrapper]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Process ()
master = do
  

  -- Get information about available slave nodes
  slaves <- findSlaves

  -- Send tasks to the slave nodes
  spawn  slaves


findSlaves :: Process [NodeId]
findSlaves = do
  -- Discover and list available slave nodes (you may need to replace these with actual slave node addresses)
  -- For simplicity, we assume that slave nodes are running on the same machine.
  return $ map NodeId ["localhost:10501", "localhost:10502"]

sendTasks :: [NodeId] -> Process ()
sendTasks slaves = do
  -- Distribute tasks to slaves
  let tasks = [(1, 2), (3, 4)] -- Tasks to distribute
  let actions = cycle $ [addNumbers, stringTransformWrapper]
  results <- forM (zip tasks slaves actions) $ \(task, slave, action) -> do
    pid <- spawn slave $ $(mkClosure 'action) task
    result <- expect
    result
  forM_ results showResult 
  return ()

main :: IO ()
main = do
  Right transport <- flip createTransport defaultTCPParameters $ defaultTCPAddr "localhost" "10500"
  node <- newLocalNode transport myRemoteTable
  runProcess node master


{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}

import qualified Data.ByteString.Char8 as BS
import Control.Monad (forM, forM_)
import Data.Typeable
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters, defaultTCPAddr)
import Network.Transport


data Message = StringMsg (String, String) |
               NumberMsg (Int, Int)

data (Show a) => Result a = Result { _getResult :: a }
instance (Show a) => Show (Result a) where
  show a = show $ _getResult a

showResult :: (Show a) => Result a -> Process ()
showResult a = say $ show a

addNumbers :: (Int, Int) -> Process ()
addNumbers (x, y) = do 
  let a = (x + y)
  say $ show a

stringTransform :: String -> Process ()
stringTransform str = do 
  let a = reverse str
  say a

stringTransformWrapper :: (Int, Int) -> Process ()
stringTransformWrapper (one, two) = stringTransform $ show one <> show two

remotable ['addNumbers, 'stringTransformWrapper]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Process ()
master = do

  -- Get information about available slave nodes
  slaves <- findSlaves

  -- Send tasks to the slave nodes
  sendTasks slaves


findSlaves :: Process [NodeId]
findSlaves = do
  -- Discover and list available slave nodes (you may need to replace these with actual slave node addresses)
  -- For simplicity, we assume that slave nodes are running on the same machine.
  return $ map NodeId [EndPointAddress (BS.pack "localhost:10501"), EndPointAddress ( BS.pack "localhost:10502")]

sendTasks :: [NodeId] -> Process ()
sendTasks slaves = do
  -- Distribute tasks to slaves
  let task1 = [(1, 2), (3, 4)] :: [(Int, Int)] -- Tasks to distribute
  let task2 = [(1, 2), (3, 4)] :: [(Int, Int)] -- Tasks to distribute

  results1 <- forM (zip task1 slaves) $ \(task, slave) -> do
    pid <- spawn slave $ $(mkClosure 'addNumbers) task
    expect
  forM_ results1 showResult 

  results2 <- forM (zip task2 slaves) $ \(task, slave) -> do
    pid <- spawn slave $ $(mkClosure 'stringTransformWrapper) task
    expect
  forM_ results2 showResult 
  return ()

main :: IO ()
main = do
  Right transport <- flip createTransport defaultTCPParameters $ defaultTCPAddr "localhost" "10500"
  node <- newLocalNode transport myRemoteTable
  runProcess node master
