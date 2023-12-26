{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Char8 as BS
import System.Environment (getArgs)
import GHC.Generics
import Control.Monad (forM, forM_)
import Data.Typeable
import Data.Binary (Binary)
import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Network.Transport.TCP (createTransport, defaultTCPParameters, defaultTCPAddr)
import Network.Transport (Transport, EndPointAddress(..))
import Control.Concurrent (forkIO)


replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg


data MyMessage = StringMsg (String, String) |
               IntMsg (Int, Int) |
               QueryMsg ProcessId |
               ResultMsg (String, Int) |
               TerminateMsg deriving (Generic, Binary)

data (Show a) => Result a = Result { _getResult :: a }
instance (Show a) => Show (Result a) where
  show a = show $ _getResult a

showResult :: (Show a) => Result a -> Process ()
showResult a = say $ show a

addNumbers :: (Int, Int) -> Int
addNumbers (x, y) = (x + y)

stringTransform :: String -> String
stringTransform str = reverse str

stringTransformWrapper :: (String, String) -> String
stringTransformWrapper (one, two) = stringTransform $ show one ++ show two

slaveProcess :: (String, Int) -> Process ()
slaveProcess (s, i) = do
  say $ "before: string: " <> s <> "and number:" <> show i
  (s', i') <- receiveWait[match commandCatch]
  say $ "after: string: " <> s' <> "and number:" <> show i'
  slaveProcess (s', i')
  where
    commandCatch (QueryMsg pid) = do
      send pid (s, i)
      a <- getProcessInfo pid
      say $ show a
      say $ "send to master"
      return (s, i)
    commandCatch (StringMsg (s1, s2)) = return (id (s <> (stringTransformWrapper (s1, s2))), i)
    commandCatch (IntMsg (i1, i2)) = return (s, (i + (addNumbers (i1, i2))))
    commandCatch TerminateMsg = do
      say "died"
      terminate

query :: ProcessId -> ProcessId -> Process (String, Int)
query cpid mypid = do
  send cpid (QueryMsg mypid)
  expect


remotable ['slaveProcess]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

getStats :: NodeId -> Process ()
getStats n = do
  b <- getNodeStats n
  say $ show b

master :: Process ()
master = do
  say "master started"
  -- Get information about available slave nodes
  slaves <- findSlaves
  say "looking for slaves"
  forM_ slaves getStats 
  say $ show slaves
  mypid <- getSelfPid
  -- forM_ slaves $ \process -> send process "hello" >> send process (mypid, "hello")
  -- forM_ slaves $ \process -> monitorNode process
  -- say "say hello!"
  processes <- getProcess slaves
  forM_ processes $ \process -> monitor process
  say "start processes on slaves"
  -- Send tasks to the slave nodes
  say "send tasks"
  sendTasks processes mypid

expectLoop :: Process ()
expectLoop = do
  e <- expect
  say e

findSlaves :: Process [NodeId]
findSlaves = do
  -- Discover and list available slave nodes (you may need to replace these with actual slave node addresses)
  -- For simplicity, we assume that slave nodes are running on the same machine.
  return $ map NodeId [EndPointAddress . BS.pack $ "127.0.0.1:8081:0", EndPointAddress . BS.pack $ "127.0.0.1:8080:0"]

getProcess :: [NodeId] -> Process [ProcessId]
getProcess = do
  traverse (\slave -> spawn slave $ $(mkClosure 'slaveProcess) ("" :: String, 0 :: Int))

sendTasks :: [ProcessId] -> ProcessId -> Process ()
sendTasks processes mypid = do
  -- Distribute tasks to slaves
  let task1 = [(1, 2), (3, 4)] :: [(Int, Int)] -- Tasks to distribute
  let task2 = [("1", "2"), ("3", "4")] :: [(String, String)] -- Tasks to distribute
  forM_ (zip3 task1 task2 processes) $ \(taskInt, taskStr, process) -> do
    send process $ IntMsg taskInt
    send process $ StringMsg taskStr
  a <- forM processes $ (\process -> query process mypid)
  say $ "get results"
  forM_ a (\a -> say $ "string: " <> fst a <> "and number:" <> show (snd a))
  forM_ processes $ (\process -> send process (TerminateMsg))
  let count = 0 :: Int
  forever $ do
    say "start waiting"
    reason <- expect :: Process ProcessMonitorNotification
    say $ show reason
    -- liftIO $ threadDelay 1000000
    -- let count = count + 1
    -- if (count == 2) then terminate
    -- else (say "hayo") 




type Host = String
type Port = String
getTransport :: Host -> Port -> IO Transport
getTransport host port = do
  Right transport <- flip createTransport defaultTCPParameters $ defaultTCPAddr host port
  return transport


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
        master_transport <- getTransport host port
        node <- newLocalNode master_transport myRemoteTable
        runProcess node $ master
    ["slave", host, port] -> do
        master_transport <- getTransport host port
        node <- newLocalNode master_transport myRemoteTable
        runProcess node $ do
          a <- getLocalNodeStats 
          say $ show a
          receiveWait[match logMessage, match replyBack]


-- main :: IO ()
-- main = do
--   print "start"
--   master_transport <- getTransport "localhost" "10500"
--   slave1_transport <- getTransport "localhost" "10501"
--   slave2_transport <- getTransport "localhost" "10502"
--   print "start"
  
--   slave_local_nodes <- flip traverse [slave1_transport, slave2_transport] (\tr -> newLocalNode tr myRemoteTable)
--   print "start"
--   let slave_nodes = map localNodeId slave_local_nodes
--   print "start"
--   slaves_process <- forM slave_local_nodes (\tr -> 
--     forkProcess tr (receiveWait[]))
--   print "slave in work"
--   node <- newLocalNode master_transport myRemoteTable
--   runProcess node $ master

