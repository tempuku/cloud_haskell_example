{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


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
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Network.Transport (Transport, EndPointAddress(..))
import Control.Concurrent (forkIO)
import Control.Distributed.Process.Backend.SimpleLocalnet

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
stringTransformWrapper (one, two) = stringTransform $ show one <> show two

slaveProcess :: (String, Int) -> Process ()
slaveProcess (s, i) = do
  say $ s <> show i
  (s', i') <- receiveWait[match commandCatch]
  say $ s' <> show i'
  slaveProcess (s', i')
  where
    commandCatch (QueryMsg pid) = do
      send pid (s, i)
      return (s, i)
    commandCatch (StringMsg (s1, s2)) = return ((s <> (stringTransformWrapper (s1, s2))), i)
    commandCatch (IntMsg (i1, i2)) = return (s, (i + (addNumbers (i1, i2))))
    commandCatch TerminateMsg = terminate

query :: ProcessId -> ProcessId -> Process ()
query cpid mypid = do
  send cpid (QueryMsg mypid)


remotable ['slaveProcess]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  say $ "Slaves: " ++ show slaves
  say "master started"
  -- Get information about available slave nodes
  -- slaves <- findSlaves
  -- say "master started"

  -- processes <- getProcess slaves
  -- say "master started"
  -- mypid <- getSelfPid

  -- forM_ processes $ \process -> send process "hello" >> send process (mypid, "hello")
  -- forM_ processes $ \process -> monitor process
  -- forM_ slaves $ \process -> monitorNode process
  -- -- Send tasks to the slave nodes
  -- say "master started"
  -- sendTasks processes mypid

expectLoop :: Process ()
expectLoop = do
  e <- expect
  say e

-- findSlaves :: Process [NodeId]
-- findSlaves = do
--   -- Discover and list available slave nodes (you may need to replace these with actual slave node addresses)
--   -- For simplicity, we assume that slave nodes are running on the same machine.
--   return $ map NodeId [EndPointAddress . BS.pack $ "localhost:10501", EndPointAddress . BS.pack $ "localhost:10502"]

-- getProcess :: [NodeId] -> Process [ProcessId]
-- getProcess = do
--   traverse (\slave -> spawn slave $ $(mkClosure 'slaveProcess) ("" :: String, 0 :: Int))

-- sendTasks :: [ProcessId] -> ProcessId -> Process ()
-- sendTasks processes mypid = do
--   -- Distribute tasks to slaves
--   let task1 = [(1, 2), (3, 4)] :: [(Int, Int)] -- Tasks to distribute
--   let task2 = [("1", "2"), ("3", "4")] :: [(String, String)] -- Tasks to distribute
--   forM_ (zip3 task1 task2 processes) $ \(taskInt, taskStr, process) -> do
--     send process $ IntMsg taskInt
--     send process $ StringMsg taskStr
--   say "master started"
--   a <- forM processes $ (\process -> query process mypid)
--   say "master started"
--   expectLoop
  -- forM a $ \(s1, i1) -> do 
  --   say s1
  --   say . show $ i1
  -- return ()


-- type Host = String
-- type Port = String
-- getTransport :: Host -> Port -> IO Transport
-- getTransport host port = do
--   Right transport <- flip createTransport defaultTCPParameters $ defaultTCPAddr host port
--   return transport


main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port myRemoteTable
      startSlave backend


-- main :: IO ()
-- main = do
--   args <- getArgs

--   case args of
--     ["master", host, port] -> do
--         master_transport <- getTransport host port
--         node <- newLocalNode master_transport myRemoteTable
--         runProcess node $ master
--     ["slave", host, port] -> do
--         master_transport <- getTransport host port
--         node <- newLocalNode master_transport myRemoteTable
--         runProcess node $ (receiveWait[match logMessage, match replyBack])


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

