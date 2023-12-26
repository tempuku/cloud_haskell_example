{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


import Text.Printf
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

type Host = String
type Port = String
getTransport :: Host -> Port -> IO Transport
getTransport host port = do
  Right transport <- flip createTransport defaultTCPParameters $ defaultTCPAddr host port
  return transport

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg



data MyMessage = Ping ProcessId
             | Pong ProcessId
  deriving (Typeable, Generic)          -- 1

instance Binary MyMessage  

pingServer :: Process ()
pingServer = do
  Ping from <- expect                              -- 1
  say $ printf "ping received from %s" (show from) -- 2
  mypid <- getSelfPid                              -- 3
  send from (Pong mypid) 

remotable ['pingServer]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

master :: Process ()
master = do
  node <- getSelfNode                               -- 1

  say $ printf "spawning on %s" (show node)
  pid <- spawn node $(mkStaticClosure 'pingServer)  -- 2

  mypid <- getSelfPid                               -- 3
  say $ printf "sending ping to %s" (show pid)
  send pid (Ping mypid)                             -- 4

  Pong _ <- expect                                  -- 5
  say "pong."

  terminate                                         -- 6


main :: IO ()
main = do
  master_transport <- getTransport "localhost" "10500"
  node <- newLocalNode master_transport myRemoteTable
  runProcess node $ master
-- main :: IO ()
-- main = do
--   args <- getArgs

--   case args of
--     ["master", host, port] -> do
--       backend <- initializeBackend host port myRemoteTable
--       startMaster backend (master backend)
--     ["slave", host, port] -> do
--       backend <- initializeBackend host port myRemoteTable
--       startSlave backend


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

