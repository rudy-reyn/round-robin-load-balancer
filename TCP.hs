{-# LANGUAGE OverloadedStrings #-}
-- 06/15/22
-- TCP.hs
module TCP where

import Data.Char (isDigit)
import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import qualified Control.Exception as E

import Network.Socket
import System.IO.Error
import qualified Data.Time.Clock as Clock

import Data.ByteString (ByteString, append)
import qualified Data.ByteString.Char8 as C

type Server = (String, String)

-- Server
runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close loop
  where resolve = do
            let hints = defaultHints {
                            addrFlags = [AI_PASSIVE],
                            addrSocketType = Stream
                        }
            head <$> getAddrInfo (Just hints) mhost (Just port)
        open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
            setSocketOption sock ReuseAddr 1
            withFdSocket sock setCloseOnExecIfNeeded
            bind sock $ addrAddress addr
            listen sock 1024
            return sock
        loop sock = forever $ E.bracketOnError (accept sock) (close . fst)
            $ \(conn, _peer) -> void $
                forkFinally (server conn) (const $ gracefulClose conn 5000)

-- Client
-- This is used for the load balancer to communicate with backend servers
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where resolve = do
            let hints = defaultHints { addrSocketType = Stream }
            head <$> getAddrInfo (Just hints) (Just host) (Just port)
        open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
            connect sock $ addrAddress addr
            return sock

-- Logging function that returns a formatted log message with the current time
type Logger = ByteString -> IO ByteString

logger :: ByteString -> Logger
logger typeof message = do
    time <- getCurrentTime
    let byteStrings = [typeof, " [", time, "]: ", message]
    return $ foldl1 append byteStrings

received :: Logger
received = logger "RECEIVED"

response :: Logger
response = logger "RESPONSE"

forwardResponse :: Logger
forwardResponse = logger "FORWARDED RESPONSE"

forwardMsg :: String -> Logger
forwardMsg port = logger (append "FORWARDED TO PORT " (C.pack port))

getCurrentTime :: IO ByteString
getCurrentTime = do time <- Clock.getCurrentTime
                    return $ C.pack (show time)

-- Gets port number to run on from passed arguments
getPort :: [String] -> String
getPort [] = error "Not port provided"
getPort args | (args !! 0) </=> isDigit = error "Invalid port"
getPort args
    | port <= 0 || port > 65535 = error "Port not between 0 and 65535"
    | otherwise = show port
    where port = read (args !! 0)::Int

(<==>) xs f = all f xs
(</=>) xs f = not (all f xs)
