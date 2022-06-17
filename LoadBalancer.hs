{-# LANGUAGE OverloadedStrings #-}
-- 06/15/22
-- LoadBalancer.hs
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import TCP

runLoadBalancer servers = runLoadBalancer' (cycle servers)

runLoadBalancer' servers s = do
    msg <- recv s 1024
    unless (C.null msg) $ do
        forwardMsg nextPort msg >>= C.putStr
        forward msg >>= sendAll s
        runLoadBalancer' nextRound s
    where ((nextIP, nextPort), nextRound) = roundRobin servers
          forward = forwardMessage nextIP nextPort

forwardMessage backendHost backendPort message =
    runTCPClient backendHost backendPort $ \s -> do
        sendAll s message
        recv s 1024

-- [(Server IPs, Server Ports)] -> ((Next IP, Next Port), [Next Servers])
roundRobin :: [Server] -> (Server, [Server])
roundRobin servers = (head servers, tail servers)

-- Example load balancer running on port 8000 with backend servers on ports 8001-8004
main :: IO ()
main = do
    putStrLn "Starting load balancer on port 8000"
    runTCPServer Nothing "8000" $ runLoadBalancer servers
  where servers = map (\p -> ("127.0.0.1", p)) ["8001", "8002", "8003", "8004"]
