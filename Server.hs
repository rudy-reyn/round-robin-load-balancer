{-# LANGUAGE OverloadedStrings #-}
-- 06/15/22
-- Server.hs
import System.Environment (getArgs)
import Control.Monad (unless, forever, void)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import TCP

runServer port s = do
    msg <- recv s 1024
    unless (BS.null msg) $ do
        received msg >>= C.putStr
        response msg >>= sendAll s
        runServer port s

main :: IO ()
main = do
    args <- getArgs
    let port = getPort args
    putStrLn $ "Starting server on port " ++ (show port)
    let talk = runServer port
    runTCPServer Nothing port talk
