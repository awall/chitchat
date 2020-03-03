{-# LANGUAGE OverloadedStrings #-}

module ChitChat.Exe.Client
  ( main
  ) where


import Control.Exception (bracket)
import Control.Monad.State (when)
import Data.List (isSuffixOf)
import Data.IORef
import Data.ByteString.UTF8 (fromString, toString)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)


main :: IO ()
main = do
  [ip, port] <- getArgs
  buffer <- newIORef ""
  runTCPClient ip port (mainLoop buffer)

runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock $ addrAddress addr
        return sock

mainLoop :: IORef String -> Socket -> IO ()
mainLoop buffer s = do
  input <- getLine
  modifyIORef buffer (++ input)
  when (endsWithSemicolon input) $ do
    fullValue <- readIORef buffer
    writeIORef buffer ""
    let sansSemicolon = withoutLastChar fullValue
    sendAll s (fromString sansSemicolon)
    response <- recv s 1024
    putStrLn (toString response)
  mainLoop buffer s
  where
    endsWithSemicolon x = ";" `isSuffixOf` x
    withoutLastChar' accum [x] = accum
    withoutLastChar' accum (x:xs) = withoutLastChar' (accum ++ [x]) xs
    withoutLastChar x = withoutLastChar' "" x