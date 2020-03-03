module ChitChat.Exe.Server
  ( main
  ) where

import ChitChat.Ast (commandP)

import Control.Concurrent (forkFinally)
import Control.Exception (bracket)
import Control.Monad (unless, forever, void)
import Data.ByteString.UTF8 (fromString, toString)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (getArgs)
import Text.Megaparsec (parse)


main :: IO ()
main = do
  [port] <- getArgs
  runTCPServer Nothing port mainLoop

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  bracket (open addr) close loop
  where
    resolve = do
      let hints = defaultHints {
          addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
      }
      head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock $ setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $ do
      (conn, _peer) <- accept sock
      void $ forkFinally (server conn) (const $ gracefulClose conn 5000)

mainLoop :: Socket -> IO ()
mainLoop s = do
  msg <- recv s 1024
  let command = toString msg
  unless (null command) $ do
    let result = runCommand command
    sendAll s (fromString result)
    mainLoop s

runCommand :: String -> String
runCommand input = 
  case (parse commandP "stdin" input) of    
    Left  error -> "ERROR: " ++ show error
    Right _ -> "PARSED!"