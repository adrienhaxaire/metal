import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM
import System.Directory

type Response = TVar String

prepareResponse :: String -> STM Response
prepareResponse s = do
  r <- newTVar ""
  let b = "HTTP/1.1 200 OK \nContent-Type: text/plain\n\n" ++ s ++ "\n"
  writeTVar r b
  return r

showResponse :: String -> IO String
showResponse lines = atomically $ prepareResponse lines >>= readTVar

serve :: Handle -> IO ()
serve handle = do
  hSetBuffering handle NoBuffering
  lines <- hGetLine handle
  r <- showResponse lines
  hPutStr handle r
  hClose handle

server :: IO ()
server = withSocketsDo $ do
  socket <- listenOn (PortNumber 8002)
  forever $ do
      (handle, hostname, port) <- accept socket
      forkIO $ serve handle

main = server


