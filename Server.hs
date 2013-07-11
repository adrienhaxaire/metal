import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM

type Response = TVar String

prepareResponse :: String -> STM Response
prepareResponse s = newTVar $
                    "HTTP/1.1 200 OK \nContent-Type: text/plain\n\n"
                    ++ s ++ "\n"

showResponse :: String -> IO String
showResponse lines = atomically $ prepareResponse lines >>= readTVar

serve :: Handle -> IO ()
serve handle = do
  hSetBuffering handle NoBuffering
  hGetLine handle >>= showResponse >>= hPutStr handle
  hClose handle

server :: IO ()
server = withSocketsDo $ do
  socket <- listenOn (PortNumber 8002)
  forever $ do
      (handle, hostname, port) <- accept socket
      forkIO $ serve handle

main = server


