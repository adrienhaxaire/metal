import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM
import System.Directory

data Method = GET | POST deriving (Show)

method :: String -> Method
method s = case s of
    "GET" -> GET
    "PUT" -> POST

header :: String -> String
header s = "HTTP/1.1 200 OK \nContent-Type: text/html\n\n" ++ s ++ "\n"

respond :: Handle -> IO ()
respond handle = do
  hSetBuffering handle NoBuffering
  reqHeader <- hGetLine handle
  let [m, uri, protocol] = words reqHeader
  case method m of
    GET -> respondGET uri handle
--    POST ->respondPOST uri handle

respond404 :: String -> Handle -> IO ()
respond404 uri handle = do
  b <- readFile "404.html"
  let h = "HTTP/1.1 404 Not Found\nContent-Type: text/html\n\n"
  hPutStr handle $ h ++ b
  hClose handle

respondGET :: String -> Handle -> IO ()
respondGET uri handle =
  let file = "." ++ uri in do
  exists <- doesFileExist file
  if exists then do contents <- readFile file
                    hPutStr handle $ header contents
                    hClose handle
  else respond404 file handle

server :: IO()
server = withSocketsDo $ do
  socket <- listenOn (PortNumber 8002)
  forever $ do
      (handle, hostname, port) <- accept socket
      process handle


type Response = TVar String

newResponse :: String -> STM Response
newResponse s = newTVar s

writeToResponse :: Response -> String -> STM ()
writeToResponse r s = do
  writeTVar r s
  return ()

readFromResponse :: Response -> STM String
readFromResponse r = do
  s <- readTVar r
  return s

respondSTM :: String -> IO String
respondSTM line = atomically $ do
  r <- newResponse line
  s <- readFromResponse r
  return s

process :: Handle -> IO ThreadId
process handle = do
  forkIO $ respond handle

-- atomically :: STM a -> IO a
processSTM :: Handle -> IO ()
processSTM handle = do
  hSetBuffering handle NoBuffering
  line <- hGetLine handle
  cts <- respondSTM line
  hPutStr handle $ header cts
  hClose handle

serverSTM :: IO()
serverSTM = withSocketsDo $ do
  socket <- listenOn (PortNumber 8002)
  forever $ do
      (handle, hostname, port) <- accept socket
      processSTM handle

main = serverSTM


