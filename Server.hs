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

respond handle = do
  hSetBuffering handle NoBuffering
  reqHeader <- hGetLine handle
  let [m, uri, protocol] = words reqHeader
  case method m of
    GET -> respondGET uri handle
--    POST ->respondPOST uri handle

respond404 :: String -> Handle -> IO ()
respond404 uri handle =
  let h = "HTTP/1.1 404 Not Found\nContent-Type: text/plain\n\n"
          ++ "File " ++ uri ++ " not found.\n"
  in hPutStr handle h

respondGET :: String -> Handle -> IO ()
respondGET uri handle =
  let file = "." ++ uri in do
  exists <- doesFileExist file
  if exists then do contents <- readFile file
                    hPutStr handle $ header contents
  else respond404 file handle

main = withSocketsDo $ do
  socket <- listenOn (PortNumber 8002)
  (handle, hostname, port) <- accept socket
  respond handle





