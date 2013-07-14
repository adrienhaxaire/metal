import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM
import qualified Data.ByteString as B

data Resp = Resp { respHeader :: [String] 
                 , respData :: B.ByteString}
           deriving (Eq, Ord, Show)
type Response = TVar Resp

data Request = Request { reqHeader :: [String] 
                       , reqData :: B.ByteString}
               deriving (Eq, Ord, Show)

-- have to parse request properly

emptyRequest :: Request
emptyRequest = Request [] B.empty

hasRequestMethod :: String -> Request -> Bool
hasRequestMethod m req = any (\l -> (take $ length m) l == m) $ reqHeader req

isPostRequest :: Request -> Bool
isPostRequest = hasRequestMethod "POST"

isGetRequest :: Request -> Bool
isGetRequest = hasRequestMethod "GET"

addToRequestHeader :: String -> Request -> Request
addToRequestHeader line req = req {reqHeader = reqHeader req ++ [init line]}

readRequest :: Handle -> IO Request
readRequest handle = go handle emptyRequest
    where 
      go h r = do
        isReady <- hReady handle
        if isReady 
        then do line <- hGetLine handle
                go h $ addToRequestHeader line r
        else return r


serve :: Handle -> IO ()
serve handle = do
  hSetBuffering handle NoBuffering
  request <- readRequest handle
  print request
  hClose handle -- do not close if keep alive
--  response <- respond request
--  hPutStr responseHeader

server :: IO ()
server = withSocketsDo $ do
  socket <- listenOn (PortNumber 8002)
  forever $ do
      (handle, hostname, port) <- accept socket
      forkIO $ serve handle

main = server

