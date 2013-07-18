import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.List.Split
import Data.Maybe

data Resp = Resp { respHeader :: M.Map String String
                 , respData :: B.ByteString}
           deriving (Eq, Ord, Show)

type Response = TVar Resp

data Method = GET | POST | PUT | UNKNOWN deriving Show

-- thank you Real World Haskell!
instance Read Method where
    readsPrec _ value = tryParse [("GET", GET), ("POST", POST), ("PUT", PUT), ("", UNKNOWN)]
        where tryParse [] = []
              tryParse ((attempt, result):xs) =
                      if take (length attempt) value == attempt
                      then [(result, drop (length attempt) value)]
                      else tryParse xs

type URI = FilePath

data Request = Request { reqMethod :: Method
                       , reqURI :: FilePath
                       , reqHeader :: M.Map String String 
                       , reqData :: B.ByteString}
               deriving Show

emptyRequest :: Request
emptyRequest = Request UNKNOWN "" M.empty B.empty

readRequest :: Handle -> IO Request
readRequest handle = do
  req <- readRequestHeader handle
  readRequestData req handle

readRequestHeader :: Handle -> IO Request
readRequestHeader handle = do
  hSetNewlineMode handle universalNewlineMode
  go handle emptyRequest
    where 
      go h r = do
        isReady <- hReady handle
        if isReady 
        then do line <- hGetLine handle
                if hasMethod line then go h $ setMethodURI line r
                else if ':' `elem` line then go h $ addToRequestHeader line r
                else return r
        else return r

setMethodURI :: String -> Request -> Request
setMethodURI line req = 
    let [m, u, p] = splitOn " " line
    in req {reqMethod = read m, reqURI = u}

addToRequestHeader :: String -> Request -> Request
addToRequestHeader line req = 
    let [k, v] = splitOn ": " line 
    in req {reqHeader = M.insert k v (reqHeader req)}

-- ok that sucks, will find cleaner way
hasMethod :: String -> Bool
hasMethod line = take (length "HTTP/1.1") (reverse line) == "1.1/PTTH"

requestMethod :: Request -> Method
requestMethod = reqMethod

isRequestType :: Method -> Request -> Bool
isRequestType m req = isJust $ M.lookup (show m) (reqHeader req)

isGetRequest, isPostRequest, isPutRequest :: Request -> Bool
isGetRequest = isRequestType GET
isPostRequest = isRequestType POST
isPutRequest = isRequestType PUT

hasData :: Request -> Bool
hasData req = isJust $ M.lookup "Content-Length" (reqHeader req)

readRequestData :: Request -> Handle -> IO Request
readRequestData req handle = 
    if not $ hasData req then return req 
    else do 
      rdata <- B.hGet handle $ contentLength req
      return $ req {reqData = rdata}

contentLength :: Request -> Int
contentLength req = read $ fromMaybe "0" $ M.lookup "Content-Length" (reqHeader req)

respond :: Handle -> Request -> IO ()
respond handle req = case reqMethod req of
                       GET -> respondGet handle req
                       POST -> respondPost handle req
                       PUT -> respondPut handle req
                       UNKNOWN -> error "Wrong type of request" -- TODO

respondGet :: Handle -> Request -> IO ()
respondGet handle req = do
  hPutStrLn handle "HTTP/1.1 200 OK"
  hPutStrLn handle "Content-Type: text/plain\n"
  hPutStrLn handle $ show req

respondPost :: Handle -> Request -> IO ()
respondPost = respondGet

respondPut :: Handle -> Request -> IO ()
respondPut handle req = do
    fh <- openFile ('.' : reqURI req) WriteMode
    B.hPut fh (reqData req)
    hClose fh
    hPutStrLn handle "HTTP/1.1 200 OK"
    hPutStrLn handle "Content-Type: text/plain\n"
    hPutStrLn handle "Upload successful"

serve :: Handle -> IO ()
serve handle = do
  hSetBuffering handle NoBuffering
  request <- readRequest handle
  respond handle request
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

