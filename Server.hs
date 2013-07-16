import Network
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.List as L
import Data.List.Split


data Resp = Resp { respHeader :: M.Map String String
                 , respData :: B.ByteString}
           deriving (Eq, Ord, Show)

type Response = TVar Resp

data Method = GET | POST | PUT | UNKNOWN deriving Show

instance Read Method where
    readsPrec _ value = tryParse [("GET", GET)
                                 , ("POST", POST)
                                 , ("PUT", PUT)
                                 , ("", UNKNOWN)]
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
readRequest = readRequestHeader


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

addToRequestHeader :: String -> Request -> Request
addToRequestHeader line req = 
    let [k, v] = splitOn ": " line 
    in req {reqHeader = M.insert k v (reqHeader req)}

-- ok that sucks, will find cleaner way
hasMethod :: String -> Bool
hasMethod line = take (length "HTTP/1.1") (reverse line) == "1.1/PTTH"

setMethodURI :: String -> Request -> Request
setMethodURI line req = 
    let [m, u, p] = splitOn " " line
    in req {reqMethod = read m, reqURI = u}


{-
readRequestData :: Request -> Handle -> IO Request
readRequestData req handle = 
    if isGetRequest req 
    then return req 
    else do 
      rdata <- B.hGet handle 4 -- $ contentLength req
      print rdata
      return $ req {reqData = rdata}

contentLength :: Request -> Int
contentLength req = 
    let cl = "Content-Length: "
        hasCL x = take (length cl) x == cl
        content_length = head $ filter hasCL $ reqHeader req
    in read $ drop (length cl) content_length
-}

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

