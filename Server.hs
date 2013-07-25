import Network as N
import System.IO
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.STM
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import Data.List.Split
import Data.Maybe
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String
import System.Time



data Method = GET 
            | HEAD 
            | POST 
            | PUT 
            | DELETE 
            | TRACE 
            | CONNECT 
            | UNKNOWN deriving Show

-- thank you Real World Haskell!
instance Read Method where
    readsPrec _ value = tryParse [("GET", GET)
                                 , ("HEAD", HEAD)
                                 , ("POST", POST)
                                 , ("PUT", PUT)
                                 , ("DELETE", DELETE)
                                 , ("TRACE", TRACE)
                                 , ("CONNECT", CONNECT)
                                 , ("UNKNOWN", UNKNOWN)]
    
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
                if hasMethod line then go h $ setRequestMethodURI line r
                else if ':' `elem` line then go h $ addToRequestHeader line r
                else return r
        else return r

setRequestMethodURI :: String -> Request -> Request
setRequestMethodURI line req = 
    let [m, u, p] = splitOn " " line
    in req {reqMethod = read m, reqURI = u}

addToRequestHeader :: String -> Request -> Request
addToRequestHeader line req = 
    let [k, v] = splitOn ": " line 
    in req {reqHeader = M.insert k v (reqHeader req)}

-- ok that sucks, will find cleaner way -- TODO
hasMethod :: String -> Bool
hasMethod line = take (length http11) (reverse line) == "1.1/PTTH"

requestMethod :: Request -> Method
requestMethod = reqMethod

isRequestMethod :: Method -> Request -> Bool
isRequestMethod m req = isJust $ M.lookup (show m) (reqHeader req)

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


------- response
data Resp = Resp { respStatusLine :: String
                 , respHeader :: M.Map String String
                 , respBody :: B.ByteString}
          deriving Show

type Response = TVar Resp

formatResponse :: Response -> STM B.ByteString
formatResponse r = 
    let f (x, y) = x ++ ": " ++ y ++ "\r\n" 
    in do
      resp <- readTVar r
      let header = respStatusLine resp ++ (concatMap f . M.toList $ respHeader resp)
      return $ B.concat [B.pack header, B.pack "\r\n", respBody resp]


emptyResponse :: STM Response
emptyResponse = newTVar $ Resp "" M.empty B.empty

fromStatusLine :: Int -> String -> STM Response
fromStatusLine code reason = newTVar $ Resp (http11 ++ " " ++ show code ++ " " ++ reason ++ "\r\n") M.empty B.empty

http11 :: String
http11 = "HTTP/1.1"

addToResponseHeader :: String -> String -> Response -> STM Response
addToResponseHeader k v r = do 
  modifyTVar' r (\x -> x {respHeader = M.insert k v (respHeader x)}) 
  return r

setResponseBody :: String -> Response -> STM Response
setResponseBody body r = do
  modifyTVar' r (\x -> x {respBody = B.pack body}) 
  return r

setContentType :: String -> Response -> STM Response
setContentType = addToResponseHeader "Content-Type"

setContentLength :: Response -> STM Response
setContentLength r = do 
  modifyTVar' r (\x -> x {respHeader = M.insert "Content-Length" (show $ B.length $ respBody x) $ respHeader x})
  return r

four04Page :: String -> String
four04Page url = renderHtml $ docTypeHtml $ do 
                   H.head $ 
                     H.title $ toHtml "Not Found"
                   H.body $ do
                     H.h1 $ H.toHtml "Not Found"
                     H.p $ H.toHtml $ "The URL requested at " ++ url ++ " was not found on this server."

four04 :: String -> STM Response
four04 url = do 
  r <- fromStatusLine 404 "Not Found"
  setContentType "text/html" r
  setResponseBody (four04Page url) r
  setContentLength r

respond :: Handle -> Request -> IO ()
respond handle req = case reqMethod req of
                       GET -> respondGet handle req
                       HEAD -> error "HEAD"
                       POST -> respondPost handle req
                       PUT -> respondPut handle req
                       DELETE -> error "DELETE"
                       TRACE -> error "TRACE"
                       CONNECT -> error "CONNECT"
                       UNKNOWN -> error "UNKNOWN"

answer :: Request -> IO B.ByteString
answer req = atomically $ four04 (reqURI req) >>= formatResponse

respondGet :: Handle -> Request -> IO ()
respondGet handle req = answer req >>= B.hPut handle 


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
  hClose handle -- do not close if keep alive ?

server :: IO ()
server = withSocketsDo $ do 
           socket <- listenOn (PortNumber 8002)
           forever $ do
             (handle, _, _) <- N.accept socket
             forkIO $ serve handle

main = server

