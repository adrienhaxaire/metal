module Server where

import Control.Monad
import Network.Socket
import System.IO
import Control.Concurrent

import Config

serve :: Handle -> IO ()
serve = undefined

run :: Config -> IO ()
run config = withSocketsDo $ do
  socket <- listen (port config)
  forever $ do
    (handle, _, _) <- accept socket
    forkIO $ serve handle



