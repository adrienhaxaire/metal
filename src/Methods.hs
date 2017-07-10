module Methods where

data Method = GET
            | HEAD
            | POST
            | PUT
            | DELETE
            | TRACE
            | CONNECT
            deriving Show

newtype X a = X (IO a)
  deriving (Monad)
