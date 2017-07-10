module Models where

type UserId = Text

authorized :: UserId -> Either String UserId
authorized uid = Right uid
           
