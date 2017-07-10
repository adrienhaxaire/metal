module Controllers where

import           Responses

type UserId = Text

home :: UserId -> Response
home uid = case authorized uid of
  Right uid -> Ok uid
  Left _    -> Unauthorized


