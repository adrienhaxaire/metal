module Responses where

import           Headers
import           StatusCodes

data Response a = Response StatusCode Headers (Body a)

