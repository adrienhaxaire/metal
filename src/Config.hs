module Config where

import Network.Socket

data Config = Config {port :: PortNumber}

defaultPort :: PortNumber
defaultPort = 8140

defaultConfig :: Config
defaultConfig = Config defaultPort


