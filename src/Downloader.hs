module Downloader (download) where
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Control.Applicative
import Data.ByteString.Lazy

download :: String -> IO ByteString
download s = return . getResponseBody =<< httpLBS =<< parseUrl s
