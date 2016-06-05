module Downloader (download) where
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Control.Applicative
import Data.ByteString.Lazy.Char8

download :: String -> IO String
download s = return . unpack . getResponseBody =<< httpLBS =<< parseUrl s
