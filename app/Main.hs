module Main where

import Scraper
import Formatter
import System.Environment (getArgs)
import Data.Maybe
import Text.HTML.Scalpel

assert :: IO()
assert = putStrLn "pass url as an arg!"

run :: String -> String -> IO()
run outputpath uri = do
    maybeitems <- scrapeURL uri items
    items <- return $ fromMaybe [] maybeitems
    res <- return . name2doc $ items
    writeFile outputpath res

helpMsg :: String
helpMsg = "NAME\n     haddock2anki -- haddock scraper to generate anki flashcards\n\nSYNOPSIS\n     haddock2anki [-ho] URL\n\nDESCRIPTION\n     Names and natures do often agree. It will do what you may want.\n\nOPTIONS\n     -h show this help.\n\n     -o specify output filename. Default is \"./out.txt\".\n"

getOutFile :: [String] -> Maybe String
getOutFile args = let idx = findIndex (== "-o") args
                  in maybe Nothing (return . (args !!) . (+1)) idx


main :: IO()
main = do
     args <- getArgs
     when (elem "-h" args)
         (putStrLn helpMsg)

     let outfile = fromMaybe "./out.txt" $ getOutFile args

     if length args == 0
         then assert
         else run outfile (last args)
