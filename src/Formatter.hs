module Formatter
       (
       name2doc
       )
       where

import Types

name2doc :: [Item] -> String
name2doc = concatMap step
  where step :: Item -> String
        step (Data n d s)= "data" ++ n ++ "\t" ++ formatDoc d
        step (Class n d m i s) = "class" ++ n ++ "\t" ++ formatDoc d
        step (Op n sig d f s) = n ++ sig ++ "\t" ++ formatDoc d ++ " " ++ f
        step (Func n sig d s) = n ++ sig ++ "\t" ++ formatDoc d

formatDoc :: Doc -> Doc
formatDoc = replaceNewLine . strip

strip :: Doc -> Doc
strip = lstrip . rstrip
  where
    wschars :: String
    wschars = " \t\r\n"
    lstrip :: String -> String
    lstrip s = case s of
        [] -> []
        (x:xs) -> if elem x wschars
                      then lstrip xs
                      else s
    rstrip :: String -> String
    rstrip = reverse . lstrip . reverse

replaceNewLine :: Doc -> Doc
replaceNewLine = filter ((/=) '\n')
