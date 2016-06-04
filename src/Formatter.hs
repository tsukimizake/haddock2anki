module Formatter
       (
       name2doc
       )
       where

import Types

name2doc :: [Item] -> String
name2doc = concatMap step
  where step :: Item -> String
        step (Data n d s)= "data" ++ n ++ "\t" ++ d
        step (Class n d m i s) = "class" ++ n ++ "\t" ++ d
        step (Op n sig d f s) = n ++ sig ++ "\t" ++ d ++ " " ++ f
        step (Func n sig d s) = n ++ sig ++ "\t" ++ d
