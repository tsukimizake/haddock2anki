module Scraper
       (
         items
       )where
import Text.HTML.Scalpel
import Control.Applicative
import Types

items :: Scraper String [Item]
items = chroots ("div" @: [hasClass "top"]) item

item :: Scraper String Item
item = scrapeOp <|> scrapeClass <|> scrapeFunc <|> scrapeData

getDef = text $ "a" @: [hasClass "def"]
getDoc = text $ "div" @: [hasClass "doc"]
getSource = attr "href" $ "a" @: [hasClass "link"]

scrapeData :: Scraper String Item
scrapeData = do
    name <- getDef
    doc <- getDoc
    source <- getSource
    return $ Data name doc source


scrapeSubMethods :: Scraper String [String]
scrapeSubMethods = texts $ "div" @: [hasClass "subs", hasClass "methods"]

scrapeInstances :: Scraper String [String]
scrapeInstances = undefined

scrapeClass :: Scraper String Item
scrapeClass = do
    text $ "span" @: [hasClass "keyword"]
    name <- getDef
    doc <- getDoc
    methods <- scrapeSubMethods
    instances <- return []
    methods <- return []
    source <- getSource
    return $ Class name doc methods instances source

scrapeOp :: Scraper String Item
scrapeOp = do
    name <- getDef
    sig <- text $ "p" @: [hasClass "src"]
    doc <- getDoc
    fixity <- text $ "span" @: [hasClass "fixity"]
    source <- getSource
    return $ Op name sig doc fixity source

scrapeFunc :: Scraper String Item
scrapeFunc = do
    name <- text $ "a" @: [hasClass "def", match beginsWithV]
    sig <- text $ "p" @: [hasClass "src"]
    doc <- getDoc
    source <- getSource
    return $ Func name sig doc source
  where
    beginsWithV :: String -> String -> Bool
    beginsWithV attr val = (attr == "name") && (take 2 val == "v:")
allItems :: String -> IO (Maybe [Item])
allItems url = scrapeURL url items
