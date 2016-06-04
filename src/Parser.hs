module Parser
       (
         allItems
       )where
import Text.HTML.Scalpel
import Control.Applicative
import Types

items :: Scraper String [Item]
items = chroots ("div" @: [hasClass "top"]) item
      
item :: Scraper String Item
item = scrapeOp <|> scrapeFunc <|> scrapeClass <|> scrapeData

getDef = text $ "a" @: [hasClass "def"]
getDoc = liftA strip $ text $ "div" @: [hasClass "doc"]
  where
    strip :: String -> String
    strip = lstrip . rstrip
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


getSource = attr "href" $ "a" @: [hasClass "link"]

scrapeData :: Scraper String Item
scrapeData = do
    name <- getDef
    doc <- getDoc
    source <- getSource
    return $ Data name doc source


scrapeSubMethods :: Scraper String [Item]
scrapeSubMethods = undefined

scrapeInstances :: Scraper String [Item]
scrapeInstances = undefined

scrapeClass :: Scraper String Item
scrapeClass = do
    name <- getDef
    doc <- getDoc
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
    name <- getDef
    sig <- text $ "p" @: [hasClass "src"]
    doc <- getDoc
    source <- getSource
    return $ Func name sig doc source

allItems :: String -> IO (Maybe [Item])
allItems url = scrapeURL url items
