module Parser
       (
         allItems
       )where
import Text.HTML.Scalpel
import Control.Applicative
type Name = String
type Doc = String
type Type = String
type Method = String
type Methods = [Method]
type Instances = [Item]
type Source = String
type Fixity = String

data Item = Data Name Doc Source 
          | Class Name Doc Methods Instances Source
          | Func Name Type Doc Fixity Source
          deriving (Show, Read, Eq)

items :: Scraper String [Item]
items = chroots ("div" @: [hasClass "top"]) item
      
item :: Scraper String Item
item = scrapedata <|> scrapeclass

scrapedata :: Scraper String Item
scrapedata = do
    name <- text $ "a" @: [hasClass "def"]
    doc <- text $ "div" @: [hasClass "doc"]
    sourcehtml <- html $ srcSel // sourceSel
    let source = ""
    return $ Data name doc source
  where srcSel :: Selector
        srcSel = "p" @: [hasClass "src"]
        sourceSel :: Selector
        sourceSel = "a" @: [hasClass "link"]
        
scrapeSubMethods :: Scraper String Item
scrapeSubMethods = undefined

scrapeclass :: Scraper String Item
scrapeclass = do
    name <- text $ "a" @: [hasClass "def"]
    doc <- text $ "div" @: [hasClass "doc"]
    methods <- return []
    instances <- return []
    source <- html $ "a" @: [hasClass "link"]
    return $ Class name doc methods instances source



scrapefunc :: Scraper String Item
scrapefunc = undefined

allItems :: String -> IO (Maybe [Item])
allItems url = scrapeURL url items
