module Parser (parseWindowData) where

import Types
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.Maybe (fromMaybe)

type Parser = Parsec Void String

sc :: Parser ()
sc = L.space space1 (L.skipLineComment "//") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

pSize :: Parser Size
pSize = do
  n <- lexeme L.decimal
  (Pct (fromIntegral n / 100) <$ symbol "%") <|> 
    (Px n <$ symbol "px") <|> 
    return (Px n)

pPropMap :: Parser [(String, String, Size)]
pPropMap = between (symbol "{") (symbol "}") (pProp `sepBy` symbol ",")
  where
    pProp = do
      key <- lexeme (some letterChar)
      _ <- symbol ":"
      if key == "color"
        then do
          -- Accept letters, numbers, and '#' for hex codes
          col <- lexeme (some (alphaNumChar <|> char '#'))
          return (key, col, Px 0) 
        else do
          sz <- pSize
          return (key, "", sz)

extractProps :: Direction -> [(String, String, Size)] -> Props
extractProps d props = Props
  { width  = fromMaybe (Pct 1.0) (lookupSize "width" props)
  , height = fromMaybe (Pct 1.0) (lookupSize "height" props)
  , dir    = d
  , color  = lookup "color" [(k, v) | (k, v, _) <- props]
  }
  where lookupSize k ps = lookup k [(k', s) | (k', _, s) <- ps]

pLayout :: Parser Layout
pLayout = pRow <|> pCol <|> pBox
  where
    pRow = do
      _ <- symbol "row"
      props <- optional (try pPropMap)
      children <- between (symbol "{") (symbol "}") (many pLayout)
      return $ Box (extractProps Row (fromMaybe [] props)) children
    
    pCol = do
      _ <- symbol "col"
      props <- optional (try pPropMap)
      children <- between (symbol "{") (symbol "}") (many pLayout)
      return $ Box (extractProps Col (fromMaybe [] props)) children
      
    pBox = do
      _ <- symbol "box"
      props <- pPropMap
      return $ Box (extractProps Row props) []

pWindow :: Parser Window
pWindow = do
  _ <- symbol "window"
  name <- lexeme (char '"' >> manyTill anySingle (char '"'))
  w <- lexeme L.decimal
  _ <- symbol "x"
  h <- lexeme L.decimal
  layout <- between (symbol "{") (symbol "}") pLayout
  return $ Window name w h layout

parseWindowData :: String -> String -> Either (ParseErrorBundle String Void) Window
parseWindowData = parse pWindow