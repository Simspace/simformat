module Main where

import Text.Megaparsec
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import Data.List (intercalate, sort)

-- General parsing

type Parser = Parsec Dec String

comma :: Parser String
comma = string ","

padded :: Parser a -> Parser a
padded = between space space

commaSep :: Parser a -> Parser [a]
commaSep a = sepBy (padded a) (padded comma)

parens :: Parser a -> Parser a
parens = between (ptoken "(") (ptoken ")")

ptoken :: String -> Parser String
ptoken = padded . string

-- Basic elements

-- |
-- >>> parseMaybe (parseList symbol) "( hi, bye )"
-- Just ["hi","bye"]
parseList :: Parser a -> Parser [a]
parseList p = parens $ commaSep p

-- |
-- >>> parseMaybe operator "(<$>)"
-- Just "(<$>)"
operator :: Parser String
operator = concat <$> sequence [ptoken "(", many $ oneOf ("|:<>?/=.$&*^!" :: [Char]), ptoken ")"]

-- |
-- >>> parseMaybe symbol "_Identity"
-- Just "_Identity"
-- >>> parseMaybe symbol "(<>)"
-- Just "(<>)"
symbol :: Parser String
symbol = padded $ operator <|> many (alphaNumChar <|> oneOf ("._'" :: [Char]))

-- High level elements

type Import = (String, Maybe [String])

importHeader :: Parser String
importHeader = do
  importKeyword <- ptoken "import"
  qualKeyword <- optional $ ptoken "qualified"
  name <- symbol
  as <- maybe [] (map Just) <$> (optional $ sequence [ptoken "as", symbol])

  pure . intercalate " " . catMaybes $ [Just importKeyword, qualKeyword, Just name] <> as

parseQualifier :: Parser (Maybe [Import])
parseQualifier = optional $ parseList parseItem

parseItem :: Parser Import
parseItem = (,) <$> symbol <*> optional (parseList symbol)

parseImport :: Parser (String, Maybe [Import])
parseImport = (,) <$> importHeader <*> parseQualifier

-- Rendering

render :: (String, Maybe [Import]) -> String
render (name, imports) = name <> rimports
  where
    renderImport (str, subs) =
      let extra = case fromMaybe [] subs of
            [] -> []
            as -> "(" <> (intercalate ", " $ sort as) <> ")"
      in str <> extra
    rimports = case imports of
      Just is ->
        let items = zipWith (<>) ("": repeat ", ") (sort $ map renderImport is)
            groups = groupLines items
        in case groups of
            [x] -> " (" <> x <> ")"
            (x:xs) -> concatMap ("\n  " <>) (("( " <> x : xs) <> [")"])
            [] -> "()"
      Nothing -> ""

groupLines :: [String] -> [String]
groupLines ls = go [] ls
  where
    go [] (x:xs) = go x xs
    go cur [] = [cur]
    go cur (x:xs) | length (cur <> x) > 100 = cur : go [] (x:xs)
    go cur (x:xs) = go (cur <> x) xs

main :: IO ()
main = do
  contents <- getContents
  case parse (many parseImport) "" contents of
    Left e -> putStr (parseErrorPretty e)
    Right c -> putStrLn . intercalate "\n" . map render $ c
