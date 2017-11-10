module Main where

import Data.Bool (bool)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.List (intercalate)
import Data.Map (Map)
import Data.Set (Set)
import Text.Megaparsec
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- $setup
-- >>> import Data.Maybe (fromJust)

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
operator = concat <$> sequence [ptoken "(", some $ oneOf ("|:<>?/=.$&*^!" :: [Char]), ptoken ")"]

-- |
-- >>> parseMaybe symbol "_Identity"
-- Just "_Identity"
-- >>> parseMaybe symbol "(<>)"
-- Just "(<>)"
symbol :: Parser String
symbol = padded $ operator <|> some (alphaNumChar <|> oneOf ("._'" :: [Char]))

-- High level elements

data ImportStmt = ImportStmt
  { importStmtQualified  :: Bool
  , importStmtModuleName :: String
  , importStmtAlias      :: Maybe String
  , importStmtImportList :: Maybe [ImportEntry]
  }
  deriving Show

data ImportEntry
  = ImportEntryGroup ImportGroup
  | ImportEntryAtom String
  deriving Show

data ImportGroup = ImportGroup
  { importGroupName :: String
  , importGroupList :: [String]
  }
  deriving Show

-- |
-- >>> parseMaybe parseImportStmt "import qualified Data.Map as Map"
-- Just (ImportStmt {importStmtQualified = True, importStmtModuleName = "Data.Map", importStmtAlias = Just "Map", importStmtImportList = Nothing})
-- >>> parseMaybe parseImportStmt "import Data.Maybe (catMaybes, fromMaybe, isJust)"
-- Just (ImportStmt {importStmtQualified = False, importStmtModuleName = "Data.Maybe", importStmtAlias = Nothing, importStmtImportList = Just [ImportEntryAtom "catMaybes",ImportEntryAtom "fromMaybe",ImportEntryAtom "isJust"]})
-- >>> parseMaybe parseImportStmt "import Data.Monoid (Monoid(mempty, mappend), (<>))"
-- Just (ImportStmt {importStmtQualified = False, importStmtModuleName = "Data.Monoid", importStmtAlias = Nothing, importStmtImportList = Just [ImportEntryGroup (ImportGroup {importGroupName = "Monoid", importGroupList = ["mempty","mappend"]}),ImportEntryAtom "(<>)"]})
-- >>> parseMaybe parseImportStmt "import Data.Monoid (Monoid(..), (<>))"
-- Just (ImportStmt {importStmtQualified = False, importStmtModuleName = "Data.Monoid", importStmtAlias = Nothing, importStmtImportList = Just [ImportEntryGroup (ImportGroup {importGroupName = "Monoid", importGroupList = [".."]}),ImportEntryAtom "(<>)"]})
-- >>> parseMaybe parseImportStmt "import OrphanInstances ()"
-- Just (ImportStmt {importStmtQualified = False, importStmtModuleName = "OrphanInstances", importStmtAlias = Nothing, importStmtImportList = Just []})
parseImportStmt :: Parser ImportStmt
parseImportStmt = ImportStmt
              <$> ( ptoken "import"
                 *> (fmap isJust . optional $ ptoken "qualified")
                  )
              <*> symbol
              <*> (optional $ ptoken "as" *> symbol)
              <*> (optional $ parseList parseImportEntry)

parseImportEntry :: Parser ImportEntry
parseImportEntry = do
  name <- symbol
  ImportEntryGroup <$> (parseImportGroup name) <|> pure (ImportEntryAtom name)

parseImportGroup :: String -> Parser ImportGroup
parseImportGroup name = ImportGroup name
                    <$> parseList symbol

-- Sorting

-- The key type is carefully chosen so `Map.toList` puts the qualified imports at the bottom,
-- and then sorts by module name.
newtype SortedImportStmts = SortedImportStmts
  { unSortedImportStmts :: Map (Bool, String, Maybe String) SortedImportList
  }
  deriving Show

-- The key type is carefully chosen so `Map.toList` puts the groups at the top, then sorts by name.
-- The @Right ()@ key is a fake group whose items are the atoms.
newtype SortedImportList = SortedImportList
  { unSortedImportList :: Map (Either String ()) (Set String)
  }
  deriving Show

instance Monoid SortedImportStmts where
  mempty = SortedImportStmts mempty
  SortedImportStmts xs `mappend` SortedImportStmts ys = SortedImportStmts (Map.unionWith (<>) xs ys)

instance Monoid SortedImportList where
  mempty = SortedImportList mempty
  SortedImportList xs `mappend` SortedImportList ys = SortedImportList (Map.unionWith (<>) xs ys)

-- "import Foo" is 'mempty', "import Foo ()" is 'instancesOnly'
instancesOnly :: SortedImportList
instancesOnly = SortedImportList $ Map.singleton (Right ()) mempty

toSortedImportStmts :: [ImportStmt] -> SortedImportStmts
toSortedImportStmts = foldMap go
  where
    go :: ImportStmt -> SortedImportStmts
    go (ImportStmt {..}) = SortedImportStmts $ Map.singleton key value
      where
        key   = (importStmtQualified, importStmtModuleName, importStmtAlias)
        value = toSortedImportList importStmtImportList

toSortedImportList :: Maybe [ImportEntry] -> SortedImportList
toSortedImportList = maybe mempty (instancesOnly <>)
                   . fmap (foldMap go)
  where
    go :: ImportEntry -> SortedImportList
    go (ImportEntryGroup (ImportGroup {..})) = SortedImportList $ Map.singleton key value
      where
        key   = Left importGroupName
        value = Set.fromList importGroupList
    go (ImportEntryAtom atom)                = SortedImportList $ Map.singleton key value
      where
        key   = Right ()
        value = Set.singleton atom

fromSortedImportStmts :: SortedImportStmts -> [ImportStmt]
fromSortedImportStmts = map (uncurry go) . Map.toList . unSortedImportStmts
  where
    go :: (Bool, String, Maybe String) -> SortedImportList -> ImportStmt
    go (qualified, moduleName, alias) sortedImportList = ImportStmt
      { importStmtQualified  = qualified
      , importStmtModuleName = moduleName
      , importStmtAlias      = alias
      , importStmtImportList = fromSortedImportList sortedImportList
      }

fromSortedImportList :: SortedImportList -> Maybe [ImportEntry]
fromSortedImportList = fmap (foldMap (uncurry go) . Map.toList) . detectEmpty . unSortedImportList
  where
    detectEmpty xs | Map.null xs = Nothing
                   | otherwise   = Just xs
    go :: Either String () -> Set String -> [ImportEntry]
    go (Left groupName) items = [ImportEntryGroup . ImportGroup groupName . Set.toList $ items]
    go (Right ())       items = map ImportEntryAtom . Set.toList $ items

sortImportStmts :: [ImportStmt] -> [ImportStmt]
sortImportStmts = fromSortedImportStmts . toSortedImportStmts

sortImportStmt :: ImportStmt -> ImportStmt
sortImportStmt importStmt@(ImportStmt {..}) = importStmt
  { importStmtImportList = sortImportList importStmtImportList
  }

sortImportList :: Maybe [ImportEntry] -> Maybe [ImportEntry]
sortImportList = fromSortedImportList . toSortedImportList

-- Rendering

tooLong :: String -> Bool
tooLong line = length line > 100

-- Either a string which fits on one line, or a list of lines if needed.
renderList :: String -> (a -> String) -> [a] -> Either String [String]
renderList indent renderItem xs0 | null xs0        = Left "()"
                                 | tooLong oneLine = Right multiLine
                                 | otherwise       = Left oneLine
  where
    oneLine = printf "(%s)" . intercalate ", " . map renderItem $ xs0
    multiLine = go (indent <> "( ") True xs0
      where
        -- 'acc' contains the line so far, the Bool is True if that's only punctuation
        go _   True  []     = [indent <> ")"]
        go acc False []     = [acc, indent <> ")"]
        go acc True  (x:xs) = go (acc <> renderItem x) False xs
        go acc False (x:xs) | tooLong acc' = acc : go (indent <> ", " <> renderedItem) False xs
                            | otherwise    = go acc' False xs
          where
            renderedItem = renderItem x
            acc' = acc <> ", " <> renderedItem

-- |
-- >>> let test = putStr . unlines . map ('|':) . lines . intercalate "\n" . map renderImportStmt . sortImportStmts . fromJust . parseMaybe (some parseImportStmt)
-- 
-- Short import lists are kept on a single line:
--
-- >>> test "import qualified Data.Map as Map"
-- |import qualified Data.Map as Map
-- >>> test "import Data.Maybe (catMaybes, fromMaybe, isJust)"
-- |import Data.Maybe (catMaybes, fromMaybe, isJust)
-- >>> test "import OrphanInstances ()"
-- |import OrphanInstances ()
-- 
-- Longer import lists are split onto multiple lines, keeping groups together:
--
-- >>> test "import FooBar (baz17, baz18, baz01, baz02, baz03, baz04, baz05, baz06, baz07, baz08, baz09, baz10, baz11, baz12, baz13, baz14, baz15, baz16, Foo(foo7, foo8, foo9, foo1, foo2, foo3, foo4, foo5, foo6), Bar(bar7, bar8, bar9, bar1, bar2, bar3, bar4, bar5, bar6), baz19, baz20)"
-- |import FooBar
-- |  ( Bar(bar1, bar2, bar3, bar4, bar5, bar6, bar7, bar8, bar9)
-- |  , Foo(foo1, foo2, foo3, foo4, foo5, foo6, foo7, foo8, foo9), baz01, baz02, baz03, baz04, baz05
-- |  , baz06, baz07, baz08, baz09, baz10, baz11, baz12, baz13, baz14, baz15, baz16, baz17, baz18, baz19
-- |  , baz20
-- |  )
-- >>> test "import FooBar (quux17, quux18, quux01, quux02, quux03, quux04, quux05, quux06, quux07, quux08, quux09, quux10, quux11, quux12, quux13, quux14, quux15, quux16, Foo(foo7, foo8, foo9, foo1, foo2, foo3, foo4, foo5, foo6), Baz(baz01, baz02, baz03, baz04, baz05, baz06, baz07, baz010, baz011, baz012, baz013, baz014, baz015, baz016, baz017, baz020), Bar(bar7, bar8, bar9, bar1, bar2, bar3, bar4, bar5, bar6), quux19, quux20)"
-- |import FooBar
-- |  ( Bar(bar1, bar2, bar3, bar4, bar5, bar6, bar7, bar8, bar9)
-- |  , Baz
-- |    ( baz01, baz010, baz011, baz012, baz013, baz014, baz015, baz016, baz017, baz02, baz020, baz03
-- |    , baz04, baz05, baz06, baz07
-- |    )
-- |  , Foo(foo1, foo2, foo3, foo4, foo5, foo6, foo7, foo8, foo9), quux01, quux02, quux03, quux04
-- |  , quux05, quux06, quux07, quux08, quux09, quux10, quux11, quux12, quux13, quux14, quux15, quux16
-- |  , quux17, quux18, quux19, quux20
-- |  )
--
-- Multiple imports are merged and sorted:
--
-- >>> test "import A (A(unA, runA))\nimport qualified Data.Map as Map\nimport A (A(unA, MkA))\nimport Data.Map (lookup)"
-- |import A (A(MkA, runA, unA))
-- |import Data.Map (lookup)
-- |import qualified Data.Map as Map
renderImportStmt :: ImportStmt -> String
renderImportStmt (ImportStmt {..}) = "import"
                                  <> bool "" " qualified" importStmtQualified
                                  <> printf " %s" importStmtModuleName
                                  <> maybe "" (printf " as %s") importStmtAlias
                                  <> maybe "" renderImportList importStmtImportList

renderImportList :: [ImportEntry] -> String
renderImportList = either (' ':) (('\n':) . intercalate "\n")
                 . renderList "  " renderImportEntry

renderImportEntry :: ImportEntry -> String
renderImportEntry (ImportEntryGroup x) = renderImportGroup x
renderImportEntry (ImportEntryAtom  x) = x

renderImportGroup :: ImportGroup -> String
renderImportGroup (ImportGroup {..}) = importGroupName
                                    <> renderGroupList importGroupList

renderGroupList :: [String] -> String
renderGroupList = either id (('\n':) . intercalate "\n")
                . renderList "    " id

main :: IO ()
main = do
  contents <- getContents
  case parse (many parseImportStmt) "" contents of
    Left e -> putStr (parseErrorPretty e)
    Right c -> putStrLn . intercalate "\n" . map renderImportStmt . sortImportStmts $ c
