{-# OPTIONS -Wno-unused-matches -Wno-unused-local-binds -Wno-unused-top-binds -Wno-unused-imports #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

module SimSpace.SimFormat (reformat) where

import Control.Monad (MonadFail)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Foldable (foldlM)
import Data.Function (on)
import Data.List (groupBy, intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub, sort)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Semigroup ((<>), Semigroup)
import Data.Set (Set)
import Data.Void (Void)
import Debug.Trace
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (readProcessWithExitCode)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Printf (printf)
import qualified Data.Map as Map
import qualified Data.Set as Set

type Parser = Parsec Void String

-- Sorting

-- The key type is carefully chosen so `Map.toList` orders the imports in the
-- desired order.
newtype SortedImportStmts = SortedImportStmts
  { unSortedImportStmts :: Map ImportStmtHead SortedImportList
  }
  deriving (Eq, Ord, Show)
  deriving newtype (Semigroup)

-- High level elements

data ImportStmtHead = ImportStmtHead
  { importStmtHeadCommented   :: Bool
  , importStmtHeadQualified   :: Bool
  , importStmtHeadPackageName :: Maybe String
  , importStmtHeadModuleName  :: String
  , importStmtHeadAlias       :: Maybe String
  } deriving (Eq, Show)

-- This order of the tuple represents the order of the fields used when sorting.
-- That means package imports are placed at the top, then unqualified imports
-- which are sorted by module name, alias and commented (uncommented first).
-- Finally, the qualified imports are placed using the same order as unqualified
-- imports.
instance Ord ImportStmtHead where
  ImportStmtHead c1 q1 p1 m1 a1 `compare` ImportStmtHead c2 q2 p2 m2 a2
    = (p1, q1, m1, a1, c1) `compare` (p2, q2, m2, a2, c2)

data ImportStmt = ImportStmt
  { importStmtHead        :: ImportStmtHead
  , importStmtImportList  :: SortedImportList
  } deriving (Eq, Ord, Show)

setDiff :: Ord a => Set a -> Set a -> Maybe (Set a)
setDiff x y = Just $ Set.difference x y

-- Order chosen here so that sorting them will put the hiding clauses
-- first, then partial imports, then open imports.
data SortedImportList
  = HidingImport  (Map GroupKeyType (Set String))
  | PartialImport (Map GroupKeyType (Set String))
  | OpenImport
  deriving (Eq, Ord, Show)

instance Semigroup SortedImportList where
  OpenImport      <> _               = OpenImport
  _               <> OpenImport      = OpenImport
  HidingImport  a <> HidingImport b  = HidingImport  (Map.intersectionWith Set.intersection a b)
  PartialImport a <> PartialImport b = PartialImport (Map.unionWith Set.union a b)
  HidingImport  a <> PartialImport b = HidingImport  (Map.differenceWith setDiff a b)
  PartialImport b <> HidingImport a  = HidingImport  (Map.differenceWith setDiff a b)

data GroupKeyType
  = ConstructorTypeKey String
  | NoGroupTypeKey
  | ConstructorKey String
  | NoGroupKey
  | NoGroupPatternKey
  deriving (Show,Ord,Eq)

data ImportEntry
  = ImportEntryTypeGroup ImportGroup
  | ImportEntryTypeAtom String
  | ImportEntryGroup ImportGroup
  | ImportEntryAtom String
  | ImportEntryPatternAtom String
  deriving (Show,Ord,Eq)

-- | ImportGroup: something like (Monoid(mempty,mappend)) in an import
--   line.
data ImportGroup = ImportGroup
  { _importGroupName :: String
  , _importGroupList :: Set String
  }
  deriving (Show,Ord,Eq)

-- $setup
-- >>> import Data.Maybe (fromJust)

-- General parsing

comma :: Parser String
comma = string ","

padded :: Parser a -> Parser a
padded = between space space

commaSep :: Parser a -> Parser [a]
commaSep a = sepEndBy (padded a) (padded comma)

parens :: Parser a -> Parser a
parens = between (ptoken "(") (ptoken ")")

quoted :: Parser a -> Parser a
quoted = between (ptoken "\"") (ptoken "\"")

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
-- >>> parseMaybe operator "(∈)"
-- Just "(\8712)"
operator :: Parser String
operator = concat <$> sequence [ptoken "(", symbolChars, ptoken ")"]

-- See https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4 (ascSymbol) for allowed characters in operators.
-- Also supports unicode symbols.
symbolChars :: Parser String
symbolChars = some (oneOf ("!#$%&*+./<=>?@^|-~:\\" :: String)) <|> some symbolChar

-- |
-- >>> parseMaybe symbol "_Identity"
-- Just "_Identity"
-- >>> parseMaybe symbol "(<>)"
-- Just "(<>)"
symbol :: Parser String
symbol = padded $ operator <|> some (alphaNumChar <|> oneOf ("._'#" :: String))

-- |
-- >>> parseMaybe packageName "simformat"
-- Just "simformat"
-- >>> parseMaybe packageName "servant-client-core"
-- Just "servant-client-core"
-- >>> parseMaybe packageName "foo_bar"
-- Nothing
packageName :: Parser String
packageName = some (alphaNumChar <|> char '-')

parseImportBlock :: Parser SortedImportStmts
parseImportBlock = toSortedImportStmts <$> some parseImportStmt

-- |
-- >>> parseMaybe parseImportStmt "import qualified Data.Map as Map"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = True, importStmtHeadPackageName = Nothing, importStmtHeadModuleName = "Data.Map", importStmtHeadAlias = Just "Map"}, importStmtImportList = OpenImport})
-- >>> parseMaybe parseImportStmt "import Data.Maybe (catMaybes, fromMaybe, isJust)"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = False, importStmtHeadPackageName = Nothing, importStmtHeadModuleName = "Data.Maybe", importStmtHeadAlias = Nothing}, importStmtImportList = PartialImport (fromList [(NoGroupKey,fromList ["catMaybes","fromMaybe","isJust"])])})
-- >>> parseMaybe parseImportStmt "import Data.Monoid (Monoid(mempty, mappend), (<>))"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = False, importStmtHeadPackageName = Nothing, importStmtHeadModuleName = "Data.Monoid", importStmtHeadAlias = Nothing}, importStmtImportList = PartialImport (fromList [(ConstructorKey "Monoid",fromList ["mappend","mempty"]),(NoGroupKey,fromList ["(<>)"])])})
-- >>> parseMaybe parseImportStmt "import Data.Monoid (Monoid(..), (<>))"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = False, importStmtHeadPackageName = Nothing, importStmtHeadModuleName = "Data.Monoid", importStmtHeadAlias = Nothing}, importStmtImportList = PartialImport (fromList [(ConstructorKey "Monoid",fromList [".."]),(NoGroupKey,fromList ["(<>)"])])})
-- >>> parseMaybe parseImportStmt "import OrphanInstances ()"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = False, importStmtHeadPackageName = Nothing, importStmtHeadModuleName = "OrphanInstances", importStmtHeadAlias = Nothing}, importStmtImportList = PartialImport (fromList [])})
-- >>> parseMaybe parseImportStmt "import Foo hiding (Bar, (+))"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = False, importStmtHeadPackageName = Nothing, importStmtHeadModuleName = "Foo", importStmtHeadAlias = Nothing}, importStmtImportList = HidingImport (fromList [(NoGroupKey,fromList ["(+)","Bar"])])})
-- >>> parseMaybe parseImportStmt "import \"foo\" Foo"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = False, importStmtHeadPackageName = Just "foo", importStmtHeadModuleName = "Foo", importStmtHeadAlias = Nothing}, importStmtImportList = OpenImport})
-- >>> parseMaybe parseImportStmt "import qualified \"foo\" Foo"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = False, importStmtHeadQualified = True, importStmtHeadPackageName = Just "foo", importStmtHeadModuleName = "Foo", importStmtHeadAlias = Nothing}, importStmtImportList = OpenImport})
-- >>> parseMaybe parseImportStmt "-- import qualified \"foo\" Foo"
-- Just (ImportStmt {importStmtHead = ImportStmtHead {importStmtHeadCommented = True, importStmtHeadQualified = True, importStmtHeadPackageName = Just "foo", importStmtHeadModuleName = "Foo", importStmtHeadAlias = Nothing}, importStmtImportList = OpenImport})
parseImportStmt :: Parser ImportStmt
parseImportStmt = do
  importStmtHead <- ImportStmtHead
                <$> (fmap isJust . optional $ ptoken "--")
                <*> (ptoken "import" *> (fmap isJust . optional $ ptoken "qualified"))
                <*> (optional . padded $ quoted packageName)
                <*> symbol
                <*> optional (ptoken "as" *> symbol)
  importStmtImportList <- mkSortedImportList =<<
                      (,) <$> optional (parseList parseImportEntry)
                          <*> optional (string "hiding" >> parseList parseImportEntry)
  pure ImportStmt {..}
  where
    mkSortedImportList :: (Maybe [ImportEntry], Maybe [ImportEntry]) -> Parser SortedImportList
    mkSortedImportList (Just x, Nothing) = pure $ PartialImport $ buildImportList x
    mkSortedImportList (Nothing,Just x)  = pure $ HidingImport $ buildImportList  x
    mkSortedImportList (Nothing,Nothing) = pure OpenImport
    mkSortedImportList (Just _, Just _) = fail "can't specify both a hiding clause and an import clause"

    buildImportList :: [ImportEntry] -> Map.Map GroupKeyType (Set String)
    buildImportList = removeDups . Map.fromListWith Set.union . map extractKey

    -- "import Foo (Bar(MkBar), Bar)" imports "Bar" twice, remove the second
    removeDups :: Map GroupKeyType (Set String) -> Map GroupKeyType (Set String)
    removeDups kss = Map.adjust (Set.\\ groupKeys) NoGroupKey kss
      where
        groupKeys :: Set String
        groupKeys = Set.fromList $ concatMap onlyGroupKeys $ Map.keys kss

        onlyGroupKeys :: GroupKeyType -> [String]
        onlyGroupKeys (ConstructorKey k) = [k]
        onlyGroupKeys _                  = []

    extractKey :: ImportEntry -> (GroupKeyType, Set String)
    extractKey (ImportEntryTypeGroup (ImportGroup k s)) = (ConstructorTypeKey k, s)
    extractKey (ImportEntryGroup (ImportGroup k s))     = (ConstructorKey k, s)
    extractKey (ImportEntryTypeAtom s)                  = (NoGroupTypeKey, Set.singleton s)
    extractKey (ImportEntryAtom s)                      = (NoGroupKey, Set.singleton s)
    extractKey (ImportEntryPatternAtom s)               = (NoGroupPatternKey, Set.singleton s)

parseImportEntry :: Parser ImportEntry
parseImportEntry = do
  name <- symbol
  case name of
    "type" -> do
      name' <- symbol
      ImportEntryTypeGroup <$> parseImportGroup name' <|> pure (ImportEntryTypeAtom name')
    "pattern" -> ImportEntryPatternAtom <$> symbol
    _ -> ImportEntryGroup <$> parseImportGroup name <|> pure (ImportEntryAtom name)

parseImportGroup :: String -> Parser ImportGroup
parseImportGroup name = ImportGroup name . Set.fromList
                    <$> parseList symbol

toSortedImportStmts :: [ImportStmt] -> SortedImportStmts
toSortedImportStmts = SortedImportStmts . fmap simplify . Map.fromListWith (<>) . map extractKey
  where
    extractKey ImportStmt{..} = (importStmtHead, importStmtImportList)

    simplify :: SortedImportList -> SortedImportList
    simplify (HidingImport x)
      -- an empty hiding list is just an open import
      | all Set.null (Map.elems x) = OpenImport
      | otherwise = HidingImport x
    simplify x = x

fromSortedImportStmts :: SortedImportStmts -> [ImportStmt]
fromSortedImportStmts = map (uncurry go) . Map.toList . unSortedImportStmts
  where
    go :: ImportStmtHead -> SortedImportList -> ImportStmt
    go importStmtHead importStmtImportList = ImportStmt {..}

-- Rendering

-- Either a string which fits on one line, or a list of lines if needed.
renderList :: Int -> String -> (a -> String) -> [a] -> Either String [String]
renderList prefixLen indent renderItem xs0 | null xs0        = Left "()"
                                           | tooLongSingleLine oneLine = Right multiLine
                                           | otherwise       = Left oneLine
  where
    tooLongSingleLine :: String -> Bool
    tooLongSingleLine line = length line + prefixLen > 100

    tooLong :: String -> Bool
    tooLong line = length line > 100

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
-- >>> let test = putStr . unlines . map ('|':) . lines . intercalate "\n" . map renderImportStmt . fromSortedImportStmts . fromJust . parseMaybe parseImportBlock
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
--
-- Empty hiding lists should be elided
-- >>> test "import Foo hiding (Bar)\nimport Baz\nimport Foo hiding (quux)"
-- |import Baz
-- |import Foo
--
-- Hiding wins over imports
-- >>> test "import Foo hiding (Bar)\nimport Foo(baz)\n"
-- |import Foo hiding (Bar)
--
-- Hiding imports go first, then normal imports, then qualified
-- >>> test "import qualified Foo\nimport Bar hiding(baz)\nimport Quux"
-- |import Bar hiding (baz)
-- |import Quux
-- |import qualified Foo

renderImportStmt :: ImportStmt -> String
renderImportStmt ImportStmt {importStmtHead = ImportStmtHead {..}, importStmtImportList = importStmtImportList} =
  let everythingButImports = (if importStmtHeadCommented then "-- " else "")
                          <> "import"
                          <> bool "" " qualified" importStmtHeadQualified
                          <> maybe "" (printf " \"%s\"") importStmtHeadPackageName
                          <> printf " %s" importStmtHeadModuleName
                          <> maybe "" (printf " as %s") importStmtHeadAlias

  in everythingButImports <> renderImportList (length everythingButImports) importStmtImportList

renderImportStmts :: SortedImportStmts -> String
renderImportStmts = unlines . map renderImportStmt . fromSortedImportStmts

renderImportList :: Int -> SortedImportList -> String
renderImportList prefixLen = \case
  OpenImport      -> ""
  HidingImport h  -> " hiding" <> renderImportEntries (prefixLen + 7) h -- " hiding" is 7 chars
  PartialImport i -> renderImportEntries prefixLen i

renderImportEntries :: Int -> Map.Map GroupKeyType (Set String) -> String
renderImportEntries prefixLen = either (' ':) (('\n':) . intercalate "\n")
                              . renderList prefixLen "  " id
                              . concatMap chunkImportEntry
                              . Map.toAscList

chunkImportEntry :: (GroupKeyType, Set String) -> [String]
chunkImportEntry (NoGroupKey, s) = Set.toAscList s
chunkImportEntry (NoGroupTypeKey, s) = ("type " <>) <$> Set.toAscList s
chunkImportEntry (ConstructorKey importGroupName, importGroupList) =
  [importGroupName <> renderGroupList importGroupList]
chunkImportEntry (ConstructorTypeKey importGroupName, importGroupList) =
  ["type " <> importGroupName <> renderGroupList importGroupList]
chunkImportEntry (NoGroupPatternKey, s) = mappend "pattern " <$> Set.toAscList s

renderGroupList :: Set String -> String
renderGroupList = either id (('\n':) . intercalate "\n")
                . renderList 0 "    " id . Set.toAscList

-- https://stackoverflow.com/a/4981265/1313611
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =
  case dropWhile p s of
    "" -> []
    s' ->
      let (w, s'') = break p s'
       in w : wordsWhen p s''

type BlankLine = String
type Block = Either BlankLine String
type Line = String

reformat
  :: (MonadFail m, MonadIO m)
  => Bool {- ^ Whether to re-group imports. -}
  -> [Line]
  -> m [Line]
reformat regroup programLines = do
  let
    (nonimports, importsAndAfter) = break ("import " `isPrefixOf`) programLines
  concatMap blockToLines
    <$> reassemble
          (sortLanguagePragmas nonimports)
          (untilNothing processBlock (chunkedInputs importsAndAfter))
  where
    sortLanguagePragmas :: [String] -> [String]
    sortLanguagePragmas =
      foldMap go . groupBy (on (==) isLanguagePragma)
      where
      go xs =
        if not $ isLanguagePragma $ head xs then
          xs
        else
          fmap (\x -> "{-# LANGUAGE " <> x <> " #-}")
            $ sort $ nub $ xs >>= getLanguages

      isLanguagePragma x = "{-# LANGUAGE " `isPrefixOf` x && "#-}" `isSuffixOf` x

      getLanguages :: String -> [String]
      getLanguages x =
        if not (isLanguagePragma x) then
          error $ "Input was not a language pragma: " <> show x
        else
          wordsWhen (`elem` (", " :: String))
            $ drop (length ("{-# LANGUAGE" :: String))
            $ take (length x - length ("#-}" :: String)) x

    blockToLines :: Block -> [String]
    blockToLines (Left s) = lines (s <> "\n")
    blockToLines (Right s) = lines s

    detectBlankLine :: String -> Either BlankLine String
    detectBlankLine s | all isSpace s = Left s
                      | otherwise     = Right s

    -- we want to process each chunk of imports as its own little block
    chunkedInputs :: [String] -> [Block]
    chunkedInputs = (fmap.fmap) unlines . splitOn detectBlankLine

    processBlock :: Block -> Maybe (Either BlankLine SortedImportStmts)
    processBlock (Left  s) = pure (Left s)
    processBlock (Right s) = Right <$> parseMaybe parseImportBlock s

    reassemble
      :: (MonadFail m, MonadIO m)
      => [String]
      -> ([Either BlankLine SortedImportStmts], [Block])
      -> m [Block]
    reassemble nonImports (chunkedImports, leftovers) = do
        rechunked <- (if regroup then rechunk else pure) chunkedImports
        pure $
          fmap Left nonImports
          <> (fmap . fmap) renderImportStmts rechunked
          <> leftovers
      where
        rechunk
          :: (MonadFail m, MonadIO m)
          => [Either BlankLine SortedImportStmts]
          -> m [Either BlankLine SortedImportStmts]
        rechunk ci = do
          (preludes, locals, others) <-
            foldlM
              insertCatagorized
              ([], [], [])
              (foldMap fromSortedImportStmts [ stmts | Right stmts <- ci ])
          let ifNotEmpty p = if null p then [] else [Right $ toSortedImportStmts p, Left ""]
          pure $ ifNotEmpty preludes
              <> ifNotEmpty others
              <> ifNotEmpty locals

        insertCatagorized
          :: (MonadFail m, MonadIO m)
          => ([ImportStmt], [ImportStmt], [ImportStmt])
          -> ImportStmt
          -> m ([ImportStmt], [ImportStmt], [ImportStmt])
        insertCatagorized (preludes, locals, others) stmt =
          categorize (importStmtHead stmt) >>= \case
            Prelude -> pure (preludes <> [stmt], locals, others)
            Local -> pure (preludes, locals <> [stmt], others)
            Other -> pure (preludes, locals, others <> [stmt])

        categorize :: (MonadFail m, MonadIO m) => ImportStmtHead -> m ImportCategory
        categorize stmt
          | "Prelude" `isInfixOf` importStmtHeadModuleName stmt =
              pure Prelude
          | otherwise =
              liftIO (
                readProcessWithExitCode
                  "bash"
                  [
                    "-c",
                    "stack exec ghc-pkg -- find-module "
                    <> importStmtHeadModuleName stmt
                    <> " | grep -v 'pkgdb$' | grep -v 'package.conf.d$' | grep -qv '(no packages)'"
                  ]
                  ""
              ) >>= \case
                (ExitSuccess, _, _) ->
                  {- The grep statement found a package.  -}
                  pure Other
                (ExitFailure 1, _, _) ->
                  {-
                    grep exited with a exit code 1, meaning no packages were
                    found, meaning this is an internal module.
                  -}
                  pure Local
                procFailed -> fail (show procFailed)


data ImportCategory
  = Prelude
  | Local
  | Other

-- |divide the input into blocks while preserving the number of separators
splitOn :: (a -> Either separator b) -> [a] -> [Either separator [b]]
splitOn _ [] = []
splitOn f (x:xs) = case f x of
  Left separator -> Left separator : splitOn f xs
  Right y        -> case splitOn f xs of
    Right ys : zs -> Right (y:ys) : zs
    zs            -> Right [y] : zs

untilNothing :: (a -> Maybe b) -> [a] -> ([b], [a])
untilNothing f = go []
  where
    go ys [] = (reverse ys, [])
    go ys (x:xs) =
      case f x of
        Nothing -> (reverse ys, x:xs)
        Just y  -> go (y:ys) xs
