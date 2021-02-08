{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Description: Configuration for formatting program. -}
module SimSpace.Config where


import Control.Exception (SomeException, try)
import Data.List (inits, isPrefixOf, isSuffixOf)
import Data.Set (Set)
import Data.Traversable (for)
import Data.Yaml
  ( (.!=), (.:?), (.=), FromJSON, ToJSON, decodeFileEither, object, parseJSON, toJSON, withObject
  )
import System.Directory (doesDirectoryExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>))
import System.Process (readCreateProcess, shell)
import Turtle (commonPrefix, decodeString, encodeString, isDirectory, splitDirectories, stat)
import qualified Data.Set as Set


{- | A newtype wrapper for files or directories we want to format. -}
newtype FormatFiles = FormatFiles { unFormatFiles :: [FilePath] }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)


{- | A newtype wrapper for files or directories we want to exclude that are otherwise included by 'FormatFiles'
(typically generated files). -}
newtype WhitelistFiles = WhitelistFiles { unWhitelistFiles :: [FilePath] }
  deriving (Eq, Ord, Show, ToJSON, FromJSON)


isValid :: [FilePath] -> [FilePath] -> FilePath -> Bool
isValid files whitelist fp =
  let matchesFiles = elem "." files || any (flip isPrefixOf fp) files
      matchesWhitelist = elem "." whitelist || any (flip isPrefixOf fp) whitelist
  in isSuffixOf ".hs" fp && matchesFiles && not matchesWhitelist


{- | Filter files in a git repository by considering the files we do want to format. Normalize the raw files. -}
filterFiles :: FormatFiles -> WhitelistFiles -> Bool -> [FilePath] -> IO [FilePath]
filterFiles (FormatFiles files) (WhitelistFiles whitelist) allFiles fileList = do
  hasGit <- doesDirectoryExist ".git"
  let srcFiles = case (null fileList, not hasGit || allFiles) of
        (True, True) -> Set.toList . mconcat <$> traverse listFilesRecursive ["."]
        (True, False) -> lines <$> readCreateProcess (shell "git ls-files") ""
        (False, _) -> fmap (Set.toList . mconcat) . for fileList $ \ file -> do
          (isDirectory <$> stat (decodeString file)) >>= \ case
            False -> pure $ Set.singleton file
            True -> mconcat <$> traverse listFilesRecursive [file]
  filter (isValid files whitelist) <$> srcFiles

listFilesRecursive :: FilePath -> IO (Set FilePath)
listFilesRecursive dir = do
  dirs <- listDirectory dir
  fmap mconcat . for dirs $ \case
    -- don't include "hidden" directories, i.e. those that start with a '.'
    '.' : _ -> pure Set.empty
    fn -> do
      let
        path = if dir == "." then fn else dir </> fn
      isDir <- doesDirectoryExist path
      isSymlink <- pathIsSymbolicLink path
      case (isSymlink, isDir) of
        (True, _) -> pure mempty
        (_, True) -> listFilesRecursive path
        _ -> pure $ Set.singleton path

{- | Configuration type, to be formatted from a file called `.simformatrc` typically found in the same directory as
where the script is running. -}
data Config = Config
  { configFiles :: FormatFiles
  {- ^ The list of files or directories to consider. -}
  , configWhitelist :: WhitelistFiles
  {- ^ The list of files or directories to exclude that are otherwise included by 'configFiles'. -}
  }
  deriving (Eq, Show)


normalizeConfig :: FilePath -> Config -> Config
normalizeConfig prefix config = Config
  { configFiles = FormatFiles $ (prefix </>) <$> unFormatFiles (configFiles config)
  , configWhitelist = WhitelistFiles $ (prefix </>) <$> unWhitelistFiles (configWhitelist config)
  }


findConfig :: [FilePath] -> IO Config
findConfig files = do
  let dir = commonPrefix $ decodeString <$> files
      possibleDirs = reverse . inits . splitDirectories $ dir
      go = \ case
        [] -> pure emptyConfig
        x:xs -> do
          let thisDir = mconcat (encodeString <$> x)
          try (decodeFileEither $ thisDir </> ".simformatrc") >>= \ case
            Left (_ :: SomeException) -> go xs
            Right (Left _) -> go xs
            Right (Right config) -> pure $ normalizeConfig thisDir config
  go possibleDirs


{- | A default, empty config in case `.simformatrc` is not provided or not found. -}
emptyConfig :: Config
emptyConfig = Config
  { configFiles = FormatFiles ["."]
  , configWhitelist = WhitelistFiles []
  }


instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    Config
      <$> obj .:? "files" .!= configFiles emptyConfig
      <*> obj .:? "whitelist" .!= configWhitelist emptyConfig


instance ToJSON Config where
  toJSON Config {..} = object
    [ "files" .= configFiles
    , "whitelist" .= configWhitelist
    ]
