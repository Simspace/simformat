{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Description: Configuration for formatting program. -}
module SimSpace.Config (
  Config(..), emptyConfig,
  FormatFiles(..), filterFiles,
) where


import Control.Monad ((>=>))
import Data.List (isPrefixOf, isSuffixOf)
import Data.Yaml ((.!=), (.:?), FromJSON, parseJSON, withObject)
import System.Directory (makeAbsolute, makeRelativeToCurrentDirectory)
import System.Process (readCreateProcess, shell)


{- | A newtype wrapper for files or directories we want to format. -}
newtype FormatFiles = FormatFiles { unFormatFiles :: [FilePath] }
  deriving (Eq, Ord, Show, FromJSON)


{- | A newtype wrapper for files or directories we want to exclude that are otherwise included by 'FormatFiles'
(typically generated files). -}
newtype WhitelistFiles = WhitelistFiles { unWhitelistFiles :: [FilePath] }
  deriving (Eq, Ord, Show, FromJSON)


{- | Filter files in a git repository by considering the files we do want to format. Normalize the raw files. -}
filterFiles :: FormatFiles -> WhitelistFiles -> IO [FilePath]
filterFiles (FormatFiles rawFiles) (WhitelistFiles rawWhitelist) = do
  files <- traverse (makeAbsolute >=> makeRelativeToCurrentDirectory) rawFiles
  whitelist <- traverse (makeAbsolute >=> makeRelativeToCurrentDirectory) rawWhitelist
  let matchesFiles fp = elem "." files || any (flip isPrefixOf fp) files
      matchesWhitelist fp = elem "." whitelist || any (flip isPrefixOf fp) whitelist
      isValid fp = isSuffixOf ".hs" fp && matchesFiles fp && not (matchesWhitelist fp)
  filter isValid . lines <$> readCreateProcess (shell "git ls-files") ""


{- | Configuration type, to be formatted from a file called `.simformatrc` typically found in the same directory as
where the script is running. -}
data Config = Config
  { configFiles :: FormatFiles
  {- ^ The list of files or directories to consider. -}
  , configWhitelist :: WhitelistFiles
  {- ^ The list of files or directories to exclude that are otherwise included by 'configFiles'. -}
  }


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
