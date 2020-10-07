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
import Data.Yaml ((.:), FromJSON, parseJSON, withObject)
import System.Directory (makeAbsolute, makeRelativeToCurrentDirectory)
import System.Process (readCreateProcess, shell)


{- | A newtype wrapper for files or directories we want to format. -}
newtype FormatFiles = FormatFiles { unFormatFiles :: [FilePath] }
  deriving (Eq, Ord, Show, FromJSON)


{- | Filter files in a git repository by considering the files we do want to format. Normalize the raw files. -}
filterFiles :: FormatFiles -> IO [FilePath]
filterFiles (FormatFiles rawFiles) = do
  files <- traverse (makeAbsolute >=> makeRelativeToCurrentDirectory) rawFiles
  let matchesFiles fp = elem "." files || any (flip isPrefixOf fp) files
      isValid fp = isSuffixOf ".hs" fp && matchesFiles fp
  filter isValid . lines <$> readCreateProcess (shell "git ls-files") ""


{- | Configuration type, to be formatted from a file called `.simformatrc` typically found in the same directory as
where the script is running. -}
data Config = Config
  { configFiles :: FormatFiles
  {- ^ The list of files or directories to consider. -}
  }


{- | A default, empty config in case `.simformatrc` is not provided or not found. -}
emptyConfig :: Config
emptyConfig = Config
  { configFiles = FormatFiles ["."]
  }


instance FromJSON Config where
  parseJSON = withObject "Config" $ \obj ->
    Config
      <$> obj .: "files"
