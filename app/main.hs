{-# LANGUAGE LambdaCase #-}
module Main (main) where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative (Parser, execParser, flag', fullDesc, help, helper, info, long, metavar, progDesc, short, strOption)

import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import SimSpace.SimFormat (reformat)

import qualified Paths_simformat as Paths

data Operation
  = PrintVersion
  | PrintNumericVersion
  | InPlace FilePath
  | StdIO

parseOperation :: Parser Operation
parseOperation = printVersion <|> printNumericVersion <|> inPlace <|> stdIO
  where
    printVersion = flag' PrintVersion
      $  long "version"
      <> short 'v'
      <> help "print the version preceded by the string 'simformat '"
    printNumericVersion = flag' PrintNumericVersion
      $  long "numeric-version"
      <> help "print the version"
    inPlace = fmap InPlace . strOption
      $  long "in-place"
      <> short 'i'
      <> metavar "TARGET"
      <> help "operate on a file in-place instead of reading from stdin and writing to stdout"
    stdIO = pure StdIO

main :: IO ()
main = execParser opts >>= \case
    PrintVersion        -> putStrLn $ "simformat " ++ showVersion Paths.version
    PrintNumericVersion -> putStrLn $ showVersion Paths.version
    InPlace file        ->  BS.writeFile file
                        =<< T.encodeUtf8 . T.pack . unlines . reformat . lines . T.unpack . T.decodeUtf8
                        <$> BS.readFile file
    StdIO               -> traverse_ putStrLn =<< reformat . lines <$> getContents
  where
    opts = info (helper <*> parseOperation)
         $ fullDesc <> progDesc "Format the imports of a Haskell file"
