module Main (main) where

import Control.Applicative ((<|>))
import Data.Foldable (traverse_)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative (Parser, execParser, flag', fullDesc, help, helper, info, long, metavar, progDesc, short, strOption)
import Turtle (decodeString, inshell, liftIO, lineToText, testfile)
import Turtle.Shell (FoldShell(FoldShell), foldShell)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import SimSpace.SimFormat (reformat)

import qualified Paths_simformat as Paths

data Operation
  = PrintVersion
  | PrintNumericVersion
  | InPlace FilePath
  | AllDirty
  | StdIO

parseOperation :: Parser Operation
parseOperation =
    (
      printVersion
      <|> printNumericVersion
      <|> inPlace
      <|> allDirty
      <|> stdIO
    )
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
    allDirty = flag' AllDirty
      $  long "all-dirty"
      <> help "operate on all dirty files in-place instead of reading from stdin and writing to stdout"
    stdIO = pure StdIO

main :: IO ()
main = execParser opts >>= \case
    PrintVersion        -> putStrLn $ "simformat " ++ showVersion Paths.version
    PrintNumericVersion -> putStrLn $ showVersion Paths.version
    InPlace file        -> oneInPlace file
    AllDirty            ->
      let cmd = inshell "git status --porcelain | grep ^\\.\\*\\.hs$ | sed s/^...//" mempty -- get all the dirty Haskell files in the tree
          op = FoldShell (\ () file -> () <$ oneInPlace (T.unpack $ lineToText file)) () (const $ pure ()) -- for each file in the shell modify in place
      in foldShell cmd op
    StdIO               -> traverse_ putStrLn =<< reformat . lines <$> getContents
  where
    oneInPlace file = do
      liftIO (testfile $ decodeString file) >>= \ case
        False -> putStrLn $ "Skipping " <> file
        True -> do
          putStrLn $ "Reformatting " <> file
          BS.writeFile file
            =<< T.encodeUtf8 . T.pack . unlines . reformat . lines . T.unpack . T.decodeUtf8
            <$> BS.readFile file
    opts = info (helper <*> parseOperation)
         $ fullDesc <> progDesc "Format the imports of a Haskell file(s)"
