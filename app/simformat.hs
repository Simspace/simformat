{-| Description: Source code for the `simformat` script, which will format and validate imports for Haskell source files. -}
module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Either (fromRight)
import Data.Foldable (for_)
import Data.Maybe (catMaybes)
import Data.Traversable (for)
import Data.Version (showVersion)
import Data.Yaml (decodeFileEither)
import SimSpace.Config (Config(Config), configFiles, configWhitelist, emptyConfig, filterFiles)
import Turtle (decodeString, liftIO, testfile)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Options.Applicative as Opt

import SimSpace.SimFormat (reformat)

import qualified Paths_simformat as Paths

data Operation
  = PrintVersion Bool Bool
  | PrintNumericVersion Bool Bool
  | InPlace FilePath Bool Bool
  | Repo Bool Bool

data Opts = Opts
  { optsConfig    :: FilePath
  , optsVerbose   :: Bool
  , optsOperation :: Operation
  }

parseArgs :: IO Opts
parseArgs = Opt.execParser (Opt.info (Opt.helper <*> parser) $ Opt.progDesc "Format some Haskell source files according to the SimSpace convention. Defaults to formatting the entire repo.")
  where
    parser = Opts
      <$> Opt.strOption (Opt.long "config" <> Opt.metavar "config" <> Opt.help "Path to config" <> Opt.value ".simformatrc" <> Opt.showDefault)
      <*> Opt.switch (Opt.long "verbose" <> Opt.help "Be verbose")
      <*> parseOperation

parseOperation :: Opt.Parser Operation
parseOperation =
    (
      printVersion
      <|> printNumericVersion
      <|> inPlace
      <|> repo
    )
    <*> regroup
    <*> validate
  where
    printVersion = Opt.flag' PrintVersion
      $  Opt.long "version"
      <> Opt.short 'v'
      <> Opt.help "print the version preceded by the string 'simformat '"
    printNumericVersion = Opt.flag' PrintNumericVersion
      $  Opt.long "numeric-version"
      <> Opt.help "print the version"
    inPlace = fmap InPlace . Opt.strOption
      $  Opt.long "in-place"
      <> Opt.short 'i'
      <> Opt.metavar "TARGET"
      <> Opt.help "operate on a file in-place instead of reading from stdin and writing to stdout"
    repo = pure Repo

    regroup =
      Opt.switch
        (
          Opt.long "regroup-imports"
          <> Opt.short 'r'
          <> Opt.help (
               "Try to be smart and re-group imports into a prelude "
               <> "group, a non-local group, and a local group, where "
               <> "\"local\" means things that are being built out of "
               <> "your stack.yaml file. This is really, really slow, "
               <> "and the slowness is linear with the number of import "
               <> "statements you have because it uses `ghc-pkg` to figure "
               <> "out where packages come from. It also requires that "
               <> "you are using stack."
             )
        )
     <|> pure False

    validate =
      Opt.switch
        (
          Opt.long "validate"
          <> Opt.help "Validate the file(s) specified. Exits with 1 if they don't match."
        )

putStrLn' :: Bool -> String -> IO ()
putStrLn' v s = if v then putStrLn s else pure ()

main :: IO ()
main = do
  Opts {..} <- parseArgs
  config <- fromRight emptyConfig <$> decodeFileEither optsConfig
  case optsOperation of
    PrintVersion _ _              -> putStrLn $ "simformat " ++ showVersion Paths.version
    PrintNumericVersion _ _       -> putStrLn $ showVersion Paths.version
    InPlace file regroup validate -> format optsVerbose config (Just file) regroup validate
    Repo regroup validate         -> format optsVerbose config Nothing regroup validate
  where
    format verbose Config {..} fileMay regroup validate = do
      files <- case fileMay of
        Just file -> pure [file]
        Nothing -> filterFiles configFiles configWhitelist
      inputsAndOutputs <- fmap catMaybes . for files $ \ file ->
        liftIO (testfile $ decodeString file) >>= \ case
          False -> do
            putStrLn' verbose $ "Skipping " <> file <> " because it was not in the config"
            pure Nothing
          True -> do
            input <- BS.readFile file
            reformatted <- reformat regroup . lines . T.unpack . T.decodeUtf8 $ input
            let output = T.encodeUtf8 . T.pack . unlines $ reformatted
            pure $ Just (file, input, output)
      case validate of
        True -> do
          let failures = catMaybes . flip map inputsAndOutputs $ \ (file, input, output) ->
                if input == output then Nothing else Just $ file <> " didn't validate"
          when (not $ null failures) $ do
            fail $ unlines failures
        False -> do
          for_ inputsAndOutputs $ \ (file, _, output) -> do
            putStrLn' verbose $ "Reformatting " <> file
            BS.writeFile file output
