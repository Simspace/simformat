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
  | Repo [FilePath] Bool Bool
  | Editor Bool Bool

data Opts = Opts
  { optsConfig    :: FilePath
  , optsVerbose   :: Bool
  , optsOperation :: Operation
  , optsAllFiles  :: Bool
  }

parseArgs :: IO Opts
parseArgs = Opt.execParser (Opt.info (Opt.helper <*> parser) $ Opt.progDesc "Format some Haskell source files according to the SimSpace convention. Defaults to formatting the entire repo.")
  where
    parser = Opts
      <$> Opt.strOption (Opt.long "config" <> Opt.metavar "config" <> Opt.help "Path to config" <> Opt.value ".simformatrc" <> Opt.showDefault)
      <*> Opt.switch (Opt.long "verbose" <> Opt.help "Be verbose")
      <*> parseOperation
      <*> Opt.switch ( Opt.long "all-files" <> Opt.help "Don't filter with git, use all files" )

parseOperation :: Opt.Parser Operation
parseOperation =
    (
      printVersion
      <|> printNumericVersion
      <|> repo
      <|> editor
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
    repo = Repo <$> Opt.many (Opt.strArgument $ Opt.metavar "FILE")
    editor = Opt.flag' Editor
      $  Opt.long "editor"
      <> Opt.short 'e'
      <> Opt.help "reads from stdin and outputs formatted to stdout (ignores --validate), for use with per-file editor integration"

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
          <> Opt.help "Validate the file(s) specified. Exits with 1 if they don't match. Ignored by --editor"
        )

putStrLn' :: Bool -> String -> IO ()
putStrLn' v s = if v then putStrLn s else pure ()

main :: IO ()
main = do
  Opts {..} <- parseArgs
  config <- fromRight emptyConfig <$> decodeFileEither optsConfig
  case optsOperation of
    PrintVersion _ _               -> putStrLn $ "simformat " ++ showVersion Paths.version
    PrintNumericVersion _ _        -> putStrLn $ showVersion Paths.version
    Repo fileList regroup validate -> format optsVerbose optsAllFiles config fileList regroup validate
    Editor regroup _               -> formatStdIn regroup
  where
    format verbose allFiles Config {..} fileList regroup validate = do
      files <- filterFiles configFiles configWhitelist allFiles fileList
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
    formatStdIn regroup = do
      getContents >>= reformat regroup . lines >>= putStr . unlines
