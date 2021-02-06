{-# OPTIONS_GHC -fno-warn-orphans #-}
module ConfigSpec where

import Data.List (intercalate)
import Data.Yaml (encode)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, generate, listOf1)
import UnliftIO.Temporary (withSystemTempDirectory)
import qualified Data.ByteString.Char8 as C8

-- the module being tested
import SimSpace.Config

newtype AbsoluteTest = AbsoluteTest { unAbsoluteTest :: FilePath }
  deriving (Eq, Show)

newtype RelativeTest = RelativeTest { unRelativeTest :: FilePath }
  deriving (Eq, Show)

alphaWord :: Gen FilePath
alphaWord = listOf1 $ elements ['a'..'z']

instance Arbitrary AbsoluteTest where
  arbitrary = AbsoluteTest . ("/" <>) . intercalate "/" <$> listOf1 alphaWord

instance Arbitrary RelativeTest where
  arbitrary = RelativeTest . intercalate "/" <$> listOf1 alphaWord

instance Arbitrary Config where
  arbitrary = Config
    <$> (FormatFiles . fmap unRelativeTest <$> listOf1 arbitrary)
    <*> (WhitelistFiles . fmap unRelativeTest <$> listOf1 arbitrary)

spec :: Spec
spec = describe "Config" $ do
  prop "absolute filename is always valid" $ \ (AbsoluteTest fp) -> isValid [fp] [] (fp </> "test.hs") `shouldBe` True
  prop "relative filename is always valid" $ \ (RelativeTest fp) -> isValid [fp] [] (fp </> "test.hs") `shouldBe` True
  prop "filtered absolute directory is not valid" $ \ (AbsoluteTest fp) -> isValid [fp] [fp </> "bar"] (fp </> "bar" </> "bin") `shouldBe` False
  prop "filtered relative directory is not valid" $ \ (RelativeTest fp) -> isValid [fp] [fp </> "bar"] (fp </> "bar" </> "bin") `shouldBe` False
  prop "filtered absolute filename is not valid" $ \ (AbsoluteTest fp) -> isValid [fp] [fp </> "bar"] (fp </> "bar" </> "test.hs") `shouldBe` False
  prop "filtered relative filename is not valid" $ \ (RelativeTest fp) -> isValid [fp] [fp </> "bar"] (fp </> "bar" </> "test.hs") `shouldBe` False

  it "can find and normalize a config" $ do
    expected :: Config <- generate arbitrary
    (dir, actual) <- withSystemTempDirectory "simformat" $ \ dir -> do
      writeFile (dir </> ".simformatrc") (C8.unpack $ encode expected)
      config <- findConfig [dir </> "test.hs"]
      pure (dir, config)
    actual `shouldBe` normalizeConfig dir expected
