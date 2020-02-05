import Test.DocTest

main :: IO ()
main = doctest ["-XRecordWildCards", "-XLambdaCase", "src/SimSpace/SimFormat.hs"]
