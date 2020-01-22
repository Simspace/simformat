import Test.DocTest

main :: IO ()
main = doctest ["-XRecordWildCards", "src/SimSpace/SimFormat.hs"]
