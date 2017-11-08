import Test.DocTest

main :: IO ()
main = doctest ["-XRecordWildCards", "main.hs"]
