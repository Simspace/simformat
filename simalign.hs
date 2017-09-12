{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Test.DocTest

import Data.Bifunctor (bimap)
import Data.Char (isPunctuation, isSymbol)
import Data.Ord (comparing)
import Data.List (init, inits, maximumBy)
import Data.Map ((!), Map)
import Data.These (These(..))
import qualified Data.Map as Map

-- |
-- >>> lcs (\c1 c2 -> if c1 == c2 then 10 else 0) "banana" "bandana"
-- (60,[These 'b' 'b',These 'a' 'a',These 'n' 'n',That 'd',These 'a' 'a',These 'n' 'n',These 'a' 'a'])
lcs :: forall a b. (Ord a, Ord b)
    => (a -> b -> Int)
    -> [a]
    -> [b]
    -> (Int, [These a b])
lcs score w1 w2 = cache ! (w1, w2)
  where
    cache :: Map ([a], [b]) (Int, [These a b])
    cache = Map.fromList [ ((w1', w2'), go w1' w2')
                         | w1' <- inits w1
                         , w2' <- inits w2
                         ]
    
    go :: [a] -> [b] -> (Int, [These a b])
    go [] _ = (0, [])
    go _ [] = (0, [])
    go w1' w2' = maximumBy (comparing fst)
               [ let (n, xs) = cache ! (init w1', init w2')
                     s = score c1 c2
                 in if s >= 2 then (n + s, xs ++ [These c1 c2])
                              else (n, xs ++ [This c1, That c2])
               , let (n, xs) = cache ! (init w1', w2')
                 in (n, xs ++ [This c1])
               , let (n, xs) = cache ! (w1', init w2')
                 in (n, xs ++ [That c2])
               ]
      where
        c1 = last w1'
        c2 = last w2'


-- |
-- >>> pad 4 "foo"
-- "foo "
pad :: Int -> String -> String
pad n s = take n (s ++ repeat ' ')

zipWhile :: (a -> b -> Bool) -> [a] -> [b] -> [(a,b)]
zipWhile p (x:xs) (y:ys) | p x y = (x,y) : zipWhile p xs ys
zipWhile _ _ _ = []

isHaskellSymbol :: Char -> Bool
isHaskellSymbol c = isSymbol c || isPunctuation c

longestPrefix :: String -> String -> Int
longestPrefix xs ys = sum . map factor . map fst $ zipWhile (==) xs ys
  where
    factor c | isHaskellSymbol c = 30
             | otherwise         = 1

-- |
-- >>> align "aaa bbb ccc" "aaa --- bbb --- --- ccc"
-- ("aaa     bbb         ccc","aaa --- bbb --- --- ccc")
-- >>> align "singletonMap (boot group <> showt k) $ keyedEnt bootGroupDiffs specificationBootGroups Diff.diffBootGroupSpec k" "singletonMap (subnet <> showt k) $ keyedEnt subnetDiffs specificationSubnets Diff.diffSubnetSpec k"
-- ("singletonMap (boot group <> showt k) $ keyedEnt bootGroupDiffs specificationBootGroups Diff.diffBootGroupSpec k","singletonMap (subnet     <> showt k) $ keyedEnt subnetDiffs    specificationSubnets    Diff.diffSubnetSpec k")
align :: String -> String -> (String, String)
align line1 line2 | n > 0     = render mapping
                  | otherwise = (line1, line2)
  where
    (n, mapping) = lcs longestPrefix (words line1) (words line2)
    
    render :: [These String String] -> (String, String)
    render = bimap unwords unwords . go ([], []) 0 0
      where
        go :: ([String], [String]) -> Int -> Int -> [These String String] -> ([String], [String])
        go (this, that) _ _ []                       = (this, that)
        go (this, that) i j (These this' that' : xs) = go ( this ++ [replicate (k-i) ' ' ++ this']
                                                          , that ++ [replicate (k-j) ' ' ++ that']
                                                          )
                                                          (length this')
                                                          (length that')
                                                          xs
          where
            k = max i j
        go (this, that) i j (This this'        : xs) = go ( this ++ [this']
                                                          , that
                                                          )
                                                          (i + length this' + 1)
                                                          j
                                                          xs
        go (this, that) i j (That that'        : xs) = go ( this
                                                          , that ++ [that']
                                                          )
                                                          i
                                                          (j + length that' + 1)
                                                          xs


main :: IO ()
main = doctest ["src/Main.hs"]
