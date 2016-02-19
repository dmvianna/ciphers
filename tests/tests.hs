
module Main where

import Ciphers (inCaesar, unCaesar, inVig, unVig)
import Test.QuickCheck

genCaesar :: Gen (Int, String)
genCaesar = do
  k <- arbitrary
  t <- arbitrary
  return (k, t)

genVig :: Gen (String, String)
genVig = do
  k <- arbitrary
  t <- arbitrary
  return (k, t)

prop_Caesar :: Property
prop_Caesar =
  forAll genCaesar
  (\(k, t) -> unCaesar k (inCaesar k t) == t)

prop_Vig :: Property
prop_Vig =
  forAll genVig
  (\(k, t) -> unVig k (inVig k t) == t)

main :: IO ()
main = do
  putStrLn "\n unCaesar returns the same string\
           \\nwhich was encoded by inCaesar."
  quickCheck prop_Caesar
  putStrLn "\n unVig returns the same string\
            \\nwhich was encoded by inVig."
  quickCheck prop_Vig
