module Main where

import System.IO
import System.Exit (exitSuccess)
import Data.Char (toUpper)
import Text.Read (reads)
import Ciphers (inCaesar, unCaesar, inVig, unVig)



encDec :: IO Char
encDec = do
  putStrLn ""
  putStrLn "CIPHER PROGRAM"
  putStrLn ""
  putStrLn "Please press E for encoding"
  putStrLn "or D for decoding,"
  ed <- getChar
  getChar
  return ed

phrase :: IO String
phrase = do
  putStrLn "Please enter target phrase: "
  ph <- getLine
  return ph

cipher :: IO Char
cipher =  do
  putStrLn "Press C for Caesar, V for Vigenere,"
  putStrLn "and anything else to exit."
  ci <- getChar
  getChar
  return ci

main :: IO ()
main = do
  ed <- encDec
  ph <- phrase
  ci <- cipher
  choice (toUpper ci) (toUpper ed) ph

choice :: Char -> Char -> String -> IO ()
choice ci ed ph
    | ed `notElem` "ED" = exitSuccess
    | ci == 'C' = caesar ed ph
    -- | cipher == 'V' = vigenere ed phrase
    | otherwise = exitSuccess

parseInt :: String -> Maybe Int
parseInt s = case (reads s) :: [(Int, String)] of
  [(int, _)] -> Just int
  [] -> Nothing

caesar :: Char -> String -> IO ()
caesar ed ph = do
  hSetBuffering stdout NoBuffering
  putStrLn "Now enter how many chars to move (number): "
  rawMoves <- getLine
  case (parseInt rawMoves) of
    Just moves ->
        case (ed == 'E') of
          True -> putStrLn (inCaesar moves ph)
          False ->  putStrLn (unCaesar moves ph)
    Nothing -> exitSuccess
  exitSuccess

vigenere :: Char -> IO ()
vigenere c = undefined

