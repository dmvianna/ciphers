module Main where

import System.IO
import System.Exit (exitSuccess)
import Data.Char (toUpper)
import Text.Read (reads)
import Ciphers (inCaesar, unCaesar, inVig, unVig)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn ""
  putStrLn "CIPHER PROGRAM"
  putStrLn ""
  putStrLn "Please press E for encoding"
  putStrLn "or D for decoding,"
  ed <- getChar
  putStrLn ""
  putStrLn "Please enter target phrase: "
  phrase <- getLine
  putStrLn ""
  putStrLn "Press C for Caesar, V for Vigenere,"
  putStrLn "and anything else to exit."
  cipher <- getChar
  choice (toUpper cipher) (toUpper ed) phrase


choice :: Char -> Char -> String -> IO ()
choice cipher ed phrase
    | ed `notElem` "ED" = exitSuccess
    | cipher == 'C' = caesar ed phrase
    -- | cipher == 'V' = vigenere ed phrase
    | otherwise = exitSuccess

parseInt :: String -> Maybe Int
parseInt s = case (reads s) :: [(Int, String)] of
  [(int, _)] -> Just int
  [] -> Nothing

caesar :: Char -> String -> IO ()
caesar ed phrase = do
  hSetBuffering stdout NoBuffering
  putStrLn "Now enter how many chars to move (number): "
  rawMoves <- getLine
  case (parseInt rawMoves) of
    Just moves ->
        case (ed == 'E') of
          True -> putStrLn (inCaesar moves phrase)
          False ->  putStrLn (unCaesar moves phrase)
    Nothing -> exitSuccess
  exitSuccess

vigenere :: Char -> IO ()
vigenere c = undefined

