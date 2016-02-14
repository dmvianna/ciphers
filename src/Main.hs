module Main where


import System.IO
import System.Exit (exitSuccess)
import Data.Char (toUpper)
import Ciphers (inCaesar, unCaesar, inVig, unVig)


encDec :: IO Char
encDec = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  putStr "\nCIPHER PROGRAM\n\
           \\nPlease press E for encoding\
           \\nor D for decoding: "
  getChar

phrase :: IO String
phrase = do
  putStrLn "\nPlease enter target phrase: \n"
  getLine

cipher :: IO Char
cipher =  do
  hSetBuffering stdout NoBuffering
  putStr "\nPress C for Caesar, V for Vigenere,\
           \\nand anything else to exit: "
  getChar

main :: IO ()
main = do
  ed <- encDec
  ph <- phrase
  ci <- cipher
  choice (toUpper ci) (toUpper ed) ph

choice :: Char -> Char -> String -> IO ()
choice ci ed ph
    | ci == 'C' = caesar ed ph
    | ci == 'V' = vigenere ed ph
    | otherwise = exitSuccess

parseInt :: String -> Maybe Int
parseInt s = case reads s :: [(Int, String)] of
  [(int, _)] -> Just int
  (_,_):(_:_) -> Nothing
  [] -> Nothing

caesar :: Char -> String -> IO ()
caesar ed ph = do
  hSetBuffering stdout NoBuffering
  putStr "\nNow enter how many chars to move (number): "
  rawMoves <- getLine
  case parseInt rawMoves of
    Just moves ->
        if ed == 'E'
        then putStrLn (inCaesar moves ph)
        else putStrLn (unCaesar moves ph)
    Nothing -> exitSuccess

vigenere :: Char -> String -> IO ()
vigenere ed target = do
  putStrLn "\nNow enter key phrase (string): \n"
  key <- getLine
  if ed == 'E'
  then putStrLn (inVig key target)
  else putStrLn (unVig key target)
  exitSuccess
