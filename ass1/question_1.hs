module Question1 (
  wordsFromString,
  wordsFromListString,
  wordsPositions
) where

  import Text.Regex.PCRE
  import Data.List

  ------------------ part a ------------------

  wordsFromString :: String -> [String]
  wordsFromString line = getAllTextMatches $ line =~ "[A-Za-z0-9]+" :: [String]

  ------------------ part b ------------------

  wordsFromListString :: [String] -> [String]
  wordsFromListString lynes = wordsFromString $ unwords lynes

  ------------------ part c ------------------

  wordsPositions :: [String] -> [(String, Int, Int)]
  wordsPositions lynes =
   nub [ (word, indexOfLine, indexOfWord) |
           indexOfLine <- [1..(length lynes)],
           word        <- let line = lynes !! (indexOfLine - 1) in wordsFromString line,
           indexOfWord <- let line = lynes !! (indexOfLine - 1) in [1..(length line)],
           let line = lynes !! (indexOfLine - 1) in word == subString line (indexOfWord - 1) (length word) ]

  -- helper method. should be private, if that's a thing in Haskell??
  subString :: String -> Int -> Int -> String
  subString str start size = take size $ drop start str



  ----------
  ---------
