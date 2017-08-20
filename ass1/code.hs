import Text.Regex    -- cabal install regex-compat
import Text.Regex.PCRE    -- cabal install regex-pcre
import Data.List

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
------------------  question 1 part a  ------------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

lineToWords :: String -> [String]
lineToWords line = getAllTextMatches $ line =~ "[A-Za-z0-9]+" :: [String]

-- a less elagant way using more basic functions:
lineToWordsAlt :: String -> [String]
lineToWordsAlt ""   = []
lineToWordsAlt line = takeWhile isWantedCha line : lineToWordsAlt rest
                      where rest = dropWhile (not . isWantedCha) $ dropWhile isWantedCha line

isWantedCha :: Char -> Bool
isWantedCha cha = cha `elem` (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
------------------  question 1 part b  ------------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

linesToWords :: [String] -> [String]
linesToWords lynes = lineToWords $ unwords lynes

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
------------------  question 1 part c  ------------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

-- This is not very DRY, however, I don't know how to extract a single 'let' or 'where' clause in list comprehension :(.
posOfWords :: [String] -> [(String, Int, Int)]
posOfWords lynes =
 nub [ (word, indexOfLine, indexOfWord) |
         indexOfLine <- [1..(length lynes)],
         word        <- let line = lynes !! (indexOfLine - 1) in lineToWords line,
         indexOfWord <- let line = lynes !! (indexOfLine - 1) in [1..(length line)],
         let line = lynes !! (indexOfLine - 1) in word == subString line (indexOfWord - 1) (length word) ]

subString :: String -> Int -> Int -> String
subString str start size = take size $ drop start str

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
------------------  question 2 part a  ------------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

wrapLines :: Int -> [String] -> [String]
wrapLines n lynes
  | n <= 1    = error "N has to be larger than 1"
  | otherwise = processWords n str
  where str = unwords $ map (unwords . words) lynes

processWords :: Int -> String -> [String]
processWords n str
  | n <= 1            = error "N has to be larger than 1"
  | null str          = []
  | length first < n  = taken : processWords n rest
  | length first == n = take n str : processWords n (drop (n + 1) str)
  | length first > n  = (take (n - 1) first ++ "-") : processWords n (drop (n - 1) str)
  where (taken, rest) = takenAndRest n str;
        first         = takeWhile (/= ' ') str

takenAndRest :: Int -> String -> (String, String)
takenAndRest n str
  | n <= 1          = error "N has to be larger than 1"
  | length str <= n = (str, "")
  | length str > n  = (taken, rest)
  where rest                    = drop (1 + length taken) str;
        taken | str !! n == ' ' = take n str
              | otherwise       = reverse $ drop 1 $ dropWhile (/= ' ') (reverse $ take n str)

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
------------------  question 2 part b  ------------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

justifyLines :: Int -> [String] -> [String]
justifyLines n lynes
  = initLines ++ [lastLine]    -- seems weird to not justify the last line
    where initLines   = map (processLine n) (take (numLines - 1) wrapedLines);
          lastLine    = wrapedLines !! (numLines - 1);
          numLines    = length wrapedLines
          wrapedLines = wrapLines n lynes

processLine :: Int -> String -> String
processLine n line = padLeft n $ bloatedLine n line

bloatedLine :: Int -> String -> String
bloatedLine n str
  = subRegex (mkRegex " ") str moreSpace
    where moreSpace                       = replicate x ' ';
          numSpaces                       = str =~ " " :: Int;
          availableSpaces                 = n - length str + numSpaces;
          x | availableSpaces < numSpaces = 1
            | otherwise                   = availableSpaces `div` numSpaces

padLeft :: Int -> String -> String
padLeft n str = whiteSpace ++ str
                where whiteSpace = replicate (n - length str) ' '

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
------------------  question 3 encode  ------------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

encode :: [String] -> ([String], [Int])
encode lynes = lexiWords $ reverse $ words $ unwords lynes

lexiWords :: [String] -> ([String], [Int])
lexiWords []       = ([], [])
lexiWords (wd:wds) = addToLexicon wd (lexiWords wds)

addToLexicon :: String -> ([String], [Int]) -> ([String], [Int])
addToLexicon wd (wds, indices)
  | wd == "\n"    = (wds, indices ++ [0])    -- this guard clause is for encodeLines
  | wd `elem` wds = (wds, indices ++ [idx])
  | otherwise     = (wds ++ [wd], indices ++ [length wds + 1])
  where idx = indexOf 0 wd wds

indexOf :: Eq a => Int -> a -> [a] -> Int
indexOf i e es
  | i >= length es = 0    -- the case that we can't find one. Not possibly used in this assignment
  | es !! i == e   = i + 1
  | otherwise      = indexOf (i + 1) e es

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
------------------  question 3 decode  ------------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

decode :: ([String], [Int]) -> String
decode (wordz, indices) = init $ _decode (wordz, indices, 0)

_decode :: ([String], [Int], Int) -> String
_decode (wordz, indices, pointer)
  | pointer >= length indices = ""
  | otherwise                 = word ++ " " ++ _decode (wordz, indices, pointer + 1)
  where idx  = indices !! pointer
        word = wordz !! (idx - 1)

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
---------------  question 3 encode lines  ---------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

encodeLines :: [String] -> ([String], [Int])
encodeLines lynes = lexiWords $ reverse $ _words $ unwords lynesWithNewLine
                    where lynesWithNewLine = map (++ " \n") (init lynes) ++ [last lynes]

-- this function is like the statndard words, except retaining the newline character '\n'
_words :: String -> [String]
_words []     = []
_words xxs@(x:xs)
  | x == ' '  = _words xs
  | otherwise = ys : _words rest
  where (ys, rest) = break (== ' ') xxs

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --
---------------  question 3 decode lines  ---------------
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- + -- + --

decodeLines :: ([String], [Int]) -> [String]
decodeLines (wordz, indices) = _decodeLines (wordz, breakIntoLines indices)

_decodeLines :: ([String], [[Int]]) -> [String]
_decodeLines (_, [])       = []
_decodeLines (wordz, i:is) = line : _decodeLines (wordz, is)
                             where line = unwords $ map (\e -> wordz !! (e - 1)) i

breakIntoLines :: [Int] -> [[Int]]
breakIntoLines [] = []
breakIntoLines indices@(idx:rest)
  | idx == 0      = breakIntoLines rest
  | otherwise     = fstLine : breakIntoLines restLines
  where (fstLine, restLines) = break (== 0) indices
