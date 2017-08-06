import Text.Regex    -- cabal install regex-compat
import Text.Regex.PCRE    -- cabal install regex-pcre
import Data.List

------------------ question 1 part a ------------------

lineToWords :: String -> [String]
lineToWords line = getAllTextMatches $ line =~ "[A-Za-z0-9]+" :: [String]

------------------ question 1 part b ------------------

linesToWords :: [String] -> [String]
linesToWords lynes = lineToWords $ unwords lynes

------------------ question 1 part c ------------------

-- This is not very DRY, however, I don't know how to extract a sigle 'let' or 'where' clause in list comprehension :(.
posOfWords :: [String] -> [(String, Int, Int)]
posOfWords lynes =
 nub [ (word, indexOfLine, indexOfWord) |
         indexOfLine <- [1..(length lynes)],
         word        <- let line = lynes !! (indexOfLine - 1) in lineToWords line,
         indexOfWord <- let line = lynes !! (indexOfLine - 1) in [1..(length line)],
         let line = lynes !! (indexOfLine - 1) in word == subString line (indexOfWord - 1) (length word) ]

-- helper method. should be private, if that's a thing in Haskell??
subString :: String -> Int -> Int -> String
subString str start size = take size $ drop start str

-------------------------------------------------------
-------------------------------------------------------

------------------ question 2 part a ------------------

wrapLines :: Int -> [String] -> [String]
wrapLines n lynes | n <= 1 = error "N has to be larger than 1"
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
                      where rest  = drop (1 + length taken) str;
                            taken | str !! n == ' ' = take n str
                                  | otherwise       = reverse $ drop 1 $ dropWhile (/= ' ') (reverse $ take n str)

------------------ question 2 part b ------------------

justifyLines :: Int -> [String] -> [String]
justifyLines n lynes = initLines ++ [lastLine]
                       where initLines = map (processLine n) (take (numLines - 1) wrapedLines);
                             lastLine  = wrapedLines !! (numLines - 1);
                             numLines  = length wrapedLines
                             wrapedLines = wrapLines n lynes

processLine :: Int -> String -> String
processLine n line = padLeft n $ bloatedLine n line

bloatedLine :: Int -> String -> String
bloatedLine n str = subRegex (mkRegex " ") str moreSpace
                    where moreSpace                       = replicate x ' ';
                          numSpaces                       = str =~ " " :: Int;
                          availableSpaces                 = n - length str;
                          x | availableSpaces < numSpaces = 1
                            | otherwise                   = availableSpaces `div` numSpaces

padLeft :: Int -> String -> String
padLeft n str = whiteSpace ++ str
                where whiteSpace = replicate (n - length str) ' '

-------------------------------------------------------
-------------------------------------------------------

--------------------- question 3 ----------------------

encode :: [String] -> ([String], [Int])



decode :: ([String], [Int]) -> String


-- For those wanting to write an encode keeps track of new lines, I suggest also having
encodeLines :: [String] -> ([String], [Int])



decodeLines :: ([String], [Int]) -> [String]
