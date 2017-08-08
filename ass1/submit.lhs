Some functions I used in this assignment are not in standard Prelude. It's pointless
to reinvent wheels. That being said, I did include alternative implementations
(i.e. less elagant ways using more basic functions).

N.B. hackage name for `Text.Regex` is `regex-compat`, and for `Text.Regex.PCRE`
`regex-pcre`.

> import Text.Regex    -- cabal install regex-compat
> import Text.Regex.PCRE    -- cabal install regex-pcre
> import Data.List
> import Data.Char

Question 1 part a:

A one line solution using regex matching:

> lineToWords :: String -> [String]
> lineToWords line = getAllTextMatches $ line =~ "[A-Za-z0-9]+" :: [String]

Then I tried doing it with plain old standard functions. This one is using ascii
number for checking if it's wanted char. Here is another implementation:

> lineToWordsAlt :: String -> [String]
> lineToWordsAlt ""   = []
> lineToWordsAlt line = takeWhile isWantedCha line : lineToWordsAlt rest
>                       where rest = dropWhile (not . isWantedCha) $ dropWhile isWantedCha line
>
> isWantedCha :: Char -> Bool
> isWantedCha cha = (47 < asc && asc < 58) || (64 < asc && asc < 91) || (96 < asc && asc < 123)
>                   where asc = ord cha

Question 1 part b:

The solution uses the function `lineToWords` defined in part a:

> linesToWords :: [String] -> [String]
> linesToWords lynes = lineToWords $ unwords lynes

Question 1 part c:

This solution is not very DRY. There are lots of repetition in the list comprehension.
I don't know how to extract variables into a single 'let' or 'where' clause in list
comprehension syntax :( Also, I have a feeling that using list comprehension in
this question is not an ideal approach, so again :(

> posOfWords :: [String] -> [(String, Int, Int)]
> posOfWords lynes =
>  nub [ (word, indexOfLine, indexOfWord) |
>          indexOfLine <- [1..(length lynes)],
>          word        <- let line = lynes !! (indexOfLine - 1) in lineToWords line,
>          indexOfWord <- let line = lynes !! (indexOfLine - 1) in [1..(length line)],
>          let line = lynes !! (indexOfLine - 1) in word == subString line (indexOfWord - 1) (length word) ]
>
> subString :: String -> Int -> Int -> String
> subString str start size = take size $ drop start str

Question 2 part a:

The basic idea here is:

1) Break each line into words (using the function `words`), and put them back together.
   In doing this, all white spaces are removed, and multiple consecutive whitespaces
   are replaced with a single space character.

   e.g.
   [
     "    1234567890        For 12345678901        ",
     "     example, in this     ",
     "sentence, there are 9 words.",
     "Though we have yet to try the maximization",
     "of this example!"
    ]
    becomes:
    "1234567890 For 12345678901 example, in this sentence, there are 9 words.
    Though we have yet to try the maximization of this example!"

2) Compare the length of the first word with the given N.
   - If the first word is longer than N, then break it into two parts where the
     first part has '-' at end.
   - If it's equal to N, then use it as the current line and process the rest.
   - If it's shorter than N, then the function `takenAndRest` will be used to decide
     how many word(s) is/are taken as current line and what's the left string.

> wrapLines :: Int -> [String] -> [String]
> wrapLines n lynes
>   | n <= 1    = error "N has to be larger than 1"
>   | otherwise = processWords n str
>                 where str = unwords $ map (unwords . words) lynes
>
> processWords :: Int -> String -> [String]
> processWords n str
>   | n <= 1            = error "N has to be larger than 1"
>   | null str          = []
>   | length first < n  = taken : processWords n rest
>   | length first == n = take n str : processWords n (drop (n + 1) str)
>   | length first > n  = (take (n - 1) first ++ "-") : processWords n (drop (n - 1) str)
>                         where (taken, rest) = takenAndRest n str;
>                               first         = takeWhile (/= ' ') str
>
> takenAndRest :: Int -> String -> (String, String)
> takenAndRest n str
>   | n <= 1          = error "N has to be larger than 1"
>   | length str <= n = (str, "")
>   | length str > n  = (taken, rest)
>                       where rest  = drop (1 + length taken) str;
>                             taken | str !! n == ' ' = take n str
>                                   | otherwise       = reverse $ drop 1 $ dropWhile (/= ' ') (reverse $ take n str)

Question 2 part b:

The basic idea:
1) Use the function `wrapLines` from question 2 part a to get the output from
   part a.
2) For each line, we bloat it with spaces (if possible) using function `bloatedLine`,
   and then pad spaces to the left (if possible) using function `padLeft` so it'll
   be right-justified.

> justifyLines :: Int -> [String] -> [String]
> justifyLines n lynes
>   = initLines ++ [lastLine]  -- seems weird to not justify the last line
>     where initLines   = map (processLine n) (take (numLines - 1) wrapedLines);
>           lastLine    = wrapedLines !! (numLines - 1);
>           numLines    = length wrapedLines
>           wrapedLines = wrapLines n lynes
>
> processLine :: Int -> String -> String
> processLine n line = padLeft n $ bloatedLine n line
>
> bloatedLine :: Int -> String -> String
> bloatedLine n str
>   = subRegex (mkRegex " ") str moreSpace
>     where moreSpace                       = replicate x ' ';
>           numSpaces                       = str =~ " " :: Int;
>           availableSpaces                 = n - length str;
>           x | availableSpaces < numSpaces = 1
>             | otherwise                   = availableSpaces `div` numSpaces
>
> padLeft :: Int -> String -> String
> padLeft n str = whiteSpace ++ str
>                 where whiteSpace = replicate (n - length str) ' '

Question 3 encode:

Basic idea:
1) Break the input into a list of words
2) For each word (in reversed order):
   - If it's in the lexicon, then append the index of it to the end of the list
     of indices.
   - If it's not in the lexicon, then append the word to the end of the lexicon,
     and also append the index of it to the end of the list of indices.

> encode :: [String] -> ([String], [Int])
> encode lynes = lexiWords $ reverse $ words $ unwords lynes
>
> lexiWords :: [String] -> ([String], [Int])
> lexiWords []       = ([], [])
> lexiWords (wd:wds) = addToLexicon wd (lexiWords wds)
>
> addToLexicon :: String -> ([String], [Int]) -> ([String], [Int])
> addToLexicon wd (wds, indices)
>   | wd == "\n"    = (wds, indices ++ [0])    -- this guard clause is for encodeLines
>   | wd `elem` wds = (wds, indices ++ [idx])
>   | otherwise     = (wds ++ [wd], indices ++ [length wds + 1])
>     where idx = indexOf 0 wd wds
>
> indexOf :: Eq a => Int -> a -> [a] -> Int
> indexOf i e es
>   | i >= length es = 0    -- the case that we can't find one. Not possibly used in this assignment
>   | es !! i == e   = i + 1
>   | otherwise      = indexOf (i + 1) e es

Question 3 decode:

The idea of decode is rather simple: scan the indices from left to right, and get
the corresponding words from lexicon, and put them together.

> decode :: ([String], [Int]) -> String
> decode (wordz, indices) = init $ _decode (wordz, indices, 0)
>
> _decode :: ([String], [Int], Int) -> String
> _decode (wordz, indices, pointer)
>   | pointer >= length indices = ""
>   | otherwise                 = word ++ " " ++ _decode (wordz, indices, pointer + 1)
>                                 where idx = indices !! pointer
>                                       word = wordz !! (idx - 1)

Question 3 encode lines:

For `encodeLines`, the implementation is extended from `encode`. The differences
exist in 1. the preprocessing part, where each line is appended with a new line
character '\n'; and 2, the part of dealing with the new line character, where 0
is appended to the list of indices.

For this part I also wrote a `_words` function which is like the standard `words`
function except that it treats the new line character as a word.

> encodeLines :: [String] -> ([String], [Int])
> encodeLines lynes = lexiWords $ reverse $ _words $ unwords lynesWithNewLine
>                     where lynesWithNewLine = map (++ " \n") (init lynes) ++ [last lynes]
>
> -- this function is like the statndard words, except retaining the newline character '\n'
> _words :: String -> [String]
> _words []     = []
> _words xxs@(x:xs)
>   | x == ' '  = _words xs
>   | otherwise = ys : _words rest
>                 where (ys, rest) = break (== ' ') xxs

Question 3 decode lines:

Basic idea:
1) Break the list of indices from where the index is 0.
   e.g. form [1,2,3,4,5,2,3,6,0,1,2,3,7,5,2,3,8] to [[1,2,3,4,5,2,3,6], [1,2,3,7,5,2,3,8]]
2) What step 1 gives us is a one-to-one map from word to index with line breaks
   preserved. Then the rest is straightforward, just map each index back to word.

> decodeLines :: ([String], [Int]) -> [String]
> decodeLines (wordz, indices) = _decodeLines (wordz, breakIntoLines indices)
>
> _decodeLines :: ([String], [[Int]]) -> [String]
> _decodeLines (_, [])       = []
> _decodeLines (wordz, i:is) = line : _decodeLines (wordz, is)
>                              where line = unwords $ map (\e -> wordz !! (e - 1)) i
>
> breakIntoLines :: [Int] -> [[Int]]
> breakIntoLines [] = []
> breakIntoLines indices@(idx:rest)
>   | idx == 0      = breakIntoLines rest
>   | otherwise     = fstLine : breakIntoLines restLines
>                     where (fstLine, restLines) = break (== 0) indices
