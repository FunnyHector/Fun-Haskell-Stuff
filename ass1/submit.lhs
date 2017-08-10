Some functions I used in this assignment are not in standard Prelude. It's pointless
to reinvent wheels. That being said, I did try and include alternative implementations
(i.e. less elagant ways using more basic functions).

N.B. hackage name for `Text.Regex` is `regex-compat`, and for `Text.Regex.PCRE`
`regex-pcre`.

> import Text.Regex    -- cabal install regex-compat
> import Text.Regex.PCRE    -- cabal install regex-pcre
> import Data.List
> import Data.Char

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
                Question 1 part a
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

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

To run the test functions for question 1 part a, simply run `t1a`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

The definition of test functions are listed here, which are simply testing whether
`lineToWords` will pick only words formated as "[A-Za-z0-9]+":

> t1a :: Bool
> t1a = all (== True) [t1a1, t1a2, t1a3]
>
> t1a1 :: Bool
> t1a1 = lineToWords "this is a test" == ["this", "is", "a", "test"]
> t1a2 :: Bool
> t1a2 = lineToWords "For example, this sentence has 7 words!" == ["For","example","this","sentence","has","7","words"]
> t1a3 :: Bool
> t1a3 = lineToWords "!@#$ da^^^^funk!!!!Yay----- .....____....." == ["da", "funk", "Yay"]

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
                Question 1 part b
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

The solution uses the function `lineToWords` defined in part a:

> linesToWords :: [String] -> [String]
> linesToWords lynes = lineToWords $ unwords lynes

To run the test functions for question 1 part b, simply run `t1b`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

The definition of test functions are listed here, which are quite similar to those
from `t1a`. They only differ in input types:

> t1b :: Bool
> t1b = all (== True) [t1b1, t1b2, t1b3]
>
> t1b1 :: Bool
> t1b1 = linesToWords ["this", "is", "a", "test"] == ["this", "is", "a", "test"]
> t1b2 :: Bool
> t1b2 = linesToWords ["For example,", "in this sentence,", "there are 9 words."] == ["For","example","in","this","sentence","there","are","9","words"]
> t1b3 :: Bool
> t1b3 = linesToWords ["!@#$ da^^^^funk!!", "!!Yay----- .....____....."] == ["da", "funk", "Yay"]

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
                Question 1 part c
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

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

To run the test functions for question 1 part c, simply run `t1c`. It contains
several test functions, and should return `False` because `t1c2` can't pass. If
run `t1c1`, `t1c2`, and `t1c3` seperately, `t1c1` and `t1c3` should pass.

Since `posOfWords` does not ignore punctuation characters or whitespaces when
counting positions, so I added extra non-word characters and whitespaces to make
more challenge. From the test result, non-word characters and whitespaces can be
treated correctly.

Also I added repeating words to check if `posOfWords` could preserve the order
of them. Sadly test `t1c2` fails, which means `posOfWords` can't preserve the
order of repeating words (however this is not explicitly required :P)

The definition of test functions are listed here:

> t1c :: Bool
> t1c = all (== True) [t1c1, t1c2, t1c3]
>
> t1c1 :: Bool
> t1c1 = posOfWords ["    this", "   is     a", "test"]
>          == [("this",1,5),("is",2,4),("a",2,11),("test",3,1)]
> t1c2 :: Bool
> t1c2 = posOfWords ["    For example example, example!", "    in this in sentence in,", "there are 9 words."]
>          == [("For",1,5),("example",1,9),("example",1,17),("example",1,26),("in",2,5),("this",2,8),("in",2,13),("sentence",2,16),("in",2,25),("there",3,1),("are",3,7),("9",3,11),("words",3,13)]
> t1c3 :: Bool
> t1c3 = posOfWords ["!@#$ da^^^^funk!!", "!!Yay----- .....____....."]
>          == [("da",1,6),("funk",1,12),("Yay",2,3)]

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
                Question 2 part a
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

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

To run the test functions for question 2 part a, simply run `t2a`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

I also manually tested error-raising case when N <= 1 as I dont know yet how to
test the exception to be thrown as expected. These tests passed.

The definition of test functions are listed here. `t2a1` was taken from tutorial,
`t2a2` was to test the edge case where N == 2, and `t2a3` is using solely '#'
character to match every scenario described in the handout as it makes the output
easy to compare and count the number of.

> t2a :: Bool
> t2a = all (== True) [t2a1, t2a2, t2a3]
>
> t2a1 :: Bool
> t2a1 = wrapLines 11 [
>          "    1234567890        For 12345678901        ",
>          "     example, in this     ",
>          "sentence, there are 9 words.",
>          "Though we have yet to try the maximization",
>          "of this example!"
>        ] == [
>          "1234567890",
>          "For",
>          "12345678901",
>          "example, in",
>          "this",
>          "sentence,",
>          "there are 9",
>          "words.",
>          "Though we",
>          "have yet to",
>          "try the",
>          "maximizati-",
>          "on of this",
>          "example!"
>        ]
> t2a2 :: Bool
> t2a2 = wrapLines 2 ["For example,", "in. this."]
>          == ["F-","or","e-","x-","a-","m-","p-","l-","e,","i-","n.","t-","h-","i-","s."]
> t2a3 :: Bool
> t2a3 = wrapLines 4 ["#","#","##","#","##","##","####","#####","########"]
>          == ["# #","## #","##","##","####","###-","##","###-","###-","##"]

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
                Question 2 part b
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

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
>           availableSpaces                 = n - length str  + numSpaces;
>           x | availableSpaces < numSpaces = 1
>             | otherwise                   = availableSpaces `div` numSpaces
>
> padLeft :: Int -> String -> String
> padLeft n str = whiteSpace ++ str
>                 where whiteSpace = replicate (n - length str) ' '

To run the test functions for question 2 part b, simply run `t2b`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

The definition of test functions are listed here.

> t2b :: Bool
> t2b = all (== True) [t2b1, t2b2]
>
> t2b1 :: Bool
> t2b1 = justifyLines 11 [
>          "    1234567890        For 12345678901        ",
>          "     example, in this     ",
>          "sentence, there are 9 words.",
>          "Though we have yet to try the maximization",
>          "of this example!"
>        ] == [
>          " 1234567890",
>          "        For",
>          "12345678901",
>          "example, in",
>          "       this",
>          "  sentence,",
>          "there are 9",
>          "     words.",
>          "Though   we",
>          "have yet to",
>          "try     the",
>          "maximizati-",
>          " on of this",
>          "example!"
>        ]
> t2b2 :: Bool
> t2b2 = justifyLines 4 ["#","#","##","#","##","##","####","#####","########"]
>          == ["#  #","## #","  ##","  ##","####","###-","  ##","###-","###-","##"]

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
                Question 3 encode
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

Basic idea:
1) Break the input into a list of words (whitspacce not preserved)
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

To run the test functions for question 3 encode, simply run `t3a`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

The definition of test functions are listed here.

> t3a :: Bool
> t3a = all (== True) [t3a1, t3a2, t3a3]
>
> t3a1 :: Bool
> t3a1 = encode ["The more I learn, the more I know.","The more I know, the more I forget."]
>          == (["The","more","I","learn,","the","know.","know,","forget."],[1,2,3,4,5,2,3,6,1,2,3,7,5,2,3,8])
> t3a2 :: Bool
> t3a2 = encode ["     "] == ([],[])
> t3a3 :: Bool
> t3a3 = encode ["haskel haskel haskel haskel haskel"] == (["haskel"],[1,1,1,1,1])

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
                Question 3 decode
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

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

To run the test functions for question 3 encode, simply run `t3b`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

The definition of test functions are listed here.

> t3b :: Bool
> t3b = all (== True) [t3b1, t3b2, t3b3]
>
> t3b1 :: Bool
> t3b1 = decode (["The","more","I","learn,","the","know.","know,","forget."],[1,2,3,4,5,2,3,6,1,2,3,7,5,2,3,8])
>          == "The more I learn, the more I know. The more I know, the more I forget."
> t3b2 :: Bool
> t3b2 = decode (["haskel"],[1,1,1,1,1]) == "haskel haskel haskel haskel haskel"
> t3b3 :: Bool
> t3b3 = decode ([" "], [1,1,1]) == "     "

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
             Question 3 encode lines
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

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

To run the test functions for question 3 encode lines, simply run `t3c`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

The definition of test functions are listed here.

> t3c :: Bool
> t3c = all (== True) [t3c1, t3c2]
>
> t3c1 :: Bool
> t3c1 = encodeLines ["The more I learn, the more I know.","The more I know, the more I forget."]
>          == (["The","more","I","learn,","the","know.","know,","forget."],[1,2,3,4,5,2,3,6,0,1,2,3,7,5,2,3,8])
> t3c2 :: Bool
> t3c2 = encodeLines ["# #","## #","##","##","####","###-","##","###-","###-","##"] == (["#","##","####","###-"],[1,1,0,2,1,0,2,0,2,0,3,0,4,0,2,0,4,0,4,0,2])

-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +
             Question 3 decode lines
-- + -- + -- + -- + -- + -- + -- + -- + -- + -- +

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

To run the test functions for question 3 decode lines, simply run `t3d`. It contains
several test functions, and should return `True` indicating that all test cases
passed.

The definition of test functions are listed here.

> t3d :: Bool
> t3d = all (== True) [t3d1, t3d2]
>
> t3d1 :: Bool
> t3d1 = (decodeLines lexi == lynes) && (decodeLines (encodeLines lynes) == lynes)
>          where lexi = (["The","more","I","learn,","the","know.","know,","forget."],[1,2,3,4,5,2,3,6,0,1,2,3,7,5,2,3,8]);
>                lynes = ["The more I learn, the more I know.","The more I know, the more I forget."]
> t3d2 :: Bool
> t3d2 = (decodeLines lexi == lynes) && (decodeLines (encodeLines lynes) == lynes)
>          where lexi = (["#","##","####","###-"],[1,1,0,2,1,0,2,0,2,0,3,0,4,0,2,0,4,0,4,0,2]);
>                lynes = ["# #","## #","##","##","####","###-","##","###-","###-","##"]
