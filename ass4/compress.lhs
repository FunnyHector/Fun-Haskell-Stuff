> module Compress where


==============================================
              Question 5 Code
==============================================


> import Data.Map as Map
> import Control.Monad.State

A lexicon is a map where the key is a word and the value is a unique interger as
the corresponding id.

> type Lexicon = Map String Int

A Dict (dictionary) is a Lexicon with a maximum id.

> type Dict = (Int, Lexicon)

compress:
Take a long String, break it down into words (by white space), and compress
words by replacing each one with a unique integer number. Returns a tuple where
the first is a list of integers representing each word, and the second is the
lexicon to map each integer into word.

> compress :: String -> ([Int], Lexicon)
> compress text = (ids, lexicon)
>   where wds                 = words text
>         initState           = (0, empty)
>         (ids, (_, lexicon)) = runState (compress' wds) initState

compress'
Take a list of String, assign each different word with a unique integer number
to represent it in the output, starting from 0 and increasing for each new word,
and use the same number in the output each time the same word appears.

> compress' :: [String] -> State Dict [Int]

> compress' [] = return []

> compress' (wd : wds) = do
>   (maxId, lexi) <- get
>   i             <- getId wd
>
>   if i < 0
>   then do
>     let newLexi = Map.insert wd maxId lexi
>     let newId   = maxId + 1
>     put (newId, newLexi)
>
>     restIds <- compress' wds
>     return (maxId : restIds)
>   else do
>     restIds <- compress' wds
>     return (i : restIds)

getId:
find the id of a given word from dictionary. If this word has not been added in
the dictionary, -1 will be returned.

> getId :: String -> State Dict Int
> getId wd = state $ \dict@(_, lexi) ->
>   case () of
>     _ | member wd lexi -> let (Just i) = Map.lookup wd lexi in (i, dict)
>       | otherwise      -> (-1, dict)


==============================================
            Question 5 Discussion
==============================================


1. Discuss the use of do notation in this version.

-



In this programme, the monad `State (Int, Dict) represents a state in the
process of compressing. The `Int` in the pair is the self-incrementing id, and
the `Dict` is obviously the dictionary/lexicon. If a new word is added into the
dictionary, the id will be incremented by 1.




2. Is the monadic version easier, or harder, than the version from Assignment 1?
Is it clearer or more obscure?

-


I think the answer really depends on how well one can use Monad and do notation
Honestly, I think I still got a very long way to go before I can use Monad with
do notation. The idea behind Monad is still a bit obscure to me,

I still can't write the `compress'` function in this question very fast like a
skilled programmer. But after the `compress'` function is finished, and I read
through it, I found that reading do notation is quite easy and clear. So my
finalised answer is: do notation is clearer to read, but can be harder to write
for a beginer Haskell programmer, like me.




==============================================
            Question 5 Test Cases
==============================================


test_getId:


> test_getId :: Bool
> test_getId = undefined


test_compress':


> test_compress' :: Bool
> test_compress' = undefined



test_compress:


> test_compress :: Bool
> test_compress = undefined



theTest_q5:
One test to test them all!

> theTest_q5 :: Bool
> theTest_q5 = all (== True) [test_getId, test_compress', test_compress]
