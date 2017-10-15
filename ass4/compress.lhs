Name:              Fang Zhao (300364061)
Course Number:     COMP304
Assignment Number: 4
Question Number:   5


==============================================
              Question 5 Code
==============================================


> module Compress where

> import qualified Data.Map as Map
> import Control.Monad.State

A lexicon is a map where the key is a word and the value is a unique interger as
the corresponding id.

> type Lexicon = Map.Map String Int

A Dict (dictionary) is a Lexicon with a size count. Should have a better name :P

> type Dict = (Int, Lexicon)

compress:
Take a long String, break it down into words by white spaces, and compress
words by replacing each one with a unique integer number. Returns a tuple where
the first is a list of integers representing each word, and the second is the
lexicon to map each integer into word.

> compress :: String -> ([Int], Lexicon)
> compress text = (ids, lexicon)
>   where wds                 = words text
>         initState           = (0, Map.empty)
>         (ids, (_, lexicon)) = runState (compress' wds) initState

compress'
Take a list of String, assign each different word with a unique integer number
to represent it in the output, starting from 0 and increasing for each new word,
and use the same number in the output each time the same word appears.
`copress'` function uses Monad `State Dict`, which represents a state in the
process of compressing. A `Dict` is a `(Int, Lexicon)` pair, where the `Int` in
is the self-incrementing id, and the `Dict` is obviously the dictionary/lexicon.
If a new word is added into the dictionary, the id will be incremented by 1.

> compress' :: [String] -> State Dict [Int]

> compress' [] = return []

> compress' (wd : wds) = do
>   (size, lexi) <- get
>   i            <- getId wd
>
>   if i < 0
>   then do
>     let newLexi = Map.insert wd size lexi
>     let newSize = size + 1
>     put (newSize, newLexi)
>
>     restIds <- compress' wds
>     return (size : restIds)
>   else do
>     restIds <- compress' wds
>     return (i : restIds)

getId:
find the id of a given word from dictionary. If this word has not been added in
the dictionary, -1 will be returned.

> getId :: String -> State Dict Int
> getId wd = state $ \dict@(_, lexi) ->
>   case () of
>     _ | Map.member wd lexi -> let (Just i) = Map.lookup wd lexi in (i, dict)
>       | otherwise          -> (-1, dict)


==============================================
            Question 5 Discussion
==============================================


1. Discuss the use of do notation in this version.

- In this programme, the monad `State Dict` represents a state in the process of
compressing. A `Dict` is a `(Int, Lexicon)` pair, where the `Int` in the pair is
the self-incrementing id, and the `Dict` is obviously the dictionary/lexicon. If
a new word is added into the dictionary, the id will be incremented by 1.

For function `getId`, the value in that monadic context is the id of the word in
the lexicon. And for function `compress'`, the value in that monadic context is
the list of replacing ids so far.

The use of do notation keeps us from thinking about the contextual noise of
State, so we can focus more on what the programme should do inside that context.

2. Is the monadic version easier, or harder, than the version from Assignment 1?
Is it clearer or more obscure?

- I think the answer really depends on how well one can use Monad and do
notation, and how thorough one can think through the State Monad in the
question. Honestly, I think I still got a very long way to go before I can use
Monad and do notation with ease.

The idea behind Monad is still a bit obscure to me. I still can't write the
`compress'` function in this question very fast like a skilled programmer. But
after the `compress'` function is finished, and when I read through it, I found
that reading do notation is quite easy and clear. So my finalised answer is: do
notation is clearer to read, but can be harder to write for a beginer Haskell
programmer, like me.


==============================================
            Question 5 Test Cases
==============================================


test_getId:
Test both cases of whether or not we can find the word.

> test_getId :: Bool
> test_getId = all (== True) [t1, t2, t3]
>   where t1 = runState (getId "a") mockState == (0, mockState)
>         t2 = runState (getId "b") mockState == (1, mockState)
>         t3 = runState (getId "x") mockState == (-1, mockState)
>         mockState = (4, Map.fromList [("a", 0), ("b", 1), ("c", 2), ("d", 3)])

test_compress':
Test on cases of adding a new word, and adding existing word.

> test_compress' :: Bool
> test_compress' = all (== True) [t1, t2, t3, t4, t5]
>   where t1 = runState (compress' []) mockState == ([], mockState)
>         t2 = runState (compress' ["a"]) mockState == ([0], mockState)
>         t3 = runState (compress' ["a", "d", "c"]) mockState == ([0, 3, 2], mockState)
>         t4 = runState (compress' ["e"]) mockState == ([4], mockState_e)
>         t5 = runState (compress' ["e", "a", "e", "b"]) mockState == ([4, 0, 4, 1], mockState_e)
>         mockState   = (4, Map.fromList [("a", 0), ("b", 1), ("c", 2), ("d", 3)])
>         mockState_e = (5, Map.fromList [("a", 0), ("b", 1), ("c", 2), ("d", 3), ("e", 4)])

test_compress:
This test is simply a behaviour test to see whether compress gives back a
expected result.

> test_compress :: Bool
> test_compress = all (== True) [t1, t2, t3]
>   where t1 = compress "" == ([], Map.empty)
>         t2 = compress "See Spot See Spot run Run Spot run" == ([0, 1, 0, 1, 2, 3, 1, 2], Map.fromList [("Run", 3), ("See", 0), ("Spot", 1), ("run", 2)])
>         t3 = compress "quick fox jumps over the lazy dog" == ([0, 1, 2, 3, 4, 5, 6], Map.fromList [("quick", 0), ("fox", 1), ("jumps", 2), ("over", 3), ("the", 4), ("lazy", 5), ("dog", 6)])

theTest_q5:
One test to test them all!

> theTest_q5 :: Bool
> theTest_q5 = all (== True) [test_getId, test_compress', test_compress]
