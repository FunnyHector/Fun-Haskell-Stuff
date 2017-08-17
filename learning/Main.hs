module Main where

  import Control.Monad

  main = do
    putStrLn "quit the programme? y/n"
    answer <- getLine
    when (answer /= "y") $ do
      putStrLn "not quitting"
      main

    -- if answer /= "y" then do
    --   putStrLn "not quitting"
    --   main
    -- else
    --   return ()
