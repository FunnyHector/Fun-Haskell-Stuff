module Test where


fun = putStrLn "What's your name?" >>
      getLine >>= \name -> putStrLn ("Hello " ++ name)

f x = do
  return [x,x]

main = do
  x <- f 10
  print x
