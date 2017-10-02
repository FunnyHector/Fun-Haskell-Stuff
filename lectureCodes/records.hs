import Control.Monad.State

data Vars = Vars {
  x :: Int,
  y :: String
} deriving Show

startVars = Vars { x = 999, y = "hello" }

setX :: Int -> State Vars ()
setX v = state $ \s -> ((), Vars { x = v, y = y s })

setY :: String -> State Vars ()
setY v = state $ \s -> ((), Vars { x = x s, y = v })

getX :: State Vars Int
getX = state $ \s -> (x s, s)
getY :: State Vars String
getY = state $ \s -> (y s, s)

demo = do a <- getX
          b <- fmap length getY
          setY (show a)
          setX b
