import Control.Monad.State
import Control.Monad.Reader
import Data.Map as M hiding (lookup, map)

birthdays = do "Kevin" `bornOn` "23 December"
               "Alice" `bornOn` "14 August"
               "Craig" `bornOn` "7 April"

-- Constructs a list of (person, birthday) pairs
bornOn :: String -> String -> State [(String, String)] ()
bornOn name bd = state $ \l -> ((), (name, bd) : l)





-- Using Reader to thread our database through
myQuery = do kevin <- dob "Kevin"
             alice <- dob "Alice"
             return ("Kevin's birthday is " ++ kevin
                     ++ " and Alice's is " ++ alice)

dob name m = let (Just x) = lookup name m in x
-- This just crashes if the name isn't there - what would be better?

-- --------------------------------------------------
-- Build up a graph structure with vertices and edges
-- --------------------------------------------------
type Vertex = Int
type Graph = Map Int [Int]

emptyGraph :: (Vertex, Graph)
emptyGraph = (0, empty)
vertex :: State (Int, Graph) Vertex
vertex = state $ \(counter, g) -> (counter, (counter + 1, insert counter [] g))

connected :: Vertex -> Vertex -> State (Int, Graph) ()
connected x y = state $ \(c, g) -> ((),
        (c, adjust (\l -> y:l) x g))

infix -->
(-->) = connected
infix <-->
(<-->) a b = connected a b >> connected b a

myGraph = do x <- vertex
             y <- vertex
             z <- vertex
             w <- vertex
             x `connected` y
             connected y w
             y --> z
             z <--> x

-- Use e.g. printGraphN2 ["x","y","z","w"] $ getGraph myGraph

-- Given a graph computation like myGraph, gives back just
-- the graph component after running it.
getGraph :: State (Int, Graph) a -> Graph
getGraph c = snd . snd $ flip runState emptyGraph c

-- Prints a human-readable rendition of the graph
printGraph :: Graph -> IO ()
printGraph g = putStr $ concat $
                 concatMap (\(k, l) -> map
                     (\v -> (show k) ++ "-->"
                         ++ (show v) ++ "\n") l)
                   (toAscList g)
-- Same, but takes a list of vertex names first
printGraphN :: [String] -> Graph -> IO ()
printGraphN n g = putStr $ concat $
                    concatMap (\(k, l) -> map
                        (\v -> (n !! k) ++ "-->"
                            ++ (n !! v) ++ "\n") l)
                      (toAscList g)

-- Same, but shows bidirectional links specially,
-- and renders the vertex declarations too.
printGraphN2 :: [String] -> Graph -> IO ()
printGraphN2 n g = putStr $ concat $
                     (map (renderV n) (keys g)) ++
                     concatMap (\(k, l) -> map
                         (\v -> render k v (n!!) g) l)
                       (toAscList g)

-- Renders the connection between two vertices as a string,
-- marking two-way links as <--> for the first appearance
-- only (and omitting them the second time around).
render :: Vertex -> Vertex -> (Vertex -> String) -> Graph -> String
render a b f g = if a > b then "" else
                   let l1 = g ! a
                       l2 = g ! b
                       in
                         if b `elem` l1 && a `notElem` l2
                            then (f a) ++ " -->" ++ (f b) ++ "\n"
                            else (f a) ++ "<-->" ++ (f b) ++ "\n"

renderV :: [String] -> Vertex -> String
renderV n v = (n !! v) ++ " <- vertex\n"
