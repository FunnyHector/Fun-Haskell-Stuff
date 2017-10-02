module Test where

import Prelude hiding (Either, Left, Right)
import Control.Monad

data Either a b = Left a | Right b
    deriving (Show, Eq)

instance Monad (Either e) where
    return = Right
    (Right x) >>= f = f x
    (Left x) >>= f = Left x

data Arith = Add Arith Arith | Div Arith Arith | Val Float deriving (Show)

eval :: Arith -> Either String Float
eval (Val x) = Right x
eval (Add l r) = do x <- eval l
                    y <- eval r
                    return (x + y)
eval (Div l r) = do x <- eval l
                    y <- eval r
                    if y == 0.0 then Left "Division by zero" else return (x / y)

a = Div (Add (Val 5) (Val 2)) (Add (Val (-3)) (Val 3))
b = Div (Add (Val 5) (Val 2)) (Add (Val (-3)) (Val 5))
c = Add (Val 4) (Div (Val 5) (Val 0))











-- Ignore these for now. We'll look at them next week.
instance Functor (Either e) where
    fmap f (Right x) = Right (f x)
    fmap f (Left x) = Left x

instance Applicative (Either e) where
    pure = return
    (<*>) = ap
