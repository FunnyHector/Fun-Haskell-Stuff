module Langage where

type Prog = [Stmt]

type Var = Char

type Val = Int

data Op = Add | Minus | Multiply | Div deriving Show

data Stmt = Asgn Var Exp | While Exp Prog deriving Show

data Exp = Const Val | Var Var | Bin Op Exp Exp deriving Show

type Store = [(Var, Val)]

run :: Prog -> Store
run prog = exac prog emptyStore

exac :: Prog -> Store -> Store
exac [] store = store
exac (stmt : rest) store = exac rest (exac' stmt store)

exac' :: Stmt -> Store -> Store
exac' (Asgn var ex) store = setVal var (eval ex store) store
exac' stmt@(While con prog) store
  = if eval con store == 0
    then store
    else exac' stmt (exac prog store)

check :: Prog -> [Var]
check prog = checkProg prog [] []

checkProg :: Prog -> [Var] -> [Var] -> [Var]
checkProg [] _ bad = bad
checkProg (Asgn var ex : stmts) asgnd bad
  | null badInExp = checkProg stmts (var : asgnd) bad
  | otherwise     = checkProg stmts asgnd (bad ++ badInExp)
  where badInExp = checkExp ex asgnd
checkProg (While ex prog : stmts) asgnd bad
  = checkExp ex asgnd ++ checkProg prog asgnd bad ++ checkProg stmts asgnd bad

checkExp :: Exp -> [Var] -> [Var]
checkExp (Const _) _ = []
checkExp (Var var) asgnd
  | var `elem` asgnd = []
  | otherwise        = [var]
checkExp (Bin _ e1 e2) asgnd = checkExp e1 asgnd ++ checkExp e2 asgnd

setVal :: Var -> Val -> Store -> Store
setVal = undefined

eval :: Exp -> Store -> Val
eval = undefined

emptyStore :: Store
emptyStore = []
