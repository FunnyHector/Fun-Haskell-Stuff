Initial code for COMP304 Assignment 3, 2017.

Author: Lindsay Groves, VUW, 2017.

This is an interpreter for a simple while program language, as presented in
lectures.  The assignment asks you to make several extensions to the language.

You run a program using the run function, which takes a program and an
initial store and returns the store resulting from executing the program, if
it executes successfully.

There are some example programs and example stores at the end of this file, so
you can try running some simple tests, e.g. run p1 s1 runs program p1 with
store s1 (which doesn't do much!).

> module While where

Map is used to implement the store.

> import Map

> import Data.List (nub)

Variable names are assumed to be single characters.

> type Var = Char

Value are assumed to be integers.

> data Val = Int Int | Bool Bool deriving (Show)

A program is just a list of statements

> type Prog = (SymTab, [Stmt])

A statement can be a skip, assignment, if or do statement.

> data Stmt = Skip
>           | Asgn Var Exp
>           | If Exp Prog Prog    -- TODO: should this be Prog or [Stmt]. If we use Prog, we have scoped variables.
>           | Do Exp Prog
>           deriving (Show)

An expression can be a constant, a variable, or a binary operator applied to
two expressions

> data Exp = Const Val
>          | Var Var
>          | Bin BOp Exp Exp
>          | Una UOp Exp
>          deriving (Show)

An binary operation can be arithmetic (+, -, *, /, ^), relational (=, /=, <,
<=, > or >=), or boolean (&&, ||)

> data BOp = Plus | Minus | Times | Div | Power |
>            Eq | Ne | Lt | Le | Gt | Ge |
>            And | Or
>            deriving (Eq, Show)

A unary operation is just boolean operation Not (!)

> data UOp = Not deriving (Eq, Show)

Type is used to check whether an expression has correct type (Int or Bool)

> data Type = IntType | BoolType deriving (Eq, Show)

A store is a map from variables to values

> type Store = Map Var Val

A symbol table is a map from varibales to their corresponding types

> type SymTab = Map Var Type

A result is like Either, it's either a good result or an error with message

> data Result a = OK a | Err String

run:
To run a program with a given initial store, we first statically check:

1. all variables used are declared with a type. (undeclaredVars)
2. all expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)
3. all variables used in expression have values initialised. (uninitVars)
4. conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer values end up as condition expression of If or Do.
   (misConExps)

Note that during these checks, there could be runtime error thrown. In other
words, errors are checked in stages. Some errors have to be fixed before other
errors can be detected. This is actually often seen in other static typed
languages.

If the programme is all good, then we just pass the program and store to exec.

> run :: Prog -> Store -> Store
> run p s
>   | allGood   = exec p s  -- the non-error scenario
>   | otherwise = error (errorMsg undclrVars badAsgns uninitvars misconExps)
>   where undclrVars = undeclaredVars p
>         badAsgns   = incorrectAsgnmts p
>         uninitvars = uninitVars p s
>         misconExps = misConExps p
>         allGood    = null undclrVars && null badAsgns && null uninitvars && null misconExps

errorMsg:
This is an ugly ugly string concatenation to put together all the error messages.

> errorMsg :: [Var] -> [Stmt] -> [Var] -> [Exp] -> String
> errorMsg undclrVars badAsgns uninitvars misconExps
>   = undclrVarsMsg ++ badAsgnsMsg ++ uninitvarsMsg ++ misconExpsMsg
>     where undclrVarsMsg = if not $ null undclrVars
>                           then "\nUndeclared Variable(s): " ++ show undclrVars
>                           else ""
>           badAsgnsMsg   = if not $ null badAsgns
>                           then "\nIncorrect Assignment(s): " ++ show badAsgns
>                           else ""
>           uninitvarsMsg = if not $ null uninitvars
>                           then "\nUninitialised Variable(s): " ++ show uninitvars
>                           else ""
>           misconExpsMsg = if not $ null misconExps
>                           then "\nIncorrect Expression(s): " ++ show misconExps
>                           else ""

exec:
To execute a program, we just execute each statement in turn, passing the
resulting state to the next statement at each step.

> exec :: Prog -> Store -> Store
> exec (_, []) store            = store
> exec (vars, stmt : rest) store = exec (vars, rest) (exec' stmt store)

exec':
Execute a single statement, according to its semantics

> exec' :: Stmt -> Store -> Store

> exec' Skip store = store

> exec' (Asgn var expression) store = setVal var (eval expression store) store

> exec' (If cond thenPart elsePart) store =
>   if b
>   then exec thenPart store
>   else exec elsePart store
>   where Bool b = eval cond store

> exec' (Do cond body) store =
>   if not b
>   then store
>   else exec' (Do cond body) (exec body store)
>   where Bool b = eval cond store

eval:
Evaluate an expression, according to its type

> eval :: Exp -> Store -> Val
> eval (Const n) _ = n
> eval (Var v) s
>   | hasKey v s = getVal v s
>   | otherwise  = error ("Undefined variable " ++ [v])  -- shouldn't come here
> eval (Bin op x y) s = applyBin op (eval x s) (eval y s)
> eval (Una op x)   s = applyUna op (eval x s)

applyBin:
Apply an binary arithmetic/relational/boolean operator

> applyBin :: BOp -> Val -> Val -> Val

> applyBin Plus  (Int x)  (Int y)  = Int (x + y)
> applyBin Minus (Int x)  (Int y)  = Int (x - y)
> applyBin Times (Int x)  (Int y)  = Int (x * y)
> applyBin Div   (Int x)  (Int y)  = Int (x `div` y)
> applyBin Power (Int x)  (Int y)  = Int (x ^ y)

> applyBin Eq    (Int x)  (Int y)  = Bool (x == y)
> applyBin Ne    (Int x)  (Int y)  = Bool (x /= y)
> applyBin Lt    (Int x)  (Int y)  = Bool (x < y)
> applyBin Le    (Int x)  (Int y)  = Bool (x <= y)
> applyBin Gt    (Int x)  (Int y)  = Bool (x > y)
> applyBin Ge    (Int x)  (Int y)  = Bool (x >= y)

> applyBin And   (Bool x) (Bool y) = Bool (x && y)
> applyBin Or    (Bool x) (Bool y) = Bool (x || y)

> applyBin op    _        _        = error ("Illegal arguments to " ++ show op)

applyUna:
Apply an Unary boolean operator (i.e. Not)

> applyUna :: UOp -> Val -> Val
> applyUna Not (Bool x) = Bool (not x)
> applyUna op  _        = error ("Illegal argument to " ++ show op)

-----------------------------------------------------
             Functions for static checking
-----------------------------------------------------

TODO: explain the approach I choose, whether uninitialised variables can be
detected statically or in runtime.

What we need to statically check:
1. all variables used are declared with a type. (undeclaredVars)
2. all expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)
3. all variables used in expression have values initialised. (uninitVars)
4. conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer values end up as condition expression of If or Do.
   (misConExps)

undeclaredVars:
Find out all the variables that are used but not declared in a programme.
(This wrapper method get rid of duplicate)

> undeclaredVars :: Prog -> [Var]
> undeclaredVars p = nub $ undeclaredVarsProg p

undeclaredVarsProg:
Find out all the variables that are used but not declared in a programme.

> undeclaredVarsProg :: Prog -> [Var]
> undeclaredVarsProg (_, []) = []
> undeclaredVarsProg (symTab, stmt : stmts)
>   = undeclaredVarsStmt (symTab, stmt) ++ undeclaredVarsProg (symTab, stmts)

undeclaredVarsStmt:
Find out all the variables that are used but not declared in a statement.

> undeclaredVarsStmt :: (SymTab, Stmt) -> [Var]

> undeclaredVarsStmt (_, Skip) = []

> undeclaredVarsStmt (symTab, Asgn v e)
>   | hasKey v symTab = undeclaredVarsExp (symTab, e)
>   | otherwise       = v : undeclaredVarsExp (symTab, e)

> undeclaredVarsStmt (symTab, If e c p)
>   = undeclaredVarsExp (symTab, e) ++ undeclaredVarsProg c ++ undeclaredVarsProg p

> undeclaredVarsStmt (symTab, Do e p)
>   = undeclaredVarsExp (symTab, e) ++ undeclaredVarsProg p

undeclaredVarsExp:
Find out all the variables that are used but not declared in an expression.

> undeclaredVarsExp :: (SymTab, Exp) -> [Var]

> undeclaredVarsExp (_, Const _) = []

> undeclaredVarsExp (symTab, Var v)
>   | hasKey v symTab = []
>   | otherwise       = [v]

> undeclaredVarsExp (symTab, Bin _ e e')
>   = undeclaredVarsExp (symTab, e) ++ undeclaredVarsExp (symTab, e')

> undeclaredVarsExp (symTab, Una _ e) = undeclaredVarsExp (symTab, e)

incorrectAsgnmts:
Find out all assignments that have unmatching types on two sides in a programme.

> incorrectAsgnmts :: Prog -> [Stmt]

> incorrectAsgnmts (_, []) = []

> incorrectAsgnmts (symTab, Asgn v e : stmts)
>   | hasKey v symTab && (getVal v symTab /= expType e symTab) = Asgn v e : incorrectAsgnmts (symTab, stmts)
>   | otherwise = incorrectAsgnmts (symTab, stmts)

> incorrectAsgnmts (symTab, If _ c p : stmts)
>   = incorrectAsgnmts c ++ incorrectAsgnmts p ++ incorrectAsgnmts (symTab, stmts)

> incorrectAsgnmts (symTab, Do _ p : stmts)
>   = incorrectAsgnmts p ++ incorrectAsgnmts (symTab, stmts)

> incorrectAsgnmts (symTab, _ : stmts) = incorrectAsgnmts (symTab, stmts)

uninitVars:
Given a store, find out all variables used but not initialised in a programme.
(This wrapper method get rid of duplicate)

> uninitVars :: Prog -> Store -> [Var]
> uninitVars p s = nub $ uninitVarsProg p s

uninitVarsProg:
Given a store, find out all variables used but not initialised in a programme.

> uninitVarsProg :: Prog -> Store -> [Var]
> uninitVarsProg (_, []) _ = []
> uninitVarsProg (symTab, stmt : stmts) store
>   = uninitVarsStmt stmt store ++ uninitVarsProg (symTab, stmts) store

uninitVarsStmt:
Given a store, find out all variables used but not initialised in a statement.

> uninitVarsStmt :: Stmt -> Store -> [Var]

> uninitVarsStmt Skip _ = []

> uninitVarsStmt (Asgn _ e) store = uninitVarsExp e store

> uninitVarsStmt (If e c p) store
>   = uninitVarsExp e store ++ uninitVarsProg c store ++ uninitVarsProg p store

> uninitVarsStmt (Do e p) store = uninitVarsExp e store ++ uninitVarsProg p store

uninitVarsExp:
Given a store, find out all variables used but not initialised in an expression.

> uninitVarsExp :: Exp -> Store -> [Var]

> uninitVarsExp (Const _) _ = []

> uninitVarsExp (Var v) store
>   | hasKey v store = []
>   | otherwise      = [v]

> uninitVarsExp (Bin _ e e') store = uninitVarsExp e store ++ uninitVarsExp e' store

> uninitVarsExp (Una _ e) store = uninitVarsExp e store

misConExps:
Find out all the places where integer valued expressions are used as condition
in If or Do.

> misConExps :: Prog -> [Exp]

> misConExps (_, []) = []

> misConExps (symTab, If e c p : stmts)
>   | expType e symTab /= BoolType = e : rest
>   | otherwise                    = rest
>   where rest = misConExps c ++ misConExps p ++ misConExps (symTab, stmts)

> misConExps (symTab, Do e p : stmts)
>   | expType e symTab /= BoolType = e : rest
>   | otherwise                    = rest
>   where rest = misConExps p ++ misConExps (symTab, stmts)

> misConExps (symTab, _ : stmts) = misConExps (symTab, stmts)

expType:
Find out the type of an expression

> expType :: Exp -> SymTab -> Type
> expType (Const (Int _)) _  = IntType
> expType (Const (Bool _)) _ = BoolType
> expType (Var v) t
>   | hasKey v t             = getVal v t
>   | otherwise              = error ("Undeclared variable " ++ show v)
> expType (Bin op x y) t     = binOpType op (expType x t) (expType y t)
> expType (Una op x) t       = unaOpType op (expType x t)

binOpType:
Find out the type of an binary operation

> binOpType :: BOp -> Type -> Type -> Type
> binOpType op IntType IntType
>   | op `elem` [Plus, Minus, Times, Div, Power] = IntType
> binOpType op IntType IntType
>   | op `elem` [Eq, Ne, Lt, Le, Gt, Ge] = BoolType
> binOpType op BoolType BoolType
>   | op `elem` [Eq, Ne, And, Or] = BoolType
> binOpType _ _ _ = error "Illegal application"

unaOpType:
Find out the type of an unary operation

> unaOpType :: UOp -> Type -> Type
> unaOpType _ BoolType = BoolType
> unaOpType _ _        = error "Illegal application"

====================================================

Some sample expressions

> e0 :: Exp
> e0 = Const (Int 0)
> e1 :: Exp
> e1 = Const (Int 1)
> e2 :: Exp
> e2 = Const (Int 2)
> e3 :: Exp
> e3 = Var 'x'
> e4 :: Exp
> e4 = Bin Plus e3 e1
> e5 :: Exp
> e5 = Bin Plus (Var 'i') e1

Some sample stores

> s1 :: Store
> s1 = []
> s2 :: Store
> s2 = [('x', Int 1)]
> s3 :: Store
> s3 = [('x', Int 1), ('y', Int 2)]
> s4 :: Store
> s4 = [('x', Bool True), ('y', Bool False), ('z', Bool True)]

some sample symbol tables

> t1 :: SymTab
> t1 = [('x', IntType)]
> t2 :: SymTab
> t2 = [('x', BoolType)]
> t3 :: SymTab
> t3 = [('x', IntType), ('y', IntType)]
> t4 :: SymTab
> t4 = [('x', BoolType), ('y', IntType)]

Some sample programs

> p1 :: Prog
> p1 = (t1, [Skip])
> p2 :: Prog
> p2 = (t1, [Skip, Skip])
> p3 :: Prog
> p3 = (t1, [Asgn 'x' e1])
> p4 :: Prog
> p4 = (t1, [Asgn 'x' (Var 'x')])
> p5 :: Prog
> p5 = (t3, [Asgn 'x' (Var 'y')])
> p6 :: Prog
> p6 = (t3, [Asgn 'x' (Bin Plus (Var 'x') (Const (Int 1)))])
> p7 :: Prog
> p7 = (t3, [Asgn 'x' e1, Asgn 'y' e2,
>       If (Bin Eq (Var 'x') (Var 'y')) ([], [Asgn 'z' e1]) ([], [Asgn 'z' e2])])
> p8 :: Prog
> p8 = (t3, [Asgn 'i' e1, Asgn 's' e0,
>       Do (Bin Lt (Var 'i') (Var 'n')) ([], [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5])])

My test:

> t9 :: SymTab
> -- t9 = [('x', BoolType), ('y', BoolType), ('m', IntType), ('n', IntType)]
> t9 = [('x', BoolType), ('y', BoolType), ('m', IntType), ('n', IntType)]

> p9 :: Prog
> p9 = (
>   t9,
>   [Asgn 'x' (Una Not (Var 'y')), Asgn 'y' (Var 'x'),
>     If (Bin And (Var 'x') (Var 'y')) (t9, [Asgn 'm' (Const (Int 5))]) (t9, [Asgn 'n' (Const (Int 0))])])
