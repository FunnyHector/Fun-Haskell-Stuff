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

nub function is used to remove duplicates

> import Data.List (nub)

Array is used to back up the array implementation in While.

> import Data.Array

Variable names are assumed to be single characters.

> type Var = Char

Value are assumed to be integers/boolean/array.

> data Val = Int Int
>          | Bool Bool
>          | Arr (Array Int Val) Int Type  -- 3 params: array, size, type
>          deriving (Show)

A program is just a list of statements

> type Prog = (SymTab, [Stmt])

A statement can be a skip, assignment, array reference assignment, if, or do
statement.

> data Stmt = Skip
>           | Asgn Var Exp
>           | AsgnArrRef Var Exp Exp
>           | If Exp Prog Prog    -- TODO: should this be Prog or [Stmt]. If we use Prog, we have scoped variables.
>           | Do Exp Prog
>           deriving (Show)

An expression can be a constant(int/bool), a variable, or a binary operator
applied to two expressions, or a unary operator applied to one expression.

> data Exp = ConstInt Int
>          | ConstBool Bool
>          | Var Var
>          | ArrRef Var Exp
>          | Bin BOp Exp Exp
>          | Una UOp Exp
>          deriving (Eq, Show)

An binary operation can be arithmetic (+, -, *, /, ^), relational (=, /=, <,
<=, > or >=), or boolean (&&, ||)

> data BOp = Plus | Minus | Times | Div | Power |
>            Eq | Ne | Lt | Le | Gt | Ge |
>            And | Or
>            deriving (Eq, Show)

A unary operation is just boolean operation Not (!)

> data UOp = Not deriving (Eq, Show)

Type is the type of a value. We can use it to check whether an expression has
correct type (Int, Bool, or Array).

> data Type = IntType
>           | BoolType
>           | ArrayType Type
>           deriving (Eq, Show)

A store is a map from variables to values

> type Store = Map Var Val

A symbol table is a map from varibales to their corresponding types

> type SymTab = Map Var Type

A result is like Either, it's either a good result or an error with message

> data Result a = OK a | Err String  -- TODO: not used so far

run:
To run a program with a given initial store, we first do a series of static
checking.

1. all variables used are declared with a type. (undeclaredVars)
2. all variables used in expression have values initialised. (uninitVars)
3. Array related check:
   3.1. array variable is not used alone, i.e. without an index. (arrVars)
   3.2. Var in (AsgnArrRef Var Exp Exp) is ArrayType. (badAsgnArrRefs)
   3.3. The first Exp (index expression) in (AsgnArrRef Var Exp Exp) is IntType.
        (badAsgnArrRefs)
   3.4. Var in (ArrRef Var Exp) is ArrayType. (badArrRefs)
   3.5. The Exp in (ArrRef Var Exp) is IntType. (badArrRefs)
4. all expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)
5. conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer/array values end up as condition expression of If or
   Do. (misConExps)

These checks are done in order, which eliminates the possibility to blow up bit
by bit as each step goes. Note that during these checks, there still could be
runtime error thrown. In fact, in many statically typed languages, errors are
checked in stages. Some errors have to be fixed before other errors can be
detected.

Can we statically check if an array reference is using an index out of boundary?
Not very feasible, at least we can't if we allow expressions for index. If we
only allow constant int values to be used as index, checking this exception can
be done. In order to check the boundary, we have to evaluate the final value
of the index, but during the evaluation, we can't guarantee that the index
expression does not attempt to evaluate another array reference. In other words,
we can't safely evaluate the index expression in compile time, hence can't check
for index out of boundary exceptions. In fact, in all the statically typed
languages that I know, index out of boundary exceptions are thrown during runtime.

If the programme is all good, then we just pass the programme and store to exec.

> run :: Prog -> Store -> Store
> run p s
>   | not $ null undclrVars
>       = error ("\nUndeclared Variable(s): " ++ show undclrVars)
>   | not $ null uninitvars
>       = error ("\nUninitialised Variable(s): " ++ show uninitvars)
>   | not $ null arrvars
>       = error ("\nArray Variable(s) used alone: " ++ show arrvars)
>   | not $ null badasgnArrRefs
>       = error ("\nType errors in array reference assignment(s): " ++ show badasgnArrRefs)
>   | not $ null badarrRefs
>       = error ("\nType errors in array reference(s): " ++ show badarrRefs)
>   | not $ null badAsgns
>       = error ("\nIncorrect Assignment(s): " ++ show badAsgns)
>   | not $ null misconExps
>       = error ("\nIncorrect Expression(s): " ++ show misconExps)
>   | otherwise
>       = exec p s -- the non-error scenario
>   where undclrVars     = undeclaredVars p
>         uninitvars     = uninitVars p s
>         arrvars        = arrVars p
>         badasgnArrRefs = badAsgnArrRefs p
>         badarrRefs     = badArrRefs p
>         badAsgns       = incorrectAsgnmts p
>         misconExps     = misConExps p

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

> exec' (AsgnArrRef var idxExp valExp) store = setVal var newVal store
>   where (Arr oldArray size tipe) = getVal var store
>         (Int idx)                = eval idxExp store
>         newVal                   = Arr (oldArray // [(idx, eval valExp store)]) size tipe

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

> eval (ConstInt i)  _ = Int i
> eval (ConstBool b) _ = Bool b

> eval (Var v) s
>   | hasKey v s = getVal v s
>   | otherwise  = error ("Undefined variable " ++ [v])  -- shouldn't come here if we do static checking before running.

> eval (ArrRef v idxExp) s
>   | hasKey v s = arr ! idx
>   | otherwise  = error ("Undefined variable " ++ [v])  -- shouldn't come here if we do static checking before running.
>   where (Arr arr _ _) = getVal v s
>         (Int idx)     = eval idxExp s

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
2. all variables used in expression have values initialised. (uninitVars)
3. Array related check:
   3.1. array variable is not used alone, i.e. without an index. (arrVars)
   3.2. Var in (AsgnArrRef Var Exp Exp) is ArrayType. (badAsgnArrRefs)
   3.3. The first Exp (index expression) in (AsgnArrRef Var Exp Exp) is IntType.
        (badAsgnArrRefs)
   3.4. Var in (ArrRef Var Exp) is ArrayType. (badArrRefs)
   3.5. The Exp in (ArrRef Var Exp) is IntType. (badArrRefs)
4. all expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)
5. conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer/array values end up as condition expression of If or
   Do. (misConExps)

undeclaredVars:
Find out all the variables that are used but not declared in a programme.
(This wrapper method get rid of duplicates)

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
>   | hasKey v symTab = rest
>   | otherwise       = v : rest
>   where rest = undeclaredVarsExp (symTab, e)

> undeclaredVarsStmt (symTab, AsgnArrRef var idxExp valExp)
>   | hasKey var symTab = rest
>   | otherwise         = var : rest
>   where rest = undeclaredVarsExp (symTab, idxExp) ++ undeclaredVarsExp (symTab, valExp)

> undeclaredVarsStmt (symTab, If e c p)
>   = undeclaredVarsExp (symTab, e) ++ undeclaredVarsProg c ++ undeclaredVarsProg p

> undeclaredVarsStmt (symTab, Do e p)
>   = undeclaredVarsExp (symTab, e) ++ undeclaredVarsProg p

undeclaredVarsExp:
Find out all the variables that are used but not declared in an expression.

> undeclaredVarsExp :: (SymTab, Exp) -> [Var]

> undeclaredVarsExp (_, ConstInt  _) = []
> undeclaredVarsExp (_, ConstBool _) = []

> undeclaredVarsExp (symTab, Var v)
>   | hasKey v symTab = []
>   | otherwise       = [v]

> undeclaredVarsExp (symTab, ArrRef v idxExp)
>   | hasKey v symTab = rest
>   | otherwise       = v : rest
>   where rest = undeclaredVarsExp (symTab, idxExp)

> undeclaredVarsExp (symTab, Bin _ e e')
>   = undeclaredVarsExp (symTab, e) ++ undeclaredVarsExp (symTab, e')

> undeclaredVarsExp (symTab, Una _ e) = undeclaredVarsExp (symTab, e)

uninitVars:
Given a store, find out all variables used but not initialised in a programme.
(This wrapper method get rid of duplicates)

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

> uninitVarsStmt (AsgnArrRef _ idxExp valExp) store
>   = uninitVarsExp idxExp store ++ uninitVarsExp valExp store

> uninitVarsStmt (If e c p) store
>   = uninitVarsExp e store ++ uninitVarsProg c store ++ uninitVarsProg p store

> uninitVarsStmt (Do e p) store = uninitVarsExp e store ++ uninitVarsProg p store

uninitVarsExp:
Given a store, find out all variables used but not initialised in an expression.

> uninitVarsExp :: Exp -> Store -> [Var]

> uninitVarsExp (ConstInt _) _ = []
> uninitVarsExp (ConstBool _) _ = []

> uninitVarsExp (Var v) store
>   | hasKey v store = []
>   | otherwise      = [v]

> uninitVarsExp (ArrRef v idxExp) store
>   | hasKey v store = rest
>   | otherwise      = v : rest
>   where rest = uninitVarsExp idxExp store

> uninitVarsExp (Bin _ e e') store = uninitVarsExp e store ++ uninitVarsExp e' store

> uninitVarsExp (Una _ e) store = uninitVarsExp e store

arrVars:
Checks whether an array variable is used alone, i.e. without an index.
(This wrapper method get rid of duplicates)

> arrVars :: Prog -> [Var]
> arrVars p = nub $ arrVars p

arrVarsProg:
Checks in programme whether an array variable is used alone, i.e. without an index.

> arrVarsProg :: Prog -> [Var]
> arrVarsProg (_, []) = []
> arrVarsProg (symTab, stmt : stmts)
>   = arrVarsStmt (symTab, stmt) ++ arrVarsProg (symTab, stmts)

arrVarsStmt:
Checks in statement whether an array variable is used alone, i.e. without an index.

> arrVarsStmt :: (SymTab, Stmt) -> [Var]

> arrVarsStmt (_, Skip) = []

> arrVarsStmt (symTab, Asgn _ e) = arrVarsExp (symTab, e)

> arrVarsStmt (symTab, AsgnArrRef _ idxExp valExp)
>   = arrVarsExp (symTab, idxExp) ++ arrVarsExp (symTab, valExp)

> arrVarsStmt (symTab, If e c p)
>   = arrVarsExp (symTab, e) ++ arrVarsProg c ++ arrVarsProg p

> arrVarsStmt (symTab, Do e p)
>   = arrVarsExp (symTab, e) ++ arrVarsProg p

arrVarsExp:
Checks in expression whether an array variable is used alone, i.e. without an index.

> arrVarsExp :: (SymTab, Exp) -> [Var]

> arrVarsExp (_, ConstInt  _) = []
> arrVarsExp (_, ConstBool _) = []

> arrVarsExp (symTab, Var v)
>   | hasKey v symTab && getVal v symTab == IntType  = []
>   | hasKey v symTab && getVal v symTab == BoolType = []
>   | otherwise       = [v]

> arrVarsExp (symTab, ArrRef _ idxExp) = arrVarsExp (symTab, idxExp)

> arrVarsExp (symTab, Bin _ e e')
>   = arrVarsExp (symTab, e) ++ arrVarsExp (symTab, e')

> arrVarsExp (symTab, Una _ e) = arrVarsExp (symTab, e)

badAsgnArrRefs:
Checks whether variables and index expressions have incorrect type in
(AsgnArrRef Var Exp Exp)

> badAsgnArrRefs :: Prog -> [Stmt]

> badAsgnArrRefs (_, []) = []

> badAsgnArrRefs (symTab, s@(AsgnArrRef v idxExp _) : stmts)
>   | hasKey v symTab && (getVal v symTab == IntType)  = s : rest
>   | hasKey v symTab && (getVal v symTab == BoolType) = s : rest
>   | expType idxExp symTab /= IntType = s : rest
>   | otherwise = rest
>   where rest = badAsgnArrRefs (symTab, stmts)


> badAsgnArrRefs (symTab, _ : stmts) = badAsgnArrRefs (symTab, stmts)

badArrRefs:
Checks whether variables and index expressions have incorrect type in
(ArrRef Var Exp)
This wrapper method get rid of duplicates.

> badArrRefs :: Prog -> [Exp]
> badArrRefs p = nub $ badArrRefs p

badArrRefsProg:
Checks whether variables and index expressions have incorrect type in
(ArrRef Var Exp) in a programme

> badArrRefsProg :: Prog -> [Exp]
> badArrRefsProg (_, []) = []
> badArrRefsProg (symTab, stmt : stmts)
>   = badArrRefsStmt (symTab, stmt) ++ badArrRefsProg (symTab, stmts)

badArrRefsStmt:
Checks whether variables and index expressions have incorrect type in
(ArrRef Var Exp) in a Statement

> badArrRefsStmt :: (SymTab, Stmt) -> [Exp]

> badArrRefsStmt (_, Skip) = []

> badArrRefsStmt (symTab, Asgn _ e) = badArrRefsExp (symTab, e)

> badArrRefsStmt (symTab, AsgnArrRef _ idxExp valExp)
>   = badArrRefsExp (symTab, idxExp) ++ badArrRefsExp (symTab, valExp)

> badArrRefsStmt (symTab, If e c p)
>   = badArrRefsExp (symTab, e) ++ badArrRefsProg c ++ badArrRefsProg p

> badArrRefsStmt (symTab, Do e p) = badArrRefsExp (symTab, e) ++ badArrRefsProg p

badArrRefsExp:
Checks whether variables and index expressions have incorrect type in
(ArrRef Var Exp) in an expression

> badArrRefsExp :: (SymTab, Exp) -> [Exp]

> badArrRefsExp (_, ConstInt  _) = []
> badArrRefsExp (_, ConstBool _) = []
> badArrRefsExp (_, Var       _) = []

> badArrRefsExp (symTab, e@(ArrRef v idxExp))
>   | hasKey v symTab && (getVal v symTab == IntType)  = e : rest
>   | hasKey v symTab && (getVal v symTab == BoolType) = e : rest
>   | expType idxExp symTab /= IntType = e : rest
>   | otherwise = rest
>   where rest = badArrRefsExp (symTab, idxExp)

> badArrRefsExp (symTab, Bin _ e e') = badArrRefsExp (symTab, e) ++ badArrRefsExp (symTab, e')

> badArrRefsExp (symTab, Una _ e) = badArrRefsExp (symTab, e)

incorrectAsgnmts:
Find out all assignments that have unmatching types on two sides in a programme.

> incorrectAsgnmts :: Prog -> [Stmt]

> incorrectAsgnmts (_, []) = []

> incorrectAsgnmts (symTab, Skip : stmts) = incorrectAsgnmts (symTab, stmts)

> incorrectAsgnmts (symTab, s@(Asgn v e) : stmts)
>   | hasKey v symTab && (getVal v symTab /= expType e symTab) = s : rest
>   | otherwise = rest
>   where rest = incorrectAsgnmts (symTab, stmts)

> incorrectAsgnmts (symTab, s@(AsgnArrRef v _ valExp) : stmts)
>   | hasKey v symTab && (expType (ArrRef v (ConstInt 0)) symTab /= expType valExp symTab) = s : rest
>   | otherwise = rest
>   where rest = incorrectAsgnmts (symTab, stmts)

> incorrectAsgnmts (symTab, If _ c p : stmts)
>   = incorrectAsgnmts c ++ incorrectAsgnmts p ++ incorrectAsgnmts (symTab, stmts)

> incorrectAsgnmts (symTab, Do _ p : stmts)
>   = incorrectAsgnmts p ++ incorrectAsgnmts (symTab, stmts)

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

> expType (ConstInt  _) _ = IntType
> expType (ConstBool _) _ = BoolType

> expType (Var v) t
>   | hasKey v t          = getVal v t
>   | otherwise           = error ("Undeclared variable " ++ show v)

> expType (ArrRef v _) t
>   | hasKey v t          = let (ArrayType tipe) = getVal v t in tipe
>   | otherwise           = error ("Undeclared variable " ++ show v)

> expType (Bin op x y) t  = binOpType op (expType x t) (expType y t)
> expType (Una op x) t    = unaOpType op (expType x t)

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
> e0 = ConstInt 0
> e1 :: Exp
> e1 = ConstInt 1
> e2 :: Exp
> e2 = ConstBool True
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
> p6 = (t3, [Asgn 'x' (Bin Plus (Var 'x') (ConstInt 1))])
> p7 :: Prog
> p7 = (t3, [Asgn 'x' e1, Asgn 'y' e2,
>       If (Bin Eq (Var 'x') (Var 'y')) ([], [Asgn 'z' e1]) ([], [Asgn 'z' e2])])
> p8 :: Prog
> p8 = (t3, [Asgn 'i' e1, Asgn 's' e0,
>       Do (Bin Lt (Var 'i') (Var 'n')) ([], [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5])])

My test:

> -- TODO: TEST IT!

> t9 :: SymTab
> -- t9 = [('x', BoolType), ('y', BoolType), ('m', IntType), ('n', IntType)]
> t9 = [('x', BoolType), ('y', BoolType), ('m', IntType), ('n', IntType), ('a', ArrayType IntType), ('b', ArrayType BoolType)]

> p9 :: Prog
> p9 = (
>   t9,
>   [Asgn 'x' (Una Not (Var 'y')), Asgn 'y' (Var 'x'),
>     If (Bin And (Var 'x') (Var 'y')) (t9, [Asgn 'm' (ConstInt 5)]) (t9, [Asgn 'n' (ConstInt 0)])])
