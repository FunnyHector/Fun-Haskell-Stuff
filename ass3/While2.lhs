-----------------------------------------------------
                 General discussion
-----------------------------------------------------

To dear marker: I strongly suggest skipping While1.lhs and While2.lhs and go
straight to While3.lhs for marking, if that does not violat any marking rules.
Discussions, more refined comments, test cases are all in While3.lhs. I didn't
get time to polish While1.lhs and While2.lhs, plus it's really tedious to do so.

I implemented 3 parts in order, and managed to keep the back-compatibility, so
While3.lhs contains every requirement from part 1 and 2. The implementation of
part 3 is greatly refactored and hence quite different from part 1 and 2 as well.

I also provided a detailed list of what I did and what I didn't do in
While3.lhs.

If needed, While1.lhs is always here for reviewing.

-----------------------------------------------------
-----------------------------------------------------

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

`nub` function is used to remove duplicates

> import Data.List (nub)

Array is used to back up the array implementation in While.

> import Data.Array

Variable names are assumed to be single characters.

> type Var = Char

Values are assumed to be integers/boolean/array.

> data Val = Int Int
>          | Bool Bool
>          | Arr (Array Int Val) Int Type  -- 3 params: array, size, type
>          deriving (Eq, Show)

A program is a symbol table and a list of statements

> type Prog = (SymTab, [Stmt])

A statement can be a skip, assignment, array reference assignment, if, or do
statement.

> data Stmt = Skip
>           | Asgn Var Exp
>           | AsgnArrRef Var Exp Exp
>           | If Exp Prog Prog    -- TODO: should this be Prog or [Stmt]? If we use Prog, we have scoped variables.
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

Unary operation at the moment only contains boolean operation Not (!)

> data UOp = Not deriving (Eq, Show)

Type is the type of a value. We can use it to check whether an expression has
correct type (Int, Bool, or Array).

> data Type = IntType
>           | BoolType
>           | ArrayType Type
>           deriving (Eq, Show)

A store is a map from variables to values

> type Store = Map Var Val

A symbol table is a map from varibales to types

> type SymTab = Map Var Type

A result is like Either, it's either a good result or an error with message

> data Result a = OK a | Err String  -- TODO: not used so far

run:
To run a program with a given initial store, we first do a series of static
checking.

1. The symbol table and the store matches each other.
   1.1. All declared variables are initialised. (dclrdButNotInitVars)
   1.2. All initialised variables are declared. (intiButNotdclrdVars)
   1.3. Types are matched. (typeMismatchedVars)

2. All variables used are declared with a type. (undeclaredVars)

3. All variables used in expression have values initialised. (uninitVars)

4. Array related check:
   4.1. Array variable is not used alone, i.e. without an index. (arrVars)
   4.2. Var in (AsgnArrRef Var Exp Exp) is ArrayType. (badAsgnArrRefs)
   4.3. The first Exp (index expression) in (AsgnArrRef Var Exp Exp) is IntType.
        (badAsgnArrRefs)
   4.4. Var in (ArrRef Var Exp) is ArrayType. (badArrRefs)
   4.5. The Exp in (ArrRef Var Exp) is IntType. (badArrRefs)

5. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)

6. Conditional expressions for If or Do statement are used correctly. This is
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

Can we statically check if an array reference is accessing an uninitialised
element? My guess for this question would also be no. The reason is same as above.
It's just difficult to evaluate an index value out of an expression without
triggering more errors. Aside from that reason, there is one more risk: Haskel
Array throwns exception if we access an element that is not there, and I don't
know how to catch the exception in Haskel yet. An ideal solution would be:

  -- say we have an array `arr`, and it has no element at index 5
  eval (ArrRef arr 5) store
    | exceptionThrown "undefined array element" = NULL
    | otherwise                                 = arr ! 5

This is only to show the idea. Please ignore the totally-wont-work syntax :P
A lot of other languages uses default values for this operation, e.g. in Java
gives back null for array of objects or 0 for array of primitive integers. But
That solution brings lots of controversial discussions as well.

If the programme is all good, then we just pass the programme and store to exec.

> run :: Prog -> Store -> Store
> run p s
>     -- these checks have really bad names, see the where clause for more info
>   | not $ null check11
>       = error ("\nDeclared but not initialised Variable(s): " ++ show check11)
>   | not $ null check12
>       = error ("\nInitialised but not declared Variable(s): " ++ show check12)
>   | not $ null check13
>       = error ("\nMismatched type in store and symbol table: " ++ show check13)
>   | not $ null check2
>       = error ("\nUndeclared Variable(s): " ++ show check2)
>   | not $ null check3
>       = error ("\nUninitialised Variable(s): " ++ show check3)
>   | not $ null check41
>       = error ("\nArray Variable(s) used alone: " ++ show check41)
>   | not $ null check423
>       = error ("\nType errors in array reference assignment(s): " ++ show check423)
>   | not $ null check445
>       = error ("\nType errors in array reference(s): " ++ show check445)
>   | not $ null check5
>       = error ("\nIncorrect Assignment(s): " ++ show check5)
>   | not $ null check6
>       = error ("\nIncorrect Expression(s): " ++ show check6)
>   | otherwise
>       = exec p s -- the non-error scenario
>   where check11  = dclrdButNotInitVars p s    -- check no. 1.1
>         check12  = intiButNotdclrdVars p s    -- check no. 1.2
>         check13  = typeMismatchedVars p s     -- check no. 1.3
>         check2   = undeclaredVars p           -- check no. 2
>         check3   = uninitVars p s             -- check no. 3
>         check41  = arrVars p                  -- check no. 4.1
>         check423 = badAsgnArrRefs p           -- check no. 4.2 & 4.3
>         check445 = badArrRefs p               -- check no. 4.4 & 4.5
>         check5   = incorrectAsgnmts p         -- check no. 5
>         check6   = misConExps p               -- check no. 6

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

1. The symbol table and the store matches each other.
   1.1. All declared variables are initialised. (dclrdButNotInitVars)
   1.2. All initialised variables are declared. (intiButNotdclrdVars)
   1.3. Types are matched. (typeMismatchedVars)

2. All variables used are declared with a type. (undeclaredVars)

3. All variables used in expression have values initialised. (uninitVars)

4. Array related check:
   4.1. Array variable is not used alone, i.e. without an index. (arrVars)
   4.2. Var in (AsgnArrRef Var Exp Exp) is ArrayType. (badAsgnArrRefs)
   4.3. The first Exp (index expression) in (AsgnArrRef Var Exp Exp) is IntType.
        (badAsgnArrRefs)
   4.4. Var in (ArrRef Var Exp) is ArrayType. (badArrRefs)
   4.5. The Exp in (ArrRef Var Exp) is IntType. (badArrRefs)

5. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)

6. Conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer/array values end up as condition expression of If or
   Do. (misConExps)


dclrdButNotInitVars:
Compare the symbol table and the store, and find out all variables that is
declared but not initialised (exists in symbol table but not in store).
(naming is hard....)

> dclrdButNotInitVars :: Prog -> Store -> [Var]
> dclrdButNotInitVars (t, _) s
>   = filter (`notElem` varsStore) varsTable
>     where varsStore = keys s
>           varsTable = keys t

intiButNotdclrdVars:
Compare the symbol table and the store, and find out all variables that is
initialised but not declared (exists in store but not in symbol table).

> intiButNotdclrdVars :: Prog -> Store -> [Var]
> intiButNotdclrdVars (t, _) s
>   = filter (`notElem` varsTable) varsStore
>     where varsStore = keys s
>           varsTable = keys t

typeMismatchedVars:
Compare the symbol table and the store, and find out all variables that have
mismatched type.
If we run the checks in order, we can guarantee that symbol table and store
have same variables now (by same I mean the name of it). We only need to check
the type now.

> typeMismatchedVars :: Prog -> Store -> [Var]
> typeMismatchedVars (table, _) store
>   = keys $ filter (\(k,v) -> not $ isSameType v (getVal k table)) store

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
> arrVars p = nub $ arrVarsProg p

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
> badArrRefs p = nub $ badArrRefsProg p

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
Find out all assignments that have mismatching types on two sides in a programme.

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
>   | otherwise           = error ("Undeclared variable " ++ show v)  -- shouldn't come here if we do static checking before running.

> expType (ArrRef v _) t
>   | hasKey v t          = let (ArrayType tipe) = getVal v t in tipe
>   | otherwise           = error ("Undeclared variable " ++ show v)  -- shouldn't come here if we do static checking before running.

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

isSameType:
A helper function to check if two variables have same types in store and in
symbol table

> isSameType :: Val -> Type -> Bool
> isSameType (Int _)       IntType         = True
> isSameType (Bool _)      BoolType        = True
> isSameType (Arr _ _ tp1) (ArrayType tp2) = tp1 == tp2
> isSameType _             _               = False

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

> s9 :: Store
> s9 = [
>        ('x', Bool True),
>        ('y', Bool False),
>        ('z', Arr arr 4 IntType),
>        ('n', Int 5)
>      ]
>      where arr = array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]

> t9 :: SymTab
> t9 = [
>        ('x', BoolType),
>        ('y', BoolType),
>        -- ('m', IntType),
>        ('n', IntType),
>        ('z', ArrayType IntType)
>        -- ('a', ArrayType BoolType)
>      ]

> p9 :: Prog
> p9 = (
>   t9,
>   [
>     Asgn 'x' (Una Not (Var 'y')),
>     Asgn 'm' (Var 'x'),
>     AsgnArrRef 'z' (ConstInt 0) (Bin Plus (ArrRef 'z' (ConstInt 2)) (ConstInt 10)),
>     If (Bin And (Var 'x') (Var 'y')) (t9, [Asgn 'm' (ConstInt 5)]) (t9, [Asgn 'n' (ConstInt 0)]),
>     Do (Bin Gt (Var 'n') (ConstInt 1)) (t9, [Asgn 'n' (Bin Minus (Var 'n') (ConstInt 1))])
>   ])
