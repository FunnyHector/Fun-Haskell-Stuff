Initial code for COMP304 Assignment 3, 2017.

Author: Lindsay Groves, VUW, 2017.

This is an interpreter for a simple while program language, as presented in
lectures.  The assignment asks you to make several extensions to the language.

You run a program using the run function, which takes a program and an initial
variable store and returns the variable store resulting from executing the
program, if it executes successfully.

There are some example programs and example stores at the end of this file, so
you can try running some simple tests, e.g. run p1 s1 runs program p1 with
variable store s1 (which doesn't do much!).

> module While where

Map is used to implement the store and table.

> import Map

`nub` function is used to remove duplicates

> import Data.List (nub)

Array is used to back up the array implementation in While.

> import Data.Array

Variable names are assumed to be single characters.

> type Var = Char

Procedure names are assumed to be strings

> type PrcdrName = String

Values are assumed to be integers/boolean/array.

> data Val = Int Int
>          | Bool Bool
>          | Arr (Array Int Val) Int Type  -- 3 params: array, size, type
>          deriving (Eq, Show)

A procedure is a list of statements, and a symbol table to define the type of
parameters. When invoking a procedure, often some parameters are passed in. A
procedure can be invoked using the name.

> type Prcdr = (SymTab, [Stmt])

A program is a symbol table, a procedure store, and a list of statements

> type Prog = (SymTab, PrcdrStore, [Stmt])

A statement can be a skip, assignment, array reference assignment, procedure
invocation, if, or do statement.

> data Stmt = Skip
>           | Asgn Var Exp
>           | AsgnArrRef Var Exp Exp
>           | InvokePrcdr PrcdrName VarStore
>           | If Exp Prog Prog    -- TODO: should this be Prog or [Stmt]? If we use Prog, we have scoped variables.
>           | Do Exp Prog
>           deriving (Eq, Show)

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

A variable store is a map from variables to values

> type VarStore = Map Var Val

A procedure store is a map from procedure name to procedures

> type PrcdrStore = Map PrcdrName Prcdr

A symbol table is a map from varibales to types

> type SymTab = Map Var Type

A result is like Either, it's either a good result or an error with message

> data Result a = OK a | Err String  -- TODO: not used so far

run:
To run a program with a given initial var store, we first do a series of static
checking.

1. The symbol table and the variable store matches each other.
   1.1. All declared variables are initialised. (dclrdButNotInitVars)
   1.2. All initialised variables are declared. (intiButNotdclrdVars)
   1.3. Types are matched. (typeMismatchedVars)

2. All procedure being invoked are defined in procedure store. (undefinedPrcdrs)

3. All variables used are declared with a type. (undeclaredVars)

4. All variables used in expression have values initialised. (uninitVars)

5. The parameters passed in matches the defined type. (incorrectParamInvocs)
   This check is essentially check no. 1 applied on procedures.

6. Array related check:
   6.1. Array variable is not used alone, i.e. without an index. (arrVars)
   6.2. Var in (AsgnArrRef Var Exp Exp) is ArrayType. (badAsgnArrRefs)
   6.3. The first Exp (index expression) in (AsgnArrRef Var Exp Exp) is IntType.
        (badAsgnArrRefs)
   6.4. Var in (ArrRef Var Exp) is ArrayType. (badArrRefs)
   6.5. The Exp in (ArrRef Var Exp) is IntType. (badArrRefs)

7. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)

8. Conditional expressions for If or Do statement are used correctly. This is
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
  eval (ArrRef arr 5) vStore
    | exceptionThrown "undefined array element" = NULL
    | otherwise                                 = arr ! 5

This is only to show the idea. Please ignore the totally-wont-work syntax :P
A lot of other languages uses default values for this operation, e.g. in Java
gives back null for array of objects or 0 for array of primitive integers. But
That solution brings lots of controversial discussions as well.

If the programme is all good, then we just pass the programme and variable store
to exec.

> run :: Prog -> VarStore -> VarStore
> run p s
>     -- these checks have really bad names, see the where clause for more info
>   | not $ null check11
>       = error ("\nDeclared but not initialised Variable(s): " ++ show check11)
>   | not $ null check12
>       = error ("\nInitialised but not declared Variable(s): " ++ show check12)
>   | not $ null check13
>       = error ("\nMismatched type in variable store and symbol table: " ++ show check13)
>   | not $ null check2
>       = error ("\nUndefined procedure(s): " ++ show check2)
>   | not $ null check3
>       = error ("\nUndeclared Variable(s): " ++ show check3)
>   | not $ null check4
>       = error ("\nUninitialised Variable(s): " ++ show check4)
>   | not $ null check5
>       = error ("\nProcedure(s) invoked with incorrect parameters: " ++ show check5)
>   | not $ null check61
>       = error ("\nArray Variable(s) used alone: " ++ show check61)
>   | not $ null check623
>       = error ("\nType errors in array reference assignment(s): " ++ show check623)
>   | not $ null check645
>       = error ("\nType errors in array reference(s): " ++ show check645)
>   | not $ null check7
>       = error ("\nIncorrect Assignment(s): " ++ show check7)
>   | not $ null check8
>       = error ("\nIncorrect Expression(s): " ++ show check8)
>   | otherwise
>       = exec p s -- the non-error scenario
>   where check11  = dclrdButNotInitVars p s    -- check no. 1.1
>         check12  = intiButNotdclrdVars p s    -- check no. 1.2
>         check13  = typeMismatchedVars p s     -- check no. 1.3
>         check2   = undefinedPrcdrs p          -- check no. 2
>         check3   = undeclaredVars p           -- check no. 3
>         check4   = uninitVars p s             -- check no. 4
>         check5   = incorrectParamInvocs p     -- check no. 5
>         check61  = arrVars p                  -- check no. 6.1
>         check623 = badAsgnArrRefs p           -- check no. 6.2 & 6.3
>         check645 = badArrRefs p               -- check no. 6.4 & 6.5
>         check7   = incorrectAsgnmts p         -- check no. 7
>         check8   = misConExps p               -- check no. 8

exec:
To execute a program, we just execute each statement in turn, passing the
resulting state to the next statement at each step.

> exec :: Prog -> VarStore -> VarStore
> exec (_, _, []) vStore = vStore
> exec (vars, pStore, stmt : rest) vStore
>   = exec (vars, pStore, rest) (exec' stmt vStore pStore)

exec':
Execute a single statement, according to its semantics

> exec' :: Stmt -> VarStore -> PrcdrStore -> VarStore

> exec' Skip vStore _ = vStore

> exec' (Asgn var expression) vStore _ = setVal var (eval expression vStore) vStore

> exec' (AsgnArrRef var idxExp valExp) vStore _ = setVal var newVal vStore
>   where (Arr oldArray size tipe) = getVal var vStore
>         (Int idx)                = eval idxExp vStore
>         newVal                   = Arr (oldArray // [(idx, eval valExp vStore)]) size tipe

to execute a procedure, we merge parameters into global variable stores, and run
the procedure as a programme. After the procedure is finished, we remove
variables that were parameters, because local variables shouldn't go global.

> exec' (InvokePrcdr prc params) vStore pStore
>   = restoreShadowedParams postPrcdrVarStore vStore params
>     where (paramTab, stmts) = getVal prc pStore
>           mergedVStore      = merge vStore params
>           postPrcdrVarStore = exec (paramTab, pStore, stmts) mergedVStore

> exec' (If cond thenPart elsePart) vStore _ =
>   if b
>   then exec thenPart vStore
>   else exec elsePart vStore
>   where Bool b = eval cond vStore

> exec' (Do cond body) vStore pStore =
>   if not b
>   then vStore
>   else exec' (Do cond body) (exec body vStore) pStore
>   where Bool b = eval cond vStore

restoreShadowedParams:
TL'DR: deal with local variables after executing the procedure.

After executing a procedure, the store will be changed. This method restores
global variables that were shadowed by parameters during executing the procedure,
and remove local variables that were in parameters but not in original store.

> restoreShadowedParams :: Eq a => Map a b -> Map a b -> Map a b -> Map a b
> restoreShadowedParams [] _ _ = []
> restoreShadowedParams ((k,v) : pairs) originStore params
>   | hasKey k originStore && hasKey k params       = (k, getVal k originStore) : restoreShadowedParams pairs originStore params
>   | hasKey k originStore && not (hasKey k params) = (k, v) : restoreShadowedParams pairs originStore params
>   | not (hasKey k originStore) && hasKey k params = restoreShadowedParams pairs originStore params
>   | otherwise                                     = error "post-procedure store should not have more variables than original store"



eval:
Evaluate an expression, according to its type

> eval :: Exp -> VarStore -> Val

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

1. The symbol table and the variable store matches each other.
   1.1. All declared variables are initialised. (dclrdButNotInitVars)
   1.2. All initialised variables are declared. (intiButNotdclrdVars)
   1.3. Types are matched. (typeMismatchedVars)

2. All procedure being invoked are defined in procedure store. (undefinedPrcdrs)

3. All variables used are declared with a type. (undeclaredVars)

4. All variables used in expression have values initialised. (uninitVars)

5. The parameters passed in matches the defined type. (incorrectParamInvocs)
   This check is essentially check no. 1 applied on procedures.

6. Array related check:
   6.1. Array variable is not used alone, i.e. without an index. (arrVars)
   6.2. Var in (AsgnArrRef Var Exp Exp) is ArrayType. (badAsgnArrRefs)
   6.3. The first Exp (index expression) in (AsgnArrRef Var Exp Exp) is IntType.
        (badAsgnArrRefs)
   6.4. Var in (ArrRef Var Exp) is ArrayType. (badArrRefs)
   6.5. The Exp in (ArrRef Var Exp) is IntType. (badArrRefs)

7. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. (incorrectAsgnmts)

8. Conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer/array values end up as condition expression of If or
   Do. (misConExps)


dclrdButNotInitVars:
Compare the symbol table and the variable store, and find out all variables that
is declared but not initialised (exists in symbol table but not in variable
store).
(naming is hard....)

> dclrdButNotInitVars :: Prog -> VarStore -> [Var]
> dclrdButNotInitVars (t, _, _) s
>   = filter (`notElem` varsStore) varsTable
>     where varsStore = keys s
>           varsTable = keys t

intiButNotdclrdVars:
Compare the symbol table and the variable store, and find out all variables that
is initialised but not declared (exists in variable store but not in symbol
table).

> intiButNotdclrdVars :: Prog -> VarStore -> [Var]
> intiButNotdclrdVars (t, _, _) s
>   = filter (`notElem` varsTable) varsStore
>     where varsStore = keys s
>           varsTable = keys t

typeMismatchedVars:
Compare the symbol table and the variable store, and find out all variables that
have mismatched type.
If we run the checks in order, we can guarantee that symbol table and variable
store have same variables now (by same I mean the name of it). We only need to
check the type now.

> typeMismatchedVars :: Prog -> VarStore -> [Var]
> typeMismatchedVars (table, _, _) vStore
>   = keys $ filter (\(k,v) -> not $ isSameType v (getVal k table)) vStore

undeclaredVars:
Find out all the variables that are used but not declared in a programme.
(This wrapper method get rid of duplicates)

> undeclaredVars :: Prog -> [Var]
> undeclaredVars p = nub $ undeclaredVarsProg p

undeclaredVarsProg:
Find out all the variables that are used but not declared in a programme.

> undeclaredVarsProg :: Prog -> [Var]
> undeclaredVarsProg (_, _, []) = []
> undeclaredVarsProg (symTab, pStore, stmt : stmts)
>   = undeclaredVarsStmt (symTab, pStore, stmt) ++ undeclaredVarsProg (symTab, pStore, stmts)

undeclaredVarsStmt:
Find out all the variables that are used but not declared in a statement.

> undeclaredVarsStmt :: (SymTab, PrcdrStore, Stmt) -> [Var]

> undeclaredVarsStmt (_, _, Skip) = []

> undeclaredVarsStmt (symTab, _, Asgn v e)
>   | hasKey v symTab = rest
>   | otherwise       = v : rest
>   where rest = undeclaredVarsExp (symTab, e)

> undeclaredVarsStmt (symTab, _, AsgnArrRef var idxExp valExp)
>   | hasKey var symTab = rest
>   | otherwise         = var : rest
>   where rest = undeclaredVarsExp (symTab, idxExp) ++ undeclaredVarsExp (symTab, valExp)

to check a procedure, we basically treat it as a programme, with parameters
merged into glabal variable store.

> undeclaredVarsStmt (symTab, pStore, InvokePrcdr prcName _)
>   = undeclaredVarsStmt (mergedSymTab, pStore, stmt) ++ undeclaredVarsProg (mergedSymTab, pStore, stmts)
>     where mergedSymTab             = merge symTab paramTab
>           (paramTab, stmt : stmts) = getVal prcName pStore

> undeclaredVarsStmt (symTab, _, If e c p)
>   = undeclaredVarsExp (symTab, e) ++ undeclaredVarsProg c ++ undeclaredVarsProg p

> undeclaredVarsStmt (symTab, _, Do e p)
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
Given a variable store, find out all variables used but not initialised in a
programme.
(This wrapper method get rid of duplicates)

> uninitVars :: Prog -> VarStore -> [Var]
> uninitVars p s = nub $ uninitVarsProg p s

uninitVarsProg:
Given a variable store, find out all variables used but not initialised in a
programme.

> uninitVarsProg :: Prog -> VarStore -> [Var]
> uninitVarsProg (_, _, []) _ = []
> uninitVarsProg (_, pStore, stmt : stmts) vStore
>   = uninitVarsStmt stmt vStore pStore ++ uninitVarsProg (emptyMap, pStore, stmts) vStore

uninitVarsStmt:
Given a variable store, find out all variables used but not initialised in a
statement.

> uninitVarsStmt :: Stmt -> VarStore -> PrcdrStore -> [Var]

> uninitVarsStmt Skip _ _ = []

> uninitVarsStmt (Asgn _ e) vStore _ = uninitVarsExp e vStore

> uninitVarsStmt (AsgnArrRef _ idxExp valExp) vStore _
>   = uninitVarsExp idxExp vStore ++ uninitVarsExp valExp vStore

> uninitVarsStmt (InvokePrcdr prcName params) vStore pStore
>   = uninitVarsStmt stmt mergedVarStore pStore ++ uninitVarsProg (emptyMap, pStore, stmts) mergedVarStore
>     where mergedVarStore    = merge vStore params
>           (_, stmt : stmts) = getVal prcName pStore

> uninitVarsStmt (If e c p) vStore _
>   = uninitVarsExp e vStore ++ uninitVarsProg c vStore ++ uninitVarsProg p vStore

> uninitVarsStmt (Do e p) vStore _ = uninitVarsExp e vStore ++ uninitVarsProg p vStore

uninitVarsExp:
Given a variable store, find out all variables used but not initialised in an
expression.

> uninitVarsExp :: Exp -> VarStore -> [Var]

> uninitVarsExp (ConstInt _) _ = []
> uninitVarsExp (ConstBool _) _ = []

> uninitVarsExp (Var v) vStore
>   | hasKey v vStore = []
>   | otherwise      = [v]

> uninitVarsExp (ArrRef v idxExp) vStore
>   | hasKey v vStore = rest
>   | otherwise      = v : rest
>   where rest = uninitVarsExp idxExp vStore

> uninitVarsExp (Bin _ e e') vStore = uninitVarsExp e vStore ++ uninitVarsExp e' vStore

> uninitVarsExp (Una _ e) vStore = uninitVarsExp e vStore

undefinedPrcdrs:
Find out all procedures used but not defined in the procedure store.
(This wrapper method get rid of duplicates)

> undefinedPrcdrs :: Prog -> [PrcdrName]
> undefinedPrcdrs p = nub $ undefinedPrcdrsProg p

undefinedPrcdrsProg:
Given a programme, Find out all procedures used but not defined in the procedure
store.

> undefinedPrcdrsProg :: Prog -> [PrcdrName]
> undefinedPrcdrsProg (_, _, []) = []
> undefinedPrcdrsProg (symTab, pStore, stmt : stmts)
>    = undefinedPrcdrsStmt (pStore, stmt) ++ undefinedPrcdrsProg (symTab, pStore, stmts)

undefinedPrcdrsStmt:
Given a statement, Find out all procedures used but not defined in the procedure
store.

> undefinedPrcdrsStmt :: (PrcdrStore, Stmt) -> [PrcdrName]
> undefinedPrcdrsStmt (pStore, InvokePrcdr prcName _)
>   | hasKey prcName pStore = []
>   | otherwise             = [prcName]
> undefinedPrcdrsStmt (_, _) = []

incorrectParamInvocs:
Find out all The procedure invocations with parameters passed in mismatching
the defined type.
(This wrapper method get rid of duplicates)

> incorrectParamInvocs :: Prog -> [Stmt]
> incorrectParamInvocs p = nub $ incorrectParamInvocsProg p

incorrectParamInvocsProg:
Given a programme, find out all The procedure invocations with parameters passed
in mismatching the defined type.

> incorrectParamInvocsProg :: Prog -> [Stmt]
> incorrectParamInvocsProg (_, _, []) = []
> incorrectParamInvocsProg (symTab, pStore, stmt : stmts)
>   = incorrectParamInvocsStmt (symTab, pStore, stmt) ++ incorrectParamInvocsProg (symTab, pStore, stmts)

incorrectParamInvocsStmt:
Given a statement, find out all The procedure invocations with parameters passed
in mismatching the defined type.

> incorrectParamInvocsStmt :: (SymTab, PrcdrStore, Stmt) -> [Stmt]
> incorrectParamInvocsStmt (_, pStore, s@(InvokePrcdr prcName params))
>   | check11 || check12 || check13 = [s]
>   | otherwise                     = []
>     where (symTab, stmts) = getVal prcName pStore
>           check11 = not $ null $ dclrdButNotInitVars (symTab, pStore, stmts) params
>           check12 = not $ null $ intiButNotdclrdVars (symTab, pStore, stmts) params
>           check13 = not $ null $ typeMismatchedVars (symTab, pStore, stmts) params

> incorrectParamInvocsStmt (_, _, If _ c p)
>   = incorrectParamInvocsProg c ++ incorrectParamInvocsProg p

> incorrectParamInvocsStmt (_, _, Do _ p) = incorrectParamInvocsProg p

> incorrectParamInvocsStmt (_, _, _) = []

arrVars:
Check whether an array variable is used alone, i.e. without an index.
(This wrapper method get rid of duplicates)

> arrVars :: Prog -> [Var]
> arrVars p = nub $ arrVarsProg p

arrVarsProg:
Checks in programme whether an array variable is used alone, i.e. without an index.

> arrVarsProg :: Prog -> [Var]
> arrVarsProg (_, _, []) = []
> arrVarsProg (symTab, pStore, stmt : stmts)
>   = arrVarsStmt (symTab, pStore, stmt) ++ arrVarsProg (symTab, pStore, stmts)

arrVarsStmt:
Checks in statement whether an array variable is used alone, i.e. without an index.

> arrVarsStmt :: (SymTab, PrcdrStore, Stmt) -> [Var]

> arrVarsStmt (_, _, Skip) = []

> arrVarsStmt (symTab, _, Asgn _ e) = arrVarsExp (symTab, e)

> arrVarsStmt (symTab, _, AsgnArrRef _ idxExp valExp)
>   = arrVarsExp (symTab, idxExp) ++ arrVarsExp (symTab, valExp)

> arrVarsStmt (symTab, pStore, InvokePrcdr prcName _)
>   = arrVarsStmt (mergedSymTab, pStore, stmt) ++ arrVarsProg (mergedSymTab, pStore, stmts)
>     where mergedSymTab             = merge symTab paramTab
>           (paramTab, stmt : stmts) = getVal prcName pStore

> arrVarsStmt (symTab, _, If e c p)
>   = arrVarsExp (symTab, e) ++ arrVarsProg c ++ arrVarsProg p

> arrVarsStmt (symTab, _, Do e p)
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

> badAsgnArrRefs (_, _, []) = []

> badAsgnArrRefs (symTab, pStore, s@(AsgnArrRef v idxExp _) : stmts)
>   | hasKey v symTab && (getVal v symTab == IntType)  = s : rest
>   | hasKey v symTab && (getVal v symTab == BoolType) = s : rest
>   | expType idxExp symTab /= IntType = s : rest
>   | otherwise = rest
>   where rest = badAsgnArrRefs (symTab, pStore, stmts)

> badAsgnArrRefs (symTab, pStore, _ : stmts) = badAsgnArrRefs (symTab, pStore, stmts)

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
> badArrRefsProg (_, _, []) = []
> badArrRefsProg (symTab, pStore, stmt : stmts)
>   = badArrRefsStmt (symTab, pStore, stmt) ++ badArrRefsProg (symTab, pStore, stmts)

badArrRefsStmt:
Checks whether variables and index expressions have incorrect type in
(ArrRef Var Exp) in a Statement

> badArrRefsStmt :: (SymTab, PrcdrStore, Stmt) -> [Exp]

> badArrRefsStmt (_, _, Skip) = []

> badArrRefsStmt (symTab, _, Asgn _ e) = badArrRefsExp (symTab, e)

> badArrRefsStmt (symTab, _, AsgnArrRef _ idxExp valExp)
>   = badArrRefsExp (symTab, idxExp) ++ badArrRefsExp (symTab, valExp)

> badArrRefsStmt (symTab, pStore, InvokePrcdr prcName _)
>   = badArrRefsStmt (mergedSymTab, pStore, stmt) ++ badArrRefsProg (mergedSymTab, pStore, stmts)
>     where mergedSymTab             = merge symTab paramTab
>           (paramTab, stmt : stmts) = getVal prcName pStore

> badArrRefsStmt (symTab, _, If e c p)
>   = badArrRefsExp (symTab, e) ++ badArrRefsProg c ++ badArrRefsProg p

> badArrRefsStmt (symTab, _, Do e p) = badArrRefsExp (symTab, e) ++ badArrRefsProg p

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

> incorrectAsgnmts (_, _, []) = []

> incorrectAsgnmts (symTab, pStore, Skip : stmts) = incorrectAsgnmts (symTab, pStore, stmts)

> incorrectAsgnmts (symTab, pStore, s@(Asgn v e) : stmts)
>   | hasKey v symTab && (getVal v symTab /= expType e symTab) = s : rest
>   | otherwise = rest
>   where rest = incorrectAsgnmts (symTab, pStore, stmts)

> incorrectAsgnmts (symTab, pStore, s@(AsgnArrRef v _ valExp) : stmts)
>   | hasKey v symTab && (expType (ArrRef v (ConstInt 0)) symTab /= expType valExp symTab) = s : rest
>   | otherwise = rest
>   where rest = incorrectAsgnmts (symTab, pStore, stmts)

> incorrectAsgnmts (symTab, pStore, InvokePrcdr prcName _ : stmts)
>   = incorrectAsgnmts (mergedSymTab, pStore, prcStmts) ++ rest
>     where mergedSymTab         = merge symTab paramTab
>           (paramTab, prcStmts) = getVal prcName pStore
>           rest                 = incorrectAsgnmts (symTab, pStore, stmts)

> incorrectAsgnmts (symTab, pStore, If _ c p : stmts)
>   = incorrectAsgnmts c ++ incorrectAsgnmts p ++ incorrectAsgnmts (symTab, pStore, stmts)

> incorrectAsgnmts (symTab, pStore, Do _ p : stmts)
>   = incorrectAsgnmts p ++ incorrectAsgnmts (symTab, pStore, stmts)

misConExps:
Find out all the places where integer valued expressions are used as condition
in If or Do.

> misConExps :: Prog -> [Exp]

> misConExps (_, _, []) = []

> misConExps (symTab, pStore, InvokePrcdr prcName _ : stmts)
>   = misConExps (mergedSymTab, pStore, prcStmts) ++ rest
>     where mergedSymTab         = merge symTab paramTab
>           (paramTab, prcStmts) = getVal prcName pStore
>           rest                 = misConExps (symTab, pStore, stmts)

> misConExps (symTab, pStore, If e c p : stmts)
>   | expType e symTab /= BoolType = e : rest
>   | otherwise                    = rest
>   where rest = misConExps c ++ misConExps p ++ misConExps (symTab, pStore, stmts)

> misConExps (symTab, pStore, Do e p : stmts)
>   | expType e symTab /= BoolType = e : rest
>   | otherwise                    = rest
>   where rest = misConExps p ++ misConExps (symTab, pStore, stmts)

> misConExps (symTab, pStore, _ : stmts) = misConExps (symTab, pStore, stmts)

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
A helper function to check if two variables have same types in variable store
and in symbol table

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

> s1 :: VarStore
> s1 = []
> s2 :: VarStore
> s2 = [('x', Int 1)]
> s3 :: VarStore
> s3 = [('x', Int 1), ('y', Int 2)]
> s4 :: VarStore
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
> p1 = (t1, pStore1, [Skip])
> p2 :: Prog
> p2 = (t1, pStore1, [Skip, Skip])
> p3 :: Prog
> p3 = (t1, pStore1, [Asgn 'x' e1])
> p4 :: Prog
> p4 = (t1, pStore1, [Asgn 'x' (Var 'x')])
> p5 :: Prog
> p5 = (t3, pStore1, [Asgn 'x' (Var 'y')])
> p6 :: Prog
> p6 = (t3, pStore1, [Asgn 'x' (Bin Plus (Var 'x') (ConstInt 1))])
> p7 :: Prog
> p7 = (t3, pStore1, [Asgn 'x' e1, Asgn 'y' e2,
>       If (Bin Eq (Var 'x') (Var 'y')) ([], [], [Asgn 'z' e1]) ([], [], [Asgn 'z' e2])])
> p8 :: Prog
> p8 = (t3, pStore1, [Asgn 'i' e1, Asgn 's' e0,
>       Do (Bin Lt (Var 'i') (Var 'n')) ([], [], [Asgn 's' (Bin Plus (Var 's') e1), Asgn 'i' e5])])

My test:

> s9 :: VarStore
> s9 = [
>        ('x', Bool True),
>        ('y', Bool False),
>        ('z', Arr arr 4 IntType),
>        ('m', Int 9999),
>        ('n', Int 5),
>        ('p', Int 0)
>      ]
>      where arr = array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]

> t9 :: SymTab
> t9 = [
>        ('x', BoolType),
>        ('y', BoolType),
>        ('z', ArrayType IntType),
>        ('m', IntType),
>        ('n', IntType),
>        ('p', IntType)
>      ]

> pStore1 :: PrcdrStore
> pStore1 = [("method",
>            ([
>               ('x', BoolType),
>               ('s', IntType)
>             ],
>             [
>                Asgn 'x' (ConstBool False),
>                Asgn 's' (Bin Plus (ConstInt 500) (ConstInt 300))
>             ])
>            )]

> p9 :: Prog
> p9 = (
>   t9,
>   pStore1,
>   [
>     Asgn 'x' (Una Not (Var 'y')),
>     Asgn 'y' (Var 'x'),
>     AsgnArrRef 'z' (ConstInt 0) (Bin Plus (ArrRef 'z' (ConstInt 2)) (ConstInt 10)),
>     If (Bin And (Var 'x') (Var 'y')) (t9, [], [Asgn 'm' (ConstInt 5)]) (t9, [], [Asgn 'n' (ConstInt 0)]),
>     Do (Bin Gt (Var 'n') (ConstInt 1)) (t9, [], [Asgn 'n' (Bin Minus (Var 'n') (ConstInt 1))]),
>     InvokePrcdr "method" [ ('x', Bool True), ('s', Int 555)]
>   ])
