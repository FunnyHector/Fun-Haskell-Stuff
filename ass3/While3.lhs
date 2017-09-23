-----------------------------------------------------
                 General discussion
-----------------------------------------------------

What I did:


type support for array of arrays. Theoretically it supports many dimensions.







What I didn't do:

1. Although multi dimension array can be stored in the initial store, it cannot
be read or updated. I think I know how to do this, but time is really tight for
such a big assignment. A possible solution is to change the statement
`AsgnArrRef Var Exp Exp` to `AsgnArrRef Exp Exp Exp`, and change the expression
`ArrRef Var Exp` to `ArrRef Exp Exp`. This change allow us to recursively access
a multi-dimension array like `ArrRef (ArrRef (Var 'x') (Int 0)) (Int 0)`. If I
had more time, this would be where I'm heading.





I didn't do variable declaration. I tried, but really couldn't within such a
short time. The most diffucult part lives in scoping variables and hence more
complicated static checking. Almost all static checking functions in this part
need to be modified to keep track of variable declarations. Also for each
declaration, we need to use different checking strategy depending on whether
it's global or local variable declaration.




My approach for implementing scopes. One limitation: parameters can be modified.



-----------------------------------------------------
                        Code
-----------------------------------------------------


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
`intersperse` function is used to display vars nicely

> import Data.List (nub, intersperse)

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

A block is a list of statements.

> type Block = [Stmt]

A procedure is a list of statements, and a symbol table to define the type of
parameters. When invoking a procedure, often some parameters are passed in. A
procedure can be invoked using the name.

> type Prcdr = (SymTab, Block)

A program is a symbol table, a procedure store, and a list of statements

> type Prog = (SymTab, PrcdrStore, Block)

A statement can be a skip, assignment, array reference assignment, procedure
invocation, if, or do statement.

> data Stmt = Skip
>           | Asgn Var Exp
>           | AsgnArrRef Var Exp Exp
>           | InvokePrcdr PrcdrName VarStore
>           | If Exp Block Block
>           | Do Exp Block
>           | Declare Var Type
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

A symbol table is a map from varibales to types

> type SymTab = Map Var Type

A procedure store is a map from procedure name to procedures

> type PrcdrStore = Map PrcdrName Prcdr

State is a representation of a state during the execution of programme. A state
contains:
  1) a list of pairs of symbol table and variable store. Each pair represents
     the local variables in current scope.
  2) procedure store (currently do not support local procedure)

> type State = ([(SymTab, VarStore)], PrcdrStore)

A result is like Either, it's either a good result or an error with message
(Not used so far)

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

4. Variables cannot be re-declared. (reDeclarations)

5. All variables used in expression have values initialised. (uninitVars)

6. The parameters passed in matches the defined type. (incorrectParamInvocs)
   This check is essentially check no. 1 applied on procedures.

7. Array related check:
   7.1. Array variable is not used alone, i.e. have to with an index. (arrVars)
   7.2. Var in `AsgnArrRef Var Exp Exp` is ArrayType. (badAsgnArrRefs)
   7.3. The first Exp (index expression) in `AsgnArrRef Var Exp Exp` is IntType.
        (badAsgnArrRefs)
   7.4. The second Exp (value expression) in `AsgnArrRef Var Exp Exp` matches
        the type of elements in this array. (badAsgnArrRefs)
   7.5. Var in `ArrRef Var Exp` is ArrayType. (badArrRefs)
   7.6. The Exp in `ArrRef Var Exp` is IntType. (badArrRefs)

8. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. N.B. this check does
   not check for `AsgnArrRef Var Exp Exp`. (wrongTypeAsgnmts)

9. Conditional expressions for If or Do statement are used correctly. This is
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
> run p@(symTab, pStore, block) vStore
>     -- these checks have really bad names, see the `where` clause for more info
>   | not $ null check11
>       = error ("\nDeclared but not initialised Variable(s): " ++ showVars check11)
>   | not $ null check12
>       = error ("\nInitialised but not declared Variable(s): " ++ showVars check12)
>   | not $ null check13
>       = error ("\nMismatched type in variable store and symbol table: " ++ showVars check13)
>   | not $ null check2
>       = error ("\nUndefined procedure(s): " ++ show check2)
>   | not $ null check3
>       = error ("\nUndeclared Variable(s): " ++ showVars check3)
>   | not $ null check4
>       = error ("\nVariable re-declaration(s) found: " ++ show check4)
>   | not $ null check5
>       = error ("\nUninitialised Variable(s): " ++ showVars check5)
>   | not $ null check6
>       = error ("\nProcedure(s) invoked with incorrect parameters: " ++ show check6)
>   | not $ null check71
>       = error ("\nArray Variable(s) used alone: " ++ showVars check71)
>   | not $ null check7234
>       = error ("\nType errors in array reference assignment(s): " ++ show check7234)
>   | not $ null check756
>       = error ("\nType errors in array reference(s): " ++ show check756)
>   | not $ null check8
>       = error ("\nType errors in Assignment(s): " ++ show check8)
>   | not $ null check9
>       = error ("\nIncorrect Expression(s): " ++ show check9)
>   | otherwise
>       = getGlobalVarStore postState -- the non-error scenario
>   where check11   = dclrdButNotInitVars p vStore    -- check no. 1.1
>         check12   = intiButNotdclrdVars p vStore    -- check no. 1.2
>         check13   = typeMismatchedVars p vStore     -- check no. 1.3
>         check2    = undefinedPrcdrs p               -- check no. 2
>         check3    = undeclaredVars p                -- check no. 3
>         check4    = reDeclarations p                -- check no. 4
>         check5    = uninitVars p vStore             -- check no. 5
>         check6    = incorrectParamInvocs p          -- check no. 6
>         check71   = arrVars p                       -- check no. 7.1
>         check7234 = badAsgnArrRefs p                -- check no. 7.2, 7.3, & 7.4
>         check756  = badArrRefs p                    -- check no. 7.5 & 7.6
>         check8    = wrongTypeAsgnmts p              -- check no. 8
>         check9    = misConExps p                    -- check no. 9
>         postState = exec block ([(symTab, vStore)], pStore)
>         showVars  = intersperse ','

exec:
To execute a program, we just execute each statement in turn, passing the
resulting state to the next statement at each step.

> exec :: Block -> State -> State
> exec [] state            = state
> exec (stmt : stmts) state = exec stmts (exec' stmt state)

exec':
Execute a single statement, according to its semantics

> exec' :: Stmt -> State -> State

> exec' Skip state = state

> exec' (Asgn var expression) state = assignVar var newVal state
>   where newVal = eval expression state

> exec' (AsgnArrRef var idxExp valExp) state = assignVar var newArray state
>   where (Arr oldArray size tipe) = getVarValue var state
>         (Int idx)                = eval idxExp state
>         newVal                   = eval valExp state
>         newArray                 = Arr (oldArray // [(idx, newVal)]) size tipe

to execute a procedure, we add parameters as a new scope, and run the procedure.
After the procedure is finished, we do not care about any change in the new
scope. In other word, we only keep changes in existing scopes.

> exec' (InvokePrcdr prcName paramStore) state@(scopes, pStore)
>   = (postScopes, postStore)
>     where (paramTab, block)           = getProcedure prcName state
>           newState                    = ((paramTab, paramStore) : scopes, pStore)
>           (_ : postScopes, postStore) = exec block newState

to execute a If block is like to execute a block with a new empty scope

> exec' (If cond thenPart elsePart) state@(scopes, pStore) =
>   if b
>   then (postThenScopes, postThenPStore)
>   else (postElseScopes, postElsePStore)
>   where Bool b   = eval cond state
>         newState = ((emptyMap, emptyMap) : scopes, pStore)
>         (_ : postThenScopes, postThenPStore) = exec thenPart newState
>         (_ : postElseScopes, postElsePStore) = exec elsePart newState

> exec' (Do cond block) state@(scopes, pStore) =
>   if not b
>   then state
>   else exec' (Do cond block) postState
>   where Bool b    = eval cond state
>         newState  = ((emptyMap, emptyMap) : scopes, pStore)
>         postState = (postThenScopes, postThenPStore)
>         (_ : postThenScopes, postThenPStore) = exec block newState

> exec' (Declare var tipe) state = declareVar var tipe state

eval:
Evaluate an expression, according to its type

> eval :: Exp -> State -> Val

> eval (ConstInt i)  _ = Int i
> eval (ConstBool b) _ = Bool b

> eval (Var v) state
>   | isVarInitialised v state = getVarValue v state
>   | otherwise                = error ("Uninitialised variable: " ++ [v])  -- shouldn't come here if we do static checking before running.

> eval (ArrRef v idxExp) state
>   | isVarInitialised v state = arr ! idx
>   | otherwise                = error ("Uninitialised variable: " ++ [v])  -- shouldn't come here if we do static checking before running.
>   where (Arr arr _ _) = getVarValue v state
>         (Int idx)     = eval idxExp state

> eval (Bin op x y) state = applyBin op (eval x state) (eval y state)

> eval (Una op x)   state = applyUna op (eval x state)

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

TODO: with check no. 3, and check no. 5, check no. 1 is redundant????

What we need to statically check:

1. The symbol table and the variable store matches each other.
   1.1. All declared variables are initialised. (dclrdButNotInitVars)
   1.2. All initialised variables are declared. (intiButNotdclrdVars)
   1.3. Types are matched. (typeMismatchedVars)

2. All procedure being invoked are defined in procedure store. (undefinedPrcdrs)

3. All variables used are declared with a type. (undeclaredVars)

4. Variables cannot be re-declared. (reDeclarations)

5. All variables used in expression have values initialised. (uninitVars)

6. The parameters passed in matches the defined type. (incorrectParamInvocs)
   This check is essentially check no. 1 applied on procedures.

7. Array related check:
   7.1. Array variable is not used alone, i.e. have to with an index. (arrVars)
   7.2. Var in `AsgnArrRef Var Exp Exp` is ArrayType. (badAsgnArrRefs)
   7.3. The first Exp (index expression) in `AsgnArrRef Var Exp Exp` is IntType.
        (badAsgnArrRefs)
   7.4. The second Exp (value expression) in `AsgnArrRef Var Exp Exp` matches
        the type of elements in this array. (badAsgnArrRefs)
   7.5. Var in `ArrRef Var Exp` is ArrayType. (badArrRefs)
   7.6. The Exp in `ArrRef Var Exp` is IntType. (badArrRefs)

8. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. N.B. this check does
   not check for `AsgnArrRef Var Exp Exp`. (wrongTypeAsgnmts)

9. Conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer/array values end up as condition expression of If or
   Do. (misConExps)

N.B. check no. 1, 2, and 3 should be redundant in part 3, because after I
implemented variable declaration, these checks need to keep track of newly
declared variables so the logic is totally re-written. I still keep checks no.
1, 2, and 3 just to prevent errors as early as possible.

In many of these checks, I used `nub` to remove duplicates. This isn't quite
right, e.g. there can be multiple occurrence of same variable that is not
declared. However, without tracking the whereabouts of these variables,
displaying same variable multiple times in error message is useless. At least
this is a working mechanism of static checking.


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

undefinedPrcdrs:
Find out all procedures used but not defined in the procedure store.
(This wrapper method get rid of duplicates)

> undefinedPrcdrs :: Prog -> [PrcdrName]
> undefinedPrcdrs (_, pStore, block)
>   = nub $ undefinedPrcdrsBlock allStmts ([(emptyMap, emptyMap)], pStore)
>     where stmtsFormPrcdrs = concatMap (snd . snd) pStore
>           allStmts        = nub $ block ++ stmtsFormPrcdrs

undefinedPrcdrsBlock:
Given a block, Find out all procedures used but not defined in the procedure
store.

> undefinedPrcdrsBlock :: Block -> State -> [PrcdrName]
> undefinedPrcdrsBlock [] _ = []
> undefinedPrcdrsBlock (stmt : stmts) state
>    = undefinedPrcdrsStmt stmt state ++ undefinedPrcdrsBlock stmts state

undefinedPrcdrsStmt:
Given a statement, Find out all procedures used but not defined in the procedure
store.

> undefinedPrcdrsStmt :: Stmt -> State -> [PrcdrName]
> undefinedPrcdrsStmt (InvokePrcdr prcName _) state
>   | isProcDefined prcName state = []
>   | otherwise                   = [prcName]
> undefinedPrcdrsStmt _ _         = []

undeclaredVars:
Find out all the variables that are used but not declared in a programme.
(This top-level wrapper method also gets rid of duplicates)

> undeclaredVars :: Prog -> [Var]
> undeclaredVars (symTab, pStore, block)
>   = nub badVars
>     where (badVars, _) = undeclaredVarsBlock block ([(symTab, emptyMap)], pStore) []

undeclaredVarsBlock:
Find out all the variables that are used but not declared in a programme.

> undeclaredVarsBlock :: Block -> State -> [PrcdrName] -> ([Var], [PrcdrName])
> undeclaredVarsBlock [] _ visitedPrc = ([], visitedPrc)
> undeclaredVarsBlock (stmt : stmts) state visitedPrc
>   = (vars ++ vars', postVisitedPrc')
>     where (vars, postState, postVisitedPrc) = undeclaredVarsStmt stmt state visitedPrc
>           (vars', postVisitedPrc') = undeclaredVarsBlock stmts postState postVisitedPrc

undeclaredVarsStmt:
Find out all the variables that are used but not declared in a statement.

> undeclaredVarsStmt :: Stmt -> State -> [PrcdrName] -> ([Var], State, [PrcdrName])

> undeclaredVarsStmt Skip state visitedPrc = ([], state, visitedPrc)

> undeclaredVarsStmt (Asgn v e) state visitedPrc
>   | isVarDeclared v state = (rest, state, visitedPrc)
>   | otherwise             = (v : rest, state, visitedPrc)
>   where rest = undeclaredVarsExp e state

> undeclaredVarsStmt (AsgnArrRef var idxExp valExp) state visitedPrc
>   | isVarDeclared var state = (rest, state, visitedPrc)
>   | otherwise               = (var : rest, state, visitedPrc)
>   where rest = undeclaredVarsExp idxExp state ++ undeclaredVarsExp valExp state

to check a procedure, we basically treat it as a programme, with parameters
merged into glabal variable store.

> undeclaredVarsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badVars, state, postVisitedPrc)
>     where (paramTab, block)         = getProcedure prcName state
>           newState                  = ((paramTab, emptyMap) : scopes, pStore)
>           (badVars, postVisitedPrc) = undeclaredVarsBlock block newState (prcName : visitedPrc)

> undeclaredVarsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   = (undeclaredVarsExp e state ++ badVars ++ badVars', state, visitedPrc)
>     where newState      = ((emptyMap, emptyMap) : scopes, pStore)
>           (badVars,  _) = undeclaredVarsBlock c newState visitedPrc
>           (badVars', _) = undeclaredVarsBlock p newState visitedPrc

> undeclaredVarsStmt (Do e p) state@(scopes, pStore) visitedPrc
>   = (undeclaredVarsExp e state ++ badVars, state, visitedPrc)
>     where newState     = ((emptyMap, emptyMap) : scopes, pStore)
>           (badVars, _) = undeclaredVarsBlock p newState visitedPrc

> undeclaredVarsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

undeclaredVarsExp:
Find out all the variables that are used but not declared in an expression.

> undeclaredVarsExp :: Exp -> State -> [Var]

> undeclaredVarsExp (Var v) state
>   | isVarDeclared v state = []
>   | otherwise             = [v]

> undeclaredVarsExp (ArrRef v idxExp) state
>   | isVarDeclared v state = rest
>   | otherwise             = v : rest
>   where rest = undeclaredVarsExp idxExp state

> undeclaredVarsExp (Bin _ e e') state
>   = undeclaredVarsExp e state ++ undeclaredVarsExp e' state

> undeclaredVarsExp (Una _ e) state = undeclaredVarsExp e state

> undeclaredVarsExp _ _ = []

reDeclarations:
Find out all attempts to re-declare variables.

> reDeclarations :: Prog -> [Stmt]
> reDeclarations (symTab, pStore, block)
>   = nub $ badStmts ++ badStmtsFromPrcs
>     where badStmts = reDeclarationsBlock block ([(symTab, emptyMap)], pStore)
>           prcdrs = map snd pStore
>           badStmtsFromPrcs = concatMap (\(paramTab, pBlock) -> reDeclarationsBlock pBlock ([(paramTab, emptyMap)], pStore)) prcdrs

reDeclarationsBlock:
Given a block, find out all attempts to re-declare variables.

> reDeclarationsBlock :: Block -> State -> [Stmt]
> reDeclarationsBlock [] _ = []
> reDeclarationsBlock (stmt : stmts) state
>   = vars ++ reDeclarationsBlock stmts postState
>     where (vars, postState) = reDeclarationsStmt stmt state

reDeclarationsStmt:
Given a statement, find out all attempts to re-declare variables.

> reDeclarationsStmt :: Stmt -> State -> ([Stmt], State)

> reDeclarationsStmt (If _ thenPart elsePart) state@(scopes, pStore)
>   = (reDeclarationsBlock thenPart newState ++ reDeclarationsBlock elsePart newState, state)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)

> reDeclarationsStmt (Do _ block) state@(scopes, pStore)
>   = (reDeclarationsBlock block newState, state)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)

> reDeclarationsStmt stmt@(Declare var tipe) state
>   | isVarDeclaredCurrentScope var state = ([stmt], state)
>   | otherwise                           = ([], declareVar var tipe state)

> reDeclarationsStmt _ state = ([], state)

uninitVars:
Find out all variables used but not initialised in a programme.
(This top-level wrapper method also gets rid of duplicates)

> uninitVars :: Prog -> VarStore -> [Var]
> uninitVars (_, pStore, block) vStore
>   = nub badVars
>     where (badVars, _) = uninitVarsBlock block ([(emptyMap, vStore)], pStore) []

uninitVarsBlock:
Find out all variables used but not initialised in a block.

> uninitVarsBlock :: Block -> State -> [PrcdrName] -> ([Var], [PrcdrName])
> uninitVarsBlock [] _ visitedPrc = ([], visitedPrc)
> uninitVarsBlock (stmt : stmts) state visitedPrc
>   = (vars ++ vars', postVisitedPrc')
>     where (vars, postState, postVisitedPrc) = uninitVarsStmt stmt state visitedPrc
>           (vars', postVisitedPrc') = uninitVarsBlock stmts postState postVisitedPrc

uninitVarsStmt:
Find out all variables used but not initialised in a statement.

> uninitVarsStmt :: Stmt -> State -> [PrcdrName] -> ([Var], State, [PrcdrName])

if the expression `e` is checked out, we assign (Int 0) to it to mark it as
initialised instead of actual value. #optimisation.

> uninitVarsStmt (Asgn v e) state visitedPrc
>   | null badVars = ([], assignVar v (Int 0) state, visitedPrc)
>   | otherwise    = (badVars, state, visitedPrc)
>   where badVars = uninitVarsExp e state

> uninitVarsStmt (AsgnArrRef v idxExp valExp) state visitedPrc
>   | null badVarsIdx && null badVarsVal = ([], assignVar v (Int 0) state, visitedPrc)
>   | otherwise                          = (badVarsIdx ++ badVarsVal, state, visitedPrc)
>   where badVarsIdx = uninitVarsExp idxExp state
>         badVarsVal = uninitVarsExp valExp state

> uninitVarsStmt (InvokePrcdr prcName params) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badVars, state, postVisitedPrc)
>     where (_, block)                = getProcedure prcName state
>           newState                  = ((emptyMap, params) : scopes, pStore)
>           (badVars, postVisitedPrc) = uninitVarsBlock block newState (prcName : visitedPrc)

> uninitVarsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   = (uninitVarsExp e state ++ badVars ++ badVars', state, visitedPrc)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)
>           (badVars,  _)  = uninitVarsBlock c newState visitedPrc
>           (badVars',  _) = uninitVarsBlock p newState visitedPrc

> uninitVarsStmt (Do e p) state@(scopes, pStore) visitedPrc
>   = (uninitVarsExp e state ++ badVars, state, visitedPrc)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)
>           (badVars,  _)  = uninitVarsBlock p newState visitedPrc

> uninitVarsStmt _ state visitedPrc = ([], state, visitedPrc)

uninitVarsExp:
Find out all variables used but not initialised in an expression.

> uninitVarsExp :: Exp -> State -> [Var]

> uninitVarsExp (Var v) state
>   | isVarInitialised v state = []
>   | otherwise                = [v]

> uninitVarsExp (ArrRef v idxExp) state
>   | isVarInitialised v state = rest
>   | otherwise                = v : rest
>   where rest = uninitVarsExp idxExp state

> uninitVarsExp (Bin _ e e') state = uninitVarsExp e state ++ uninitVarsExp e' state

> uninitVarsExp (Una _ e) state = uninitVarsExp e state

> uninitVarsExp  _ _ = []

incorrectParamInvocs:
Find out all The procedure invocations with parameters passed in mismatching
the defined type.
(This wrapper method get rid of duplicates)

> incorrectParamInvocs :: Prog -> [Stmt]
> incorrectParamInvocs (_, pStore, block)
>   = nub badVars
>     where (badVars, _) = incorrectParamInvocsBlock block ([(emptyMap, emptyMap)], pStore) []

incorrectParamInvocsBlock:
Given a block, find out all The procedure invocations with parameters passed
in mismatching the defined type.

> incorrectParamInvocsBlock :: Block -> State -> [PrcdrName] -> ([Stmt], [PrcdrName])
> incorrectParamInvocsBlock [] _ visitedPrc = ([], visitedPrc)
> incorrectParamInvocsBlock (stmt : stmts) state visitedPrc
>   = (vars ++ vars', postVisitedPrc')
>     where (vars,  postVisitedPrc) = incorrectParamInvocsStmt stmt state visitedPrc
>           (vars', postVisitedPrc') = incorrectParamInvocsBlock stmts state postVisitedPrc

incorrectParamInvocsStmt:
Given a statement, find out all The procedure invocations with parameters passed
in mismatching the defined type.

> incorrectParamInvocsStmt :: Stmt -> State -> [PrcdrName] -> ([Stmt], [PrcdrName])
> incorrectParamInvocsStmt stmt@(InvokePrcdr prcName params) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], visitedPrc)
>   | check11 || check12 || check13 = (stmt : badStmts, postVisitedPrc)
>   | otherwise                     = (badStmts, postVisitedPrc)
>   where (symTab, stmts)            = getProcedure prcName state
>         newState                   = ((emptyMap, emptyMap) : scopes, pStore)
>         (badStmts, postVisitedPrc) = incorrectParamInvocsBlock stmts newState (prcName : visitedPrc)
>         check11 = not $ null $ dclrdButNotInitVars (symTab, pStore, stmts) params
>         check12 = not $ null $ intiButNotdclrdVars (symTab, pStore, stmts) params
>         check13 = not $ null $ typeMismatchedVars (symTab, pStore, stmts) params

> incorrectParamInvocsStmt (If _ c p) state visitedPrc
>   = (badStmts ++ badStmts', visitedPrc)
>     where (badStmts,  _) = incorrectParamInvocsBlock c state visitedPrc
>           (badStmts', _) = incorrectParamInvocsBlock p state visitedPrc

> incorrectParamInvocsStmt (Do _ p) state visitedPrc
>   = (badStmts, visitedPrc)
>     where (badStmts,  _) = incorrectParamInvocsBlock p state visitedPrc

> incorrectParamInvocsStmt _ _ visitedPrc = ([], visitedPrc)

arrVars:
Check whether an array variable is used alone, i.e. without an index.
(This top-level wrapper method also gets rid of duplicates)

> arrVars :: Prog -> [Var]
> arrVars (symTab, pStore, block)
>   = nub badVars
>     where (badVars, _) = arrVarsBlock block ([(symTab, emptyMap)], pStore) []

arrVarsBlock:
Checks whether an array variable is used alone in a block, i.e. without an index.

> arrVarsBlock :: Block -> State -> [PrcdrName] -> ([Var], [PrcdrName])
> arrVarsBlock [] _ visitedPrc = ([], visitedPrc)
> arrVarsBlock (stmt : stmts) state visitedPrc
>   = (vars ++ vars', postVisitedPrc')
>     where (vars, postState, postVisitedPrc) = arrVarsStmt stmt state visitedPrc
>           (vars', postVisitedPrc') = arrVarsBlock stmts postState postVisitedPrc

arrVarsStmt:
Checks whether an array variable is used alone in a statement, i.e. without an index.

> arrVarsStmt :: Stmt -> State -> [PrcdrName] -> ([Var], State, [PrcdrName])

> arrVarsStmt Skip state visitedPrc = ([], state, visitedPrc)

> arrVarsStmt (Asgn _ e) state visitedPrc = (arrVarsExp e state, state, visitedPrc)

> arrVarsStmt (AsgnArrRef _ idxExp valExp) state visitedPrc
>   = (arrVarsExp idxExp state ++ arrVarsExp valExp state, state, visitedPrc)

> arrVarsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badVars, state, postVisitedPrc)
>     where (paramTab, block)         = getProcedure prcName state
>           newState                  = ((paramTab, emptyMap) : scopes, pStore)
>           (badVars, postVisitedPrc) = arrVarsBlock block newState (prcName : visitedPrc)

> arrVarsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   = (arrVarsExp e state ++ badVars ++ badVars', state, visitedPrc)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)
>           (badVars,   _) = arrVarsBlock c newState visitedPrc
>           (badVars',  _) = arrVarsBlock p newState visitedPrc

> arrVarsStmt (Do e p) state@(scopes, pStore) visitedPrc
>   = (arrVarsExp e state ++ badVars, state, visitedPrc)
>     where newState     = ((emptyMap, emptyMap) : scopes, pStore)
>           (badVars, _) = arrVarsBlock p newState visitedPrc

> arrVarsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

arrVarsExp:
Checks whether an array variable is used alone in an expression, i.e. without an index.

> arrVarsExp :: Exp -> State -> [Var]

> arrVarsExp (Var v) state
>   | isVarDeclared v state && getVarType v state == IntType  = []
>   | isVarDeclared v state && getVarType v state == BoolType = []
>   | otherwise       = [v]

> arrVarsExp (ArrRef _ idxExp) state = arrVarsExp idxExp state

> arrVarsExp (Bin _ e e') state = arrVarsExp e state ++ arrVarsExp e' state

> arrVarsExp (Una _ e) state = arrVarsExp e state

> arrVarsExp _ _ = []

badAsgnArrRefs:
Checks through a programme whether variables and index expressions have
incorrect type in `AsgnArrRef Var Exp Exp`.
(This top-level wrapper method also gets rid of duplicates)

> badAsgnArrRefs :: Prog -> [Stmt]
> badAsgnArrRefs (symTab, pStore, block)
>   = nub badStmts
>     where (badStmts, _) = badAsgnArrRefsBlock block ([(symTab, emptyMap)], pStore) []

badAsgnArrRefsBlock:
Checks through a block whether variables and index expressions have incorrect
type in `AsgnArrRef Var Exp Exp`.

> badAsgnArrRefsBlock :: Block -> State -> [PrcdrName] -> ([Stmt], [PrcdrName])
> badAsgnArrRefsBlock [] _ visitedPrc = ([], visitedPrc)
> badAsgnArrRefsBlock (stmt : stmts) state visitedPrc
>   = (vars ++ vars', postVisitedPrc')
>     where (vars, postState, postVisitedPrc) = badAsgnArrRefsStmt stmt state visitedPrc
>           (vars', postVisitedPrc') = badAsgnArrRefsBlock stmts postState postVisitedPrc

badAsgnArrRefsStmt:
Checks through a statement whether variables and index expressions have
incorrect type in `AsgnArrRef Var Exp Exp`.

> badAsgnArrRefsStmt :: Stmt -> State -> [PrcdrName] -> ([Stmt], State, [PrcdrName])

> badAsgnArrRefsStmt s@(AsgnArrRef v idxExp valExp) state visitedPrc
>   | isVarDeclared v state && (getVarType v state == IntType) = ([s], state, visitedPrc)
>   | isVarDeclared v state && (getVarType v state == BoolType) = ([s], state, visitedPrc)
>   | expType idxExp state /= IntType = ([s], state, visitedPrc)
>   | expType valExp state /= tipe = ([s], state, visitedPrc)
>   | otherwise = ([], state, visitedPrc)
>   where (ArrayType tipe) = getVarType v state

> badAsgnArrRefsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>  | prcName `elem` visitedPrc = ([], state, visitedPrc)
>  | otherwise                 = (badStmts, state, postVisitedPrc)
>     where (paramTab, block)          = getProcedure prcName state
>           newState                   = ((paramTab, emptyMap) : scopes, pStore)
>           (badStmts, postVisitedPrc) = badAsgnArrRefsBlock block newState (prcName : visitedPrc)

> badAsgnArrRefsStmt (If _ c p) state@(scopes, pStore) visitedPrc
>   = (badStmts ++ badStmts', state, visitedPrc)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)
>           (badStmts,  _) = badAsgnArrRefsBlock c newState visitedPrc
>           (badStmts', _) = badAsgnArrRefsBlock p newState visitedPrc

> badAsgnArrRefsStmt (Do _ p) state@(scopes, pStore) visitedPrc
>   = (badStmts, state, visitedPrc)
>     where newState      = ((emptyMap, emptyMap) : scopes, pStore)
>           (badStmts, _) = badAsgnArrRefsBlock p newState visitedPrc

> badAsgnArrRefsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

> badAsgnArrRefsStmt _ state visitedPrc = ([], state, visitedPrc)

badArrRefs:
Checks through a programme whether variables and index expressions have
incorrect type in `ArrRef Var Exp`
(This top-level wrapper method also gets rid of duplicates)

> badArrRefs :: Prog -> [Exp]
> badArrRefs (symTab, pStore, block)
>   = nub badExps
>     where (badExps, _) = badArrRefsBlock block ([(symTab, emptyMap)], pStore) []

badArrRefsBlock:
Checks through a block whether variables and index expressions have incorrect
type in `ArrRef Var Exp`

> badArrRefsBlock :: Block -> State -> [PrcdrName] -> ([Exp], [PrcdrName])
> badArrRefsBlock [] _ visitedPrc = ([], visitedPrc)
> badArrRefsBlock (stmt : stmts) state visitedPrc
>   = (vars ++ vars', postVisitedPrc')
>     where (vars, postState, postVisitedPrc) = badArrRefsStmt stmt state visitedPrc
>           (vars', postVisitedPrc') = badArrRefsBlock stmts postState postVisitedPrc

badArrRefsStmt:
Checks through a statement whether variables and index expressions have
incorrect type in `ArrRef Var Exp`

> badArrRefsStmt :: Stmt -> State -> [PrcdrName] -> ([Exp], State, [PrcdrName])

> badArrRefsStmt Skip state visitedPrc = ([], state, visitedPrc)

> badArrRefsStmt (Asgn _ e) state visitedPrc = (badArrRefsExp e state, state, visitedPrc)

> badArrRefsStmt (AsgnArrRef _ idxExp valExp) state visitedPrc
>   = (badArrRefsExp idxExp state ++ badArrRefsExp valExp state, state, visitedPrc)

> badArrRefsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badExps, state, postVisitedPrc)
>     where (paramTab, block)         = getProcedure prcName state
>           newState                  = ((paramTab, emptyMap) : scopes, pStore)
>           (badExps, postVisitedPrc) = badArrRefsBlock block newState (prcName : visitedPrc)

> badArrRefsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   = (badArrRefsExp e state ++ badExps ++ badExps', state, visitedPrc)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)
>           (badExps,  _) = badArrRefsBlock c newState visitedPrc
>           (badExps', _) = badArrRefsBlock p newState visitedPrc

> badArrRefsStmt (Do e p) state@(scopes, pStore) visitedPrc
>   = (badArrRefsExp e state ++ badExps, state, visitedPrc)
>     where newState     = ((emptyMap, emptyMap) : scopes, pStore)
>           (badExps, _) = badArrRefsBlock p newState visitedPrc

> badArrRefsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

badArrRefsExp:
Checks through an expression whether variables and index expressions have
incorrect type in `ArrRef Var Exp`

> badArrRefsExp :: Exp -> State -> [Exp]

> badArrRefsExp e@(ArrRef v idxExp) state
>   | isVarDeclared v state && (getVarType v state == IntType)  = e : rest
>   | isVarDeclared v state && (getVarType v state == BoolType) = e : rest
>   | expType idxExp state /= IntType                           = e : rest
>   | otherwise                                                 = rest
>   where rest = badArrRefsExp idxExp state

> badArrRefsExp (Bin _ e e') state
>   = badArrRefsExp e state ++ badArrRefsExp e' state

> badArrRefsExp (Una _ e)  state = badArrRefsExp e state

> badArrRefsExp _ _ = []

wrongTypeAsgnmts:
Checks through a programme whether an assignment has mismatched types on two
sides. N.B. this method doesn't check for array reference assignment.
`badAsgnArrRefs` checks for type mismatch in array reference assignments.
(This top-level wrapper method also gets rid of duplicates)

> wrongTypeAsgnmts :: Prog -> [Stmt]
> wrongTypeAsgnmts (symTab, pStore, block)
>   = nub badStmts
>     where (badStmts, _) = wrongTypeAsgnmtsBlock block ([(symTab, emptyMap)], pStore) []

wrongTypeAsgnmtsBlock:
Checks through a bock whether an assignment has mismatched types on two sides.
N.B. this method doesn't check for array reference assignment. `badAsgnArrRefs`
checks for type mismatch in array reference assignments.

> wrongTypeAsgnmtsBlock :: Block -> State -> [PrcdrName] -> ([Stmt], [PrcdrName])
> wrongTypeAsgnmtsBlock [] _ visitedPrc = ([], visitedPrc)
> wrongTypeAsgnmtsBlock (stmt : stmts) state visitedPrc
>   = (badStmts ++ badStmts', postVisitedPrc')
>     where (badStmts, postState, postVisitedPrc) = wrongTypeAsgnmtsStmt stmt state visitedPrc
>           (badStmts', postVisitedPrc') = wrongTypeAsgnmtsBlock stmts postState postVisitedPrc

wrongTypeAsgnmtsStmt:
Checks through a statement whether an assignment has mismatched types on two
sides. N.B. this method doesn't check for array reference assignment.
`badAsgnArrRefs` checks for type mismatch in array reference assignments.

> wrongTypeAsgnmtsStmt :: Stmt -> State -> [PrcdrName] -> ([Stmt], State, [PrcdrName])

> wrongTypeAsgnmtsStmt s@(Asgn v e) state visitedPrc
>   | isVarDeclared v state && (getVarType v state /= expType e state) = ([s], state, visitedPrc)
>   | otherwise = ([], state, visitedPrc)

> wrongTypeAsgnmtsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badStmts, state, postVisitedPrc)
>     where (paramTab, block)          = getProcedure prcName state
>           newState                   = ((paramTab, emptyMap) : scopes, pStore)
>           (badStmts, postVisitedPrc) = wrongTypeAsgnmtsBlock block newState (prcName : visitedPrc)

> wrongTypeAsgnmtsStmt (If _ c p) state@(scopes, pStore) visitedPrc
>   = (badStmts ++ badStmts', state, visitedPrc)
>     where newState = ((emptyMap, emptyMap) : scopes, pStore)
>           (badStmts,  _) = wrongTypeAsgnmtsBlock c newState visitedPrc
>           (badStmts', _) = wrongTypeAsgnmtsBlock p newState visitedPrc

> wrongTypeAsgnmtsStmt (Do _ p) state@(scopes, pStore) visitedPrc
>   = (badStmts, state, visitedPrc)
>     where newState      = ((emptyMap, emptyMap) : scopes, pStore)
>           (badStmts, _) = wrongTypeAsgnmtsBlock p newState visitedPrc

> wrongTypeAsgnmtsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

> wrongTypeAsgnmtsStmt _ state visitedPrc = ([], state, visitedPrc)

misConExps:
Checks through a programme whether the condition expression in If or Do
statement is Int typed or Array typed.
(This top-level wrapper method also gets rid of duplicates)

> misConExps :: Prog -> [Exp]
> misConExps (symTab, pStore, block)
>   = nub badExps
>     where (badExps, _) = misConExpsBlock block ([(symTab, emptyMap)], pStore) []

misConExpsBlock:
Checks through a block whether the condition expression in If or Do statement
is Int typed or Array typed.

> misConExpsBlock :: Block -> State -> [PrcdrName] -> ([Exp], [PrcdrName])
> misConExpsBlock [] _ visitedPrc = ([], visitedPrc)
> misConExpsBlock (stmt : stmts) state visitedPrc
>   = (exps ++ exps', postVisitedPrc')
>     where (exps, postState, postVisitedPrc) = misConExpsStmt stmt state visitedPrc
>           (exps', postVisitedPrc') = misConExpsBlock stmts postState postVisitedPrc

misConExpsStmt:
Checks through a statement whether the condition expression in If or Do
statement is Int typed or Array typed.

> misConExpsStmt :: Stmt -> State -> [PrcdrName] -> ([Exp], State, [PrcdrName])

> misConExpsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badExps, state, postVisitedPrc)
>     where (paramTab, block)         = getProcedure prcName state
>           newState                  = ((paramTab, emptyMap) : scopes, pStore)
>           (badExps, postVisitedPrc) = misConExpsBlock block newState (prcName : visitedPrc)

> misConExpsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   | expType e state /= BoolType = (e : rest, state, visitedPrc)
>   | otherwise                   = (rest, state, visitedPrc)
>   where newState      = ((emptyMap, emptyMap) : scopes, pStore)
>         (badExps, _)  = misConExpsBlock c newState visitedPrc
>         (badExps', _) = misConExpsBlock p newState visitedPrc
>         rest          = badExps ++ badExps'

> misConExpsStmt (Do e p) state@(scopes, pStore) visitedPrc
>   | expType e state /= BoolType = (e : badExps, state, visitedPrc)
>   | otherwise                   = (badExps, state, visitedPrc)
>   where newState     = ((emptyMap, emptyMap) : scopes, pStore)
>         (badExps, _) = misConExpsBlock p newState visitedPrc

> misConExpsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

> misConExpsStmt _ state visitedPrc = ([], state, visitedPrc)

expType:
Find out the type of an expression

> expType :: Exp -> State -> Type

> expType (ConstInt  _) _ = IntType
> expType (ConstBool _) _ = BoolType

> expType (Var v) state
>   | isVarDeclared v state = getVarType v state
>   | otherwise             = error ("Undeclared variable " ++ show v)  -- shouldn't come here if we do static checking before running.

> expType (ArrRef v _) state
>   | isVarDeclared v state = let (ArrayType tipe) = getVarType v state in tipe
>   | otherwise             = error ("Undeclared variable " ++ show v)  -- shouldn't come here if we do static checking before running.

> expType (Bin op x y) state = binOpType op (expType x state) (expType y state)
> expType (Una op x) state   = unaOpType op (expType x state)

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


-----------------------------------------------------
                Functions for State
-----------------------------------------------------


A state represents a snapshot of variables and stored procedures during the
execution of programme. It's a composition of multiple layers of variable scope
where each layer is a pair of symbol table and variable store, and a procedure
store. The layers of scopes are like a Stack. The access rule is FILO.

The purpose of using State is to simulate a scoped environment when executing
programmes. More specifically, by using State type, we can easily seperate
variables available in current scope and variables above current scope. Using
State also allow local variables to shadow global variables.

(Ideally, I want to implement the state as a separated module. But then it
brings recursive import which I can't solve. So I have to put it in the same
file. This is a monster huge file...)


getVarType:
Given a varible name, return the type of it. The search order from inside out.

> getVarType :: Var -> State -> Type
> getVarType v ([], _) = error ("Variable not declared: " ++ show v)
> getVarType v ((symTab, _) : scopes, pStore)
>   | hasKey v symTab = getVal v symTab
>   | otherwise       = getVarType v (scopes, pStore)

getVarValue:
Given a varible name, return the value of it. The search order is local first
and global last.

> getVarValue :: Var -> State -> Val
> getVarValue v ([], _) = error ("Variable not initialised: " ++ show v)
> getVarValue v ((_, vStore) : scopes, pStore)
>   | hasKey v vStore = getVal v vStore
>   | otherwise       = getVarValue v (scopes, pStore)

isVarDeclared:
Has this var been declared?

> isVarDeclared :: Var -> State -> Bool
> isVarDeclared _ ([], _) = False
> isVarDeclared v ((symTab, _) : scopes, pStore)
>   | hasKey v symTab = True
>   | otherwise       = isVarDeclared v (scopes, pStore)

isVarDeclaredCurrentScope:
Has this var been declared in current scope?

> isVarDeclaredCurrentScope :: Var -> State -> Bool
> isVarDeclaredCurrentScope _ ([], _) = error "Cannot find global scope! Scope stack size is 0!"
> isVarDeclaredCurrentScope v ((symTab, _) : _, _)
>   | hasKey v symTab = True
>   | otherwise       = False

isVarInitialised:
Has this var been initialised?

> isVarInitialised :: Var -> State -> Bool
> isVarInitialised _ ([], _) = False
> isVarInitialised v ((_, vStore) : scopes, pStore)
>   | hasKey v vStore = True
>   | otherwise       = isVarInitialised v (scopes, pStore)

declareVar:
declare a variable in current scope

> declareVar :: Var -> Type -> State -> State
> declareVar _ _ ([], _) = error "Cannot find global scope! Scope stack size is 0!"
> declareVar var tipe ((symTab, vStore) : scopes, pStore)
>   | hasKey var vStore = error ("Cannot re-declare variable: " ++ show var)
>   | otherwise         = ((setVal var tipe symTab, vStore) : scopes, pStore)

assignVar:
Assign a variable with a value

> assignVar :: Var -> Val -> State -> State
> assignVar var _ ([], _) = error ("Variable not declared: " ++ show var)
> assignVar var val (scope@(symTab, vStore) : scopes, pStore)
>   | hasKey var symTab = ((symTab, setVal var val vStore) : scopes, pStore)
>   | otherwise         = (scope : newScopes, pStore)
>   where (newScopes, _) = assignVar var val (scopes, pStore)

getProcedure:
Given a procedure name, return the stored procedure.

> getProcedure :: PrcdrName -> State -> Prcdr
> getProcedure pName (_, pStore)
>   | hasKey pName pStore = getVal pName pStore
>   | otherwise           = error ("Procedure not defined: " ++ show pName)

isProcDefined:
Is this procedure defined?

> isProcDefined :: PrcdrName -> State -> Bool
> isProcDefined pName (_, pStore) = hasKey pName pStore

getGlobalVarStore:
Get the top layer variable store

> getGlobalVarStore :: State -> VarStore
> getGlobalVarStore (scopes, _) = vStore where (_, vStore) = last scopes


-----------------------------------------------------
                     Test cases
-----------------------------------------------------


Firstly here is a slightly more complex test programme. This programme covers
most defined statements, expressions, operators, etc. It can be executed without
error thrown. Though it's not a test case, it surely proves that the programme
can run :)

A Java equivalent version is also provided:

Initial state:

    // int variables
    int a = 9999;
    int b = 5;
    int c = 0;

    // boolean variables
    boolean h = false;
    boolean i = false;

    // a one dimension array of int
    int[] o = new int[4] { 0, 1, 2, 3 };

    // a two dimension array, array of array of boolean
    // sadly currently we only support having it in store :(
    boolean[][] p = {
        { true },
        { false, true },
        { true, false, true },
    }

> s0 :: VarStore
> s0 = [
>        ('a', Int 9999),
>        ('b', Int 5),
>        ('c', Int 0),
>        ('h', Bool False),
>        ('i', Bool False),
>        ('o', Arr arr1 4 IntType),
>        ('p', Arr arr2 3 (ArrayType BoolType))
>      ]
>      where arr1 = array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]
>            arr2 = array (0,2) [
>                     (0, Arr (array (0,0) [(0, Bool True)]) 1 BoolType),
>                     (1, Arr (array (0,1) [(0, Bool False), (1, Bool True)]) 2 BoolType),
>                     (2, Arr (array (0,2) [(0, Bool True), (1, Bool False), (2, Bool True)]) 3 BoolType)
>                   ]

> t0 :: SymTab
> t0 = [
>        ('a', IntType),
>        ('b', IntType),
>        ('c', IntType),
>        ('h', BoolType),
>        ('i', BoolType),
>        ('o', ArrayType IntType),
>        ('p', ArrayType (ArrayType BoolType))
>      ]

Procedure definition:

    // parameter h shadows global variable h.
    // sadly my approach allow the programme to modify parameters :(
    // this procedure also calls itself.
    winwin(boolean h, int v) {
        if (c == 3) {
           return;  // using Skip
        } else {
            // modify a local variable (parameter actually)
            h = false;

            // another assignment on local variable
            v = 500 + 300;

            // declare a variable within the scope of winwin procedure
            int u;

            // assign the newly declared variable using a global variable.
            u = a;

            // modify the global array o.
            o[2] = o[2] ^ 2;

            // modify the global variable b.
            c = c + 1

            // call winwin()
            winwin(true, 555);
        }
    }

> pStore0 :: PrcdrStore
> pStore0 = [("winwin",
>            ([
>               ('h', BoolType),
>               ('v', IntType)
>             ],
>             [
>                If (Bin Eq (Var 'c') (ConstInt 3))
>                   [Skip]
>                   [
>                      Asgn 'h' (ConstBool False),
>                      Asgn 'v' (Bin Plus (ConstInt 500) (ConstInt 300)),
>                      Declare 'u' IntType,
>                      Asgn 'u' (Var 'a'),
>                      AsgnArrRef 'o' (ConstInt 2) (Bin Power (ArrRef 'o' (ConstInt 2)) (ConstInt 2)),
>                      Asgn 'c' (Bin Plus (Var 'c') (ConstInt 1)),
>                      InvokePrcdr "winwin" [('h', Bool True), ('v', Int 555)]
>                   ]
>             ])
>            )]

Programme:

    // variable assignment, unary operation Not, variable as expression.
    h = !i;

    // assignment
    i = h;

    // variable declaration at global scope
    Int u;

    // array reference, array reference assignment, binary operation Times
    o[0] = o[2] * 10;

    // a skip statement here.
    // Java has no such statement.

    // if statement, binary operation And
    if (h && i) {
        a = 5;
    } else {
        a = 0;
    }

    // do statement, binary operation Lt
    while (b > 1) {
        // binary operation Minus, self-decrement
        b = b - 1;
    }

    // assignment on newly declared variable.
    // variable u is soon to be shadowed in procedure winwin, so it's a good
    // test on scoping.
    u = b;

    // procedure invoked using parameters: (h: true, s: 555)
    winwin(true, 555);

> p0 :: Prog
> p0 = (
>   t0,
>   pStore0,
>   [
>     Asgn 'h' (Una Not (Var 'i')),
>     Asgn 'i' (Var 'h'),
>     Declare 'u' IntType,
>     AsgnArrRef 'o' (ConstInt 0) (Bin Times (ArrRef 'o' (ConstInt 2)) (ConstInt 10)),
>     Skip,
>     If (Bin And (Var 'h') (Var 'i')) [Asgn 'a' (ConstInt 5)] [Asgn 'a' (ConstInt 0)],
>     Do (Bin Gt (Var 'b') (ConstInt 1)) [Asgn 'b' (Bin Minus (Var 'b') (ConstInt 1))],
>     Asgn 'u' (Var 'b'),
>     InvokePrcdr "winwin" [('h', Bool True), ('v', Int 555)]
>   ])

If everything is right, after the execution, the store should be:

    // int variables
    int a = 5;
    int b = 1;
    int c = 3;
    int u = 1;

    // boolean variables
    boolean h = true;
    boolean i = true;

    // a one dimension array of int
    int[] o = new int[4] { 20, 1, 256, 3 };

    // the two dimension array is not changed, array of array of boolean
    boolean[][] p = {
        { true },
        { false, true },
        { true, false, true },
    }


And let there be test cases:





I don't know how to assert on a expected error thrown in Haskel, so for `run`
function, I manually tested all error-checking guards.
