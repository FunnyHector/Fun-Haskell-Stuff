-----------------------------------------------------
                 General discussion
-----------------------------------------------------

What I did:
- part 1:
  - Boolean variables and constants
  - Boolean operators (&&, ||, !)
  - Variables are required to be declared (as a symbol table)
  - Initial state is represented as a symbol table and a variable store, and the
    programme is represented as a list of statements.
  - No duplicate variables in symbol table or intial variable store.
  - The definition of expression and conditional are combined
  - Static check for whether operatores are applied with correct expressions.
  - Static check for uninitialised variables
  - Static check for undeclared variables
  - Static check for mismatched types in assignment statement
  - Static check to ensure boolean typed expressions are used for condition in
    control statements (If and Do)

- part 2:
  - Support fixed size arrays (using Haskell Array).
  - Array can hold int, boolean, or array values.
  - Assign or access array element using array variable and index
  - Static check to ensure array variables are used as array reference (i.e.
    followed by an index) and non-array variables are not used in this way.
  - Static check for mismatched types in array reference expression.
  - Static check for mismatched types in array reference assignment.

- part 3:
  - Support procedures. A procedure has a name, a symbol table to specify the
    types of parameters, and a block (list of statements).
  - Support invoking procedures with its name and a variable store specifying
    the values of parameters. Procedure invocation can be called inside a
    procedure.
  - A programme now is the composition of a symbol table, a procedure store,
    and a block.
  - Support variable scopes. The execution uses a new scope when reaching a
    procedure, a block in If statement, or a block in Do statement. See
    discussion below about how the scope is implemented.
  - Support variable declarations. Variables can be declared in the programme or
    in procedures. The newly declared variable exists in the scope of current
    execution.
  - Static check for undefined procedures.
  - Static check for redeclared variables in current scope.
  - Static check for wrong parameters passed when invoking procedures.
  - Bonus: Map is extended a little as well.

- part 4:
  - Nothing
    (I Don't really think anyone could handle such big workload. This is just
    not manageable for markers as well. ¯\_(ツ)_/¯)


What I didn't do:
- part 2:
  - Although multi-dimension array is supported in variable store and symbol
    table, the programme does not support access or update elements in n-th
    layer (n > 1). See discussion point below.
  - "Display the final values of the variables in a more readable way". Not sure
    if I understand this optional requirement.

- part 4:
  - The whole part 4


Discussion points:

- multi dimension arrays:
  Although multi dimension array can be stored in the initial store and the
  symbol table, and as well as the static checks for types works for multi
  dimension array, it cannot be read or updated. I think I know how to do this,
  but time is really tight for such a big assignment. A possible solution is to
  change the statement `AsgnArrRef Var Exp Exp` to `AsgnArrRef Exp Exp Exp`,
  and change the expression `ArrRef Var Exp` to `ArrRef Exp Exp`. This allows
  us to recursively access a multi-dimension array like
  `ArrRef (ArrRef (Var 'x') (Int 0)) (Int 0)`. If I had more time, this would
  be where I'm heading.

- Procedures:
  Procedure is not difficult to implement. The hard part lives in how to do
  static checks after we introduced procedures. Procedures can invoke procedures,
  and this will lead to infinite loops if we don't track visited procedures.
  The general idea is to treat procedure invocation as a block execution with
  parameters as local variables in a new scope, and track the visited procedures
  when we do static checkings. It's a lot more code refactoring than this quick
  explanation here.

- Implementation of scopes:
  Scopes are stored as a stack from bottom up. The execution is always in the
  top-most scope. Every time the execution enters a new block, a new scope is
  created on top of the stack. The access rule is FILO.
  The retrieving and updating can sink down to lower scopes if the variable is
  not in current scope. However, the creation is limited in current scope, and
  able to shadow variables from lower scopes.

  There are limitations in this approach: 1) Parameters can be modified. This
  is forbidden in Java, however allowed (slightly differently) in dynamically
  typed languages like Ruby and JavaScript. 2) Theoretically, if a procedure A
  is invoked inside another procedure B, the execution can modify variables in
  B from inside A. This isn't quite right.

- state
  To implement scoping mechanism, I restructured the programme, and created
  State to represent the memory snapshot before executing each statement. A
  state is a composition of scopes and procedures.

  The purpose of using State is to simulate a scoped environment when executing
  programmes. More specifically, by using State type, we can easily seperate
  variables available in current scope and variables below current scope.
  Ideally, I want to implement the state as a separated module. But then it
  brings recursive import problem which I can't solve. So I have to put it in
  the same file. This is a monster huge file...

- Massive static checks in part 3:
  At the moment there are 15 functions run in order before executing the
  programme to do the static checkings (see the explanation before function
  `run`). I built them up from part 1 to part 3. If we review them now, check
  no. 1, 2, and 3 should be redundant, because after I implemented variable
  declaration, all checks need to keep track of newly declared variables so the
  logic is totally re-written, and in doing this check no 1, 2, and 3 become
  obselete. I still keep these checks just so we can prevent errors as early as
  possible.

  In many of these checks, I used `nub` to remove duplicates. This isn't quite
  right, e.g. there can be multiple occurrence of same variable that is not
  declared. However, without tracking the whereabouts of these variables,
  displaying same variable multiple times in error message is meaningless. So I
  nub'd them. Afterall this is still a working mechanism of static checking.

- Can we statically check whether an array reference is using an index out of
  boundary?
  Not very feasible, at least we can't if we allow expressions for index. If we
  only allow constant int values to be used as index, checking this exception
  can easily be done.
  In order to check the boundary, we have to evaluate the final value of the
  index, but during the evaluation, we can't guarantee that the index expression
  does not attempt to evaluate another array reference. In other words, we can't
  safely evaluate the index expression in compile time, hence can't check for
  index out of boundary exceptions. In fact, in all the statically typed
  languages that I know, index out of boundary exceptions are thrown during
  runtime.

- Can we statically check if an array reference is accessing an uninitialised
  element?
  My guess for this question would also be no. The reason is similar to above
  question. It's just difficult to evaluate an index value out of an expression
  without triggering potential errors. Aside from that reason, there is one
  more risk: Haskel Array throwns exception if we access an element that is not
  there, and I don't know how to catch the exception in Haskel yet. An ideal
  solution would be:

    -- say we have an array `arr`, and it has no element at index 5
    eval (ArrRef arr 5) vStore
      | catchedException "undefined array element" = NULL
      | otherwise                                  = arr ! 5

  This is only to show the idea. Please ignore the totally-wont-work syntax :P
  A lot of other languages uses default values for this operation, e.g. Java
  gives back null for array of objects or 0 for array of primitive integers. But
  That solution brings lots of controversial discussions as well.

-----------------------------------------------------
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
`sort` function is used in testing whether two arrays have same elements

> import Data.List (nub, intersperse, sort)

Array is used to back up the array implementation in While.

> import Data.Array

Maybe is used to deal with error condition in type checking

> import Data.Maybe

Variable names are assumed to be single characters.

> type Var = Char

Procedure names are assumed to be strings

> type PrcdrName = String

Values are assumed to be integers/boolean/array.

> data Val = Int Int
>          | Bool Bool
>          | Arr (Array Int Val) Int Type  -- 3 params: array, size, type
>          deriving (Eq, Ord, Show)

A block is a list of statements.

> type Block = [Stmt]

A procedure is a list of statements, and a symbol table to specify the type of
parameters. A procedure can be invoked using the name.

> type Prcdr = (SymTab, Block)

A program is a symbol table, a procedure store, and a list of statements

> type Prog = (SymTab, PrcdrStore, Block)

A statement can be a skip, assignment, array reference assignment, procedure
invocation, if statement, do statement, or variable declaration.

> data Stmt = Skip
>           | Asgn Var Exp
>           | AsgnArrRef Var Exp Exp
>           | InvokePrcdr PrcdrName VarStore
>           | If Exp Block Block
>           | Do Exp Block
>           | Declare Var Type
>           deriving (Eq, Ord, Show)

An expression can be a constant(int/bool), a variable, an array reference, a
binary operator applied to two expressions, or a unary operator applied to one
expression.

> data Exp = ConstInt Int
>          | ConstBool Bool
>          | Var Var
>          | ArrRef Var Exp
>          | Bin BOp Exp Exp
>          | Una UOp Exp
>          deriving (Eq, Ord, Show)

An binary operation can be arithmetic (+, -, *, /, ^), relational (=, /=, <,
<=, > or >=), or boolean (&&, ||)

> data BOp = Plus | Minus | Times | Div | Power |
>            Eq | Ne | Lt | Le | Gt | Ge |
>            And | Or
>            deriving (Eq, Ord, Show)

Unary operation at the moment only contains boolean operation Not (!)

> data UOp = Not deriving (Eq, Ord, Show)

Type is the type of a value. We can use it to check whether an expression has
correct type (Int, Bool, or Array).

> data Type = IntType
>           | BoolType
>           | ArrayType Type -- recursive type
>           deriving (Eq, Ord, Show)

A variable store is a map from variables to values

> type VarStore = Map Var Val

A symbol table is a map from varibales to types

> type SymTab = Map Var Type

A procedure store is a map from procedure name to procedures

> type PrcdrStore = Map PrcdrName Prcdr

State is a representation of a state during the execution of programme. A state
contains:
  1) a list (more accurately stack) of pairs of symbol table and variable store.
     Each pair represents the local variables in a scope.
  2) procedure store

> type State = ([(SymTab, VarStore)], PrcdrStore)

A result is like Either, it's either a good result or an error with message
(Not used so far)

> data Result a = OK a | Err String

run:
To run a program with a given initial var store, we first do a series of static
checking:

1. The symbol table and the variable store matches each other.
   1.1. All declared variables are initialised. (dclrdButNotInitVars)
   1.2. All initialised variables are declared. (intiButNotdclrdVars)
   1.3. Types are matched. (typeMismatchedVars)
   1.4. No duplicate varibles existing in symbol table or initial store.
        (duplicateVars)

2. All procedure being invoked are defined in procedure store. (undefinedPrcdrs)

3. All variables used are declared with a type. (undeclaredVars)

4. Variables cannot be re-declared. (reDeclarations)

5. All variables used in expression have values initialised. (uninitVars)

6. The parameters passed in matches the defined type. (incorrectParamInvocs)
   This check is essentially check no. 1 applied on procedures.

7. Array related check:
   7.1. Array variable is not used alone, i.e. have to with an index. (arrVars)
   7.2. Var in `ArrRef Var Exp` is ArrayType. (badArrRefs)
   7.3. The Exp in `ArrRef Var Exp` is IntType. (badArrRefs)
   7.4. Var in `AsgnArrRef Var Exp Exp` is ArrayType. (badAsgnArrRefs)
   7.5. The first Exp (index expression) in `AsgnArrRef Var Exp Exp` is IntType.
        (badAsgnArrRefs)
   7.6. The second Exp (value expression) in `AsgnArrRef Var Exp Exp` matches
        the type of elements in this array. (badAsgnArrRefs)

8. All binary and unary operations are correctly applied. (badOps)

9. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. N.B. this check does
   not check for `AsgnArrRef Var Exp Exp`. (wrongTypeAsgnmts)

10. Conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer/array values end up as condition expression of If or
   Do. (misConExps)

These checks are done in order, which eliminates the possibilities to blow up
bit by bit as each step goes. This also means that some errors have to be fixed
before other errors can be detected. In fact, in many statically typed
languages, errors are checked in stages as well.

Note that during these checks, there still could be runtime error thrown. Can
never say I eliminated all errors....

If the programme is all good, then we create a global state, and pass the
statements and the global state to `exec`.

> run :: Prog -> VarStore -> VarStore
> run p@(symTab, pStore, block) vStore
>     -- these checks have really bad names, see the `where` clause for more info
>   | not $ null check11
>       = error ("\nDeclared but not initialised Variable(s): " ++ showVars check11)
>   | not $ null check12
>       = error ("\nInitialised but not declared Variable(s): " ++ showVars check12)
>   | not $ null check13
>       = error ("\nMismatched type in variable store and symbol table: " ++ showVars check13)
>   | not $ null check14
>       = error ("\nDuplicates found in variable store or symbol table: " ++ showVars check14)
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
>   | not $ null check723
>       = error ("\nType errors in array reference(s): " ++ show check723)
>   | not $ null check7456
>       = error ("\nType errors in array reference assignment(s): " ++ show check7456)
>   | not $ null check8
>       = error ("\nWrong operator(s) applied: " ++ show check8)
>   | not $ null check9
>       = error ("\nType errors in Assignment(s): " ++ show check9)
>   | not $ null checkX
>       = error ("\nIncorrect Expression(s) as control condition: " ++ show checkX)
>   | otherwise
>       = getGlobalVarStore postState -- the non-error scenario
>   where check11   = dclrdButNotInitVars p vStore    -- check no. 1.1
>         check12   = intiButNotdclrdVars p vStore    -- check no. 1.2
>         check13   = typeMismatchedVars p vStore     -- check no. 1.3
>         check14   = duplicateVars p vStore          -- check no. 1.4
>         check2    = undefinedPrcdrs p               -- check no. 2
>         check3    = undeclaredVars p                -- check no. 3
>         check4    = reDeclarations p                -- check no. 4
>         check5    = uninitVars p vStore             -- check no. 5
>         check6    = incorrectParamInvocs p          -- check no. 6
>         check71   = arrVars p                       -- check no. 7.1
>         check723  = badArrRefs p                    -- check no. 7.2 & 7.3
>         check7456 = badAsgnArrRefs p                -- check no. 7.4, 7.5, & 7.6
>         check8    = badOps p                        -- check no. 8
>         check9    = wrongTypeAsgnmts p              -- check no. 9
>         checkX    = misConExps p                    -- check no. 10
>         postState = exec block ([(symTab, vStore)], pStore)
>         showVars  = intersperse ','

exec:
To execute a block, we just execute each statement in turn, passing the
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

to execute a procedure, we use parameters as a new scope, and run the procedure.
After the procedure is finished, we do not care about any change in the new
scope. In other word, we only keep changes in existing scopes.

> exec' (InvokePrcdr prcName paramStore) state@(scopes, pStore)
>   = (postScopes, postStore)
>     where (paramTab, block)           = getProcedure prcName state
>           newState                    = ((paramTab, paramStore) : scopes, pStore)
>           (_ : postScopes, postStore) = exec block newState

to execute a If/Do block is like to execute a block with a new empty scope

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

What we need to statically check:

1. The symbol table and the variable store matches each other.
   1.1. All declared variables are initialised. (dclrdButNotInitVars)
   1.2. All initialised variables are declared. (intiButNotdclrdVars)
   1.3. Types are matched. (typeMismatchedVars)
   1.4. No duplicate varibles existing in symbol table or initial store.
        (duplicateVars)

2. All procedure being invoked are defined in procedure store. (undefinedPrcdrs)

3. All variables used are declared with a type. (undeclaredVars)

4. Variables cannot be re-declared. (reDeclarations)

5. All variables used in expression have values initialised. (uninitVars)

6. The parameters passed in matches the defined type. (incorrectParamInvocs)
   This check is essentially check no. 1 applied on procedures.

7. Array related check:
   7.1. Array variable is not used alone, i.e. have to with an index. (arrVars)
   7.2. Var in `ArrRef Var Exp` is ArrayType. (badArrRefs)
   7.3. The Exp in `ArrRef Var Exp` is IntType. (badArrRefs)
   7.4. Var in `AsgnArrRef Var Exp Exp` is ArrayType. (badAsgnArrRefs)
   7.5. The first Exp (index expression) in `AsgnArrRef Var Exp Exp` is IntType.
        (badAsgnArrRefs)
   7.6. The second Exp (value expression) in `AsgnArrRef Var Exp Exp` matches
        the type of elements in this array. (badAsgnArrRefs)

8. All binary and unary operations are correctly applied. (badOps)

9. All expressions used in assignments have correct type. The type of righthand
   expression is same as the type of lefthand variable. N.B. this check does
   not check for `AsgnArrRef Var Exp Exp`. (wrongTypeAsgnmts)

10. Conditional expressions for If or Do statement are used correctly. This is
   to make sure no integer/array values end up as condition expression of If or
   Do. (misConExps)

---------------------    check 1.1    ---------------------

dclrdButNotInitVars:
Compare the symbol table and the variable store, and find out all variables that
is declared but not initialised (exists in symbol table but not in variable
store).
(naming is hard....Orz)

> dclrdButNotInitVars :: Prog -> VarStore -> [Var]
> dclrdButNotInitVars (t, _, _) s
>   = filter (`notElem` varsStore) varsTable
>     where varsStore = keys s
>           varsTable = keys t

---------------------    check 1.2    ---------------------

intiButNotdclrdVars:
Compare the symbol table and the variable store, and find out all variables that
is initialised but not declared (exists in variable store but not in symbol
table).

> intiButNotdclrdVars :: Prog -> VarStore -> [Var]
> intiButNotdclrdVars (t, _, _) s
>   = filter (`notElem` varsTable) varsStore
>     where varsStore = keys s
>           varsTable = keys t

---------------------    check 1.3    ---------------------

typeMismatchedVars:
Compare the symbol table and the variable store, and find out all variables that
have mismatched type.
If we run the checks in order, we can guarantee that symbol table and variable
store have same variables now (by same I mean the name of it). We only need to
check the type now.

> typeMismatchedVars :: Prog -> VarStore -> [Var]
> typeMismatchedVars (table, _, _) vStore
>   = keys $ filter (\(k,v) -> not $ isSameType v (getVal k table)) vStore

---------------------    check 1.4    ---------------------

duplicateVars:
Check if there are duplicate variables in store or symbol table.

> duplicateVars :: Prog -> VarStore -> [Var]
> duplicateVars (table, _, _) store
>   = nub $ check (keys table) [] [] ++ check (keys store) [] []
>     where check [] _ dups = dups
>           check (x:xs) seen dups
>             | x `elem` seen = x : dups
>             | otherwise     = check xs (x:seen) dups

----------------------    check 2    ----------------------

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

----------------------    check 3    ----------------------

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
>   where (paramTab, block)         = getProcedure prcName state
>         newState                  = ((paramTab, emptyMap) : scopes, pStore)
>         (badVars, postVisitedPrc) = undeclaredVarsBlock block newState (prcName : visitedPrc)

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

----------------------    check 4    ----------------------

reDeclarations:
Find out all attempts to re-declare variables.

> reDeclarations :: Prog -> [Stmt]
> reDeclarations (symTab, pStore, block)
>   = nub $ badStmts ++ badStmtsFromPrcs
>     where badStmts = reDeclarationsBlock block ([(symTab, emptyMap)], pStore)
>           prcdrs = map snd pStore
>           badStmtsFromPrcs = concatMap (\(paramTab, pBlock) -> reDeclarationsBlock pBlock ([(paramTab, emptyMap), (symTab, emptyMap)], pStore)) prcdrs

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

----------------------    check 5    ----------------------

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
>   where (_, block)                = getProcedure prcName state
>         newState                  = ((emptyMap, params) : scopes, pStore)
>         (badVars, postVisitedPrc) = uninitVarsBlock block newState (prcName : visitedPrc)

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

----------------------    check 6    ----------------------

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

---------------------    check 7.1    ---------------------

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
>   where (paramTab, block)         = getProcedure prcName state
>         newState                  = ((paramTab, emptyMap) : scopes, pStore)
>         (badVars, postVisitedPrc) = arrVarsBlock block newState (prcName : visitedPrc)

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

------------------    check 7.2 & 7.3    ------------------

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
>   where (paramTab, block)         = getProcedure prcName state
>         newState                  = ((paramTab, emptyMap) : scopes, pStore)
>         (badExps, postVisitedPrc) = badArrRefsBlock block newState (prcName : visitedPrc)

> badArrRefsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   = (badArrRefsExp e state ++ badExps ++ badExps', state, visitedPrc)
>     where newState      = ((emptyMap, emptyMap) : scopes, pStore)
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
>   | expType idxExp state /= Just IntType                      = e : rest
>   | otherwise                                                 = rest
>   where rest = badArrRefsExp idxExp state

> badArrRefsExp (Bin _ e e') state
>   = badArrRefsExp e state ++ badArrRefsExp e' state

> badArrRefsExp (Una _ e) state = badArrRefsExp e state

> badArrRefsExp _ _ = []

---------------    check 7.4 & 7.5 & 7.6    ---------------

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
>   | expType idxExp state /= Just IntType = ([s], state, visitedPrc)
>   | expType valExp state /= Just tipe = ([s], state, visitedPrc)
>   | otherwise = ([], state, visitedPrc)
>   where (ArrayType tipe) = getVarType v state

> badAsgnArrRefsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badStmts, state, postVisitedPrc)
>   where (paramTab, block)          = getProcedure prcName state
>         newState                   = ((paramTab, emptyMap) : scopes, pStore)
>         (badStmts, postVisitedPrc) = badAsgnArrRefsBlock block newState (prcName : visitedPrc)

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

----------------------    check 8    ----------------------

badOps:
Check through all binary and unary operations in a programme for incorrect
application.
(This top-level wrapper method also gets rid of duplicates)

> badOps :: Prog -> [Exp]
> badOps (symTab, pStore, block)
>   = nub badExps
>     where (badExps, _) = badOpsBlock block ([(symTab, emptyMap)], pStore) []

badOpsBlock:
Check through all binary and unary operations in a block for incorrect
application.

> badOpsBlock :: Block -> State -> [PrcdrName] -> ([Exp], [PrcdrName])
> badOpsBlock [] _ visitedPrc = ([], visitedPrc)
> badOpsBlock (stmt : stmts) state visitedPrc
>   = (vars ++ vars', postVisitedPrc')
>     where (vars, postState, postVisitedPrc) = badOpsStmt stmt state visitedPrc
>           (vars', postVisitedPrc') = badOpsBlock stmts postState postVisitedPrc

badOpsStmt:
Check through all binary and unary operations in a statement for incorrect
application.

> badOpsStmt :: Stmt -> State -> [PrcdrName] -> ([Exp], State, [PrcdrName])

> badOpsStmt Skip state visitedPrc = ([], state, visitedPrc)

> badOpsStmt (Asgn _ e) state visitedPrc = (badOpsExp e state, state, visitedPrc)

> badOpsStmt (AsgnArrRef _ idxExp valExp) state visitedPrc
>   = (badOpsExp idxExp state ++ badOpsExp valExp state, state, visitedPrc)

> badOpsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badExps, state, postVisitedPrc)
>   where (paramTab, block)         = getProcedure prcName state
>         newState                  = ((paramTab, emptyMap) : scopes, pStore)
>         (badExps, postVisitedPrc) = badOpsBlock block newState (prcName : visitedPrc)

> badOpsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   = (badOpsExp e state ++ badExps ++ badExps', state, visitedPrc)
>     where newState      = ((emptyMap, emptyMap) : scopes, pStore)
>           (badExps,  _) = badOpsBlock c newState visitedPrc
>           (badExps', _) = badOpsBlock p newState visitedPrc

> badOpsStmt (Do e p) state@(scopes, pStore) visitedPrc
>   = (badOpsExp e state ++ badExps, state, visitedPrc)
>     where newState     = ((emptyMap, emptyMap) : scopes, pStore)
>           (badExps, _) = badOpsBlock p newState visitedPrc

> badOpsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

badOpsExp:
Check through all binary and unary operations in an expression for incorrect
application.

> badOpsExp :: Exp -> State -> [Exp]

> badOpsExp (ArrRef _ idxExp) state = badOpsExp idxExp state

> badOpsExp e@(Bin op x y) state
>   | isNothing $ binOpType op (expType x state) (expType y state) = e : rest
>   | otherwise = rest
>   where rest = badOpsExp x state ++ badOpsExp y state

> badOpsExp e@(Una op x) state
>   | isNothing $ unaOpType op (expType x state) = e : rest
>   | otherwise = rest
>   where rest = badOpsExp x state

> badOpsExp _ _ = []

----------------------    check 9    ----------------------

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
>   | isVarDeclared v state && (Just (getVarType v state) /= expType e state) = ([s], state, visitedPrc)
>   | otherwise = ([], state, visitedPrc)

> wrongTypeAsgnmtsStmt (InvokePrcdr prcName _) state@(scopes, pStore) visitedPrc
>   | prcName `elem` visitedPrc = ([], state, visitedPrc)
>   | otherwise                 = (badStmts, state, postVisitedPrc)
>   where (paramTab, block)          = getProcedure prcName state
>         newState                   = ((paramTab, emptyMap) : scopes, pStore)
>         (badStmts, postVisitedPrc) = wrongTypeAsgnmtsBlock block newState (prcName : visitedPrc)

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

----------------------    check 10    ----------------------

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
>   where (paramTab, block)         = getProcedure prcName state
>         newState                  = ((paramTab, emptyMap) : scopes, pStore)
>         (badExps, postVisitedPrc) = misConExpsBlock block newState (prcName : visitedPrc)

> misConExpsStmt (If e c p) state@(scopes, pStore) visitedPrc
>   | expType e state /= Just BoolType = (e : rest, state, visitedPrc)
>   | otherwise                        = (rest, state, visitedPrc)
>   where newState      = ((emptyMap, emptyMap) : scopes, pStore)
>         (badExps, _)  = misConExpsBlock c newState visitedPrc
>         (badExps', _) = misConExpsBlock p newState visitedPrc
>         rest          = badExps ++ badExps'

> misConExpsStmt (Do e p) state@(scopes, pStore) visitedPrc
>   | expType e state /= Just BoolType = (e : badExps, state, visitedPrc)
>   | otherwise                        = (badExps, state, visitedPrc)
>   where newState     = ((emptyMap, emptyMap) : scopes, pStore)
>         (badExps, _) = misConExpsBlock p newState visitedPrc

> misConExpsStmt (Declare var tipe) state visitedPrc
>   = ([], declareVar var tipe state, visitedPrc)

> misConExpsStmt _ state visitedPrc = ([], state, visitedPrc)

-------    helper functions for static checks    -------

expType:
Find out the type of an expression

> expType :: Exp -> State -> Maybe Type

> expType (ConstInt  _) _ = Just IntType
> expType (ConstBool _) _ = Just BoolType

> expType (Var v) state
>   | isVarDeclared v state = Just (getVarType v state)
>   | otherwise             = Nothing

> expType (ArrRef v _) state
>   | isVarDeclared v state = let (ArrayType tipe) = getVarType v state in Just tipe
>   | otherwise             = Nothing

> expType (Bin op x y) state = binOpType op (expType x state) (expType y state)
> expType (Una op x) state   = unaOpType op (expType x state)

binOpType:
Find out the type of an binary operation

> binOpType :: BOp -> Maybe Type -> Maybe Type -> Maybe Type
> binOpType op (Just IntType) (Just IntType)
>   | op `elem` [Plus, Minus, Times, Div, Power] = Just IntType
> binOpType op (Just IntType) (Just IntType)
>   | op `elem` [Eq, Ne, Lt, Le, Gt, Ge] = Just BoolType
> binOpType op (Just BoolType) (Just BoolType)
>   | op `elem` [Eq, Ne, And, Or] = Just BoolType
> binOpType _ _ _ = Nothing

unaOpType:
Find out the type of an unary operation

> unaOpType :: UOp -> Maybe Type -> Maybe Type
> unaOpType _ (Just BoolType) = Just BoolType
> unaOpType _ _        = Nothing

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

stateEqual:
Compare two states. Order doesn't matter here.

> stateEqual :: State -> State -> Bool
> stateEqual (scopes1, pStore1) (scopes2, pStore2)
>   | pStore1 === pStore2 = scopesEqual scopes1 scopes2
>   | otherwise           = False

scopesEqual:
Compare two scopes of variables. For each table in each scope, order doesn't
matter.

> scopesEqual :: [(SymTab, VarStore)] -> [(SymTab, VarStore)] -> Bool
> scopesEqual [] [] = True
> scopesEqual [] _  = False
> scopesEqual _  [] = False
> scopesEqual ((symTab1, vStore1) : scopes1) ((symTab2, vStore2) : scopes2)
>   | symTab1 === symTab2 && vStore1 === vStore2 = scopesEqual scopes1 scopes2
>   | otherwise                                  = False


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

> vs0 :: VarStore
> vs0 = [
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

> st0 :: SymTab
> st0 = [
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
           return;  // this is the exit point.
        } else {
            // modify a local variable (parameter actually)
            h = false;

            // another assignment on local variable
            v = 500 + 300;

            // declare a variable within the scope of winwin procedure
            int u;

            // assign the newly declared variable using a global variable.
            u = a;

            // modify the global array o. (modifies global state)
            o[2] = o[2] ^ 2;

            // modify the global variable c. (modifies global state)
            c = c + 1

            // call winwin()
            winwin(true, 555);
        }
    }

> pStore0 :: PrcdrStore
> pStore0 = [("winwin",  -- procedure name
>            ([  -- parameter types
>               ('h', BoolType),
>               ('v', IntType)
>             ],
>             [  -- procedure statements
>                If (Bin Eq (Var 'c') (ConstInt 3))  -- condition
>                   [Skip]  -- then
>                   [  -- else
>                      Asgn 'h' (ConstBool False),
>                      Asgn 'v' (Bin Plus (ConstInt 500) (ConstInt 300)),
>                      Declare 'u' IntType,
>                      Asgn 'u' (Var 'a'),
>                      AsgnArrRef 'o' (ConstInt 2) (Bin Power (ArrRef 'o' (ConstInt 2)) (ConstInt 2)),
>                      Asgn 'c' (Bin Plus (Var 'c') (ConstInt 1)),
>                      InvokePrcdr "winwin" [('h', Bool False), ('v', Int 666)]
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
    // for testing scope.
    u = b;

    // procedure invoked using parameters: (h: true, s: 555)
    winwin(true, 555);

> p0 :: Prog
> p0 = (
>   st0,  -- types of global variables
>   pStore0,  -- values of global variables
>   [  -- statements
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

The programme can be run by `run p0 vs0`.
If everything is right, after the execution, the varible store should be:

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


Now let there be tests:

One test to test them all!

> theTest :: Bool
> theTest = all (== True)
>             [test_applyUna, test_applyBin, test_eval,
>              test_exec_Skip, test_exec_Asgn, test_exec_AsgnArrRef,
>              test_exec_InvokePrcdr, test_exec_If, test_exec_Do, test_exec_Declare,
>              test_run,
>              test_dclrdButNotInitVars, test_intiButNotdclrdVars, test_typeMismatchedVars,
>              test_duplicateVars, test_undefinedPrcdrs, test_undeclaredVars,
>              test_reDeclarations, test_uninitVars, test_incorrectParamInvocs,
>              test_arrVars, test_badArrRefs, test_badAsgnArrRefs, test_badOps,
>              test_wrongTypeAsgnmts, test_misConExps]


The executing order of the programme is recursive top-down, e.g. `run` is at
programme level, `exec` is at block level, `exec'` is at statement level, `eval`
is at expression level. So I made test cases bottom up, in which order I
believe it's easier to detect and isolate the small errors.

Test cases for `applyUna`:
Test both paths of True and False. Don't know how to test the error thrown
path :(

> test_applyUna :: Bool
> test_applyUna = all (== True) [t1, t2]
>   where t1 = applyUna Not (Bool True) == Bool False
>         t2 = applyUna Not (Bool False) == Bool True

Test cases for `applyBin`:
Test all paths. Don't know how to test the error thrown path :(

> test_applyBin :: Bool
> test_applyBin = all (== True) [t01, t02, t03, t04, t05, t06, t07, t08, t09, t10, t11, t12, t13]
>   where t01 = applyBin Plus  (Int 5)  (Int 5)  == Int 10
>         t02 = applyBin Minus (Int 5)  (Int 10) == Int (-5)
>         t03 = applyBin Times (Int 6)  (Int 5)  == Int 30
>         t04 = applyBin Div   (Int 5)  (Int 3)  == Int 1
>         t05 = applyBin Power (Int 2)  (Int 6)  == Int 64
>         t06 = applyBin Eq    (Int 5)  (Int 6)  == Bool False
>         t07 = applyBin Ne    (Int 5)  (Int 6)  == Bool True
>         t08 = applyBin Lt    (Int 5)  (Int 5)  == Bool False
>         t09 = applyBin Le    (Int 5)  (Int 5)  == Bool True
>         t10 = applyBin Gt    (Int 5)  (Int 5)  == Bool False
>         t11 = applyBin Ge    (Int 5)  (Int 5)  == Bool True
>         t12 = applyBin And   (Bool True) (Bool False) == Bool False
>         t13 = applyBin Or    (Bool True) (Bool False) == Bool True

Test cases for `eval`:
Test all paths. Don't know how to test the error thrown path :(
(Somehow I feel that only testing on each path with primitive values is not
good enough. When we have recursive call, things could be more complicated.)

> test_eval :: Bool
> test_eval = all (== True) [t01, t02, t03, t04, t05, t06]
>   where t01 = eval (ConstInt 5) mockState == Int 5
>         t02 = eval (ConstBool True) mockState == Bool True
>         t03 = eval (Var 'a') mockState == Int 99
>         t04 = eval (ArrRef 'c' (ConstInt 3)) mockState == Int 3
>         t05 = eval (Bin Minus (Var 'a') (ConstInt 99)) mockState == Int 0
>         t06 = eval (Una Not (Var 'b')) mockState == Bool True

Test cases for `exec'`:
For each type of statement, there is a dedicated test.

> test_exec_Skip :: Bool
> test_exec_Skip = exec' Skip mockState == mockState

> test_exec_Asgn :: Bool
> test_exec_Asgn = t01 && t02
>   where t01 = getVarValue 'a' (exec' (Asgn 'a' (ConstInt 3)) mockState) == Int 3
>         t02 = getVarValue 'b' (exec' (Asgn 'b' (ConstBool True)) mockState) == Bool True

> test_exec_AsgnArrRef :: Bool
> test_exec_AsgnArrRef
>   = getVarValue 'c' (exec' (AsgnArrRef 'c' (ConstInt 3) (ConstInt 30)) mockState) == newArr
>     where newArr = Arr (array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 30)]) 4 IntType

> test_exec_InvokePrcdr :: Bool
> test_exec_InvokePrcdr = check1 && check2
>   where postState = exec' (InvokePrcdr "test" [('x', Int 5)]) mockState
>         check1 = getVarValue 'a' postState == Int 100
>         check2 = not $ isVarDeclared 'x' postState  -- to test local variable cannot come out to global scope

> test_exec_If :: Bool
> test_exec_If = check1 && check2
>   where postState = exec' (If (ConstBool True) [Asgn 'a' (ConstInt 0)] [Asgn 'b' (ConstBool True)]) mockState
>         check1 = getVarValue 'a' postState == Int 0
>         check2 = getVarValue 'b' postState == Bool False

> test_exec_Do :: Bool
> test_exec_Do = check1 && check2 && check3
>   where doStmt    = Do (Bin Lt (Var 'a') (ConstInt 105)) [Asgn 'a' (Bin Plus (Var 'a') (ConstInt 1))]
>         postState = exec' doStmt mockState
>         check1    = getVarValue 'a' postState == Int 105
>         check2    = getVarValue 'b' postState == Bool False
>         check3    = getVarValue 'c' postState == Arr (array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]) 4 IntType

> test_exec_Declare :: Bool
> test_exec_Declare = check1 && check2
>   where declareStmt = Declare 'x' IntType
>         postState   = exec' declareStmt mockState
>         check1      = isVarDeclared 'x' postState
>         check2      = not $ isVarInitialised 'x' postState

Test cases for `exec` and `run`:
For `exec`, honestly I don't think this function needs to be tested. It's just
iterated applications on a list of statements.
For `run` function, ideally I would test the massive error checking guards. But
I don't know how to assert on an expected error thrown in Haskel, so I did many
manual tests on them, and then test whether the example programme above can get
a correct result. Test cases for each check guard are provided later.

> test_run :: Bool
> test_run = s1 === s2
>   where s1 = run p0 vs0
>         s2 = [
>                ('a', Int 5),
>                ('b', Int 1),
>                ('c', Int 3),
>                ('h', Bool True),
>                ('i', Bool True),
>                ('o', Arr (array (0, 3) [(0, Int 20), (1, Int 1), (2, Int 256), (3, Int 3)]) 4 IntType),
>                ('p', Arr (array (0, 2) [(0, Arr (array (0, 0) [(0, Bool True)]) 1 BoolType), (1, Arr (array (0, 1) [(0, Bool False), (1, Bool True)]) 2 BoolType), (2, Arr (array (0, 2) [(0, Bool True), (1, Bool False), (2, Bool True)]) 3 BoolType)]) 3 (ArrayType BoolType)),
>                ('u', Int 1)
>              ]

mockState:
This is a simple mock state for testing. It has an integer, a boolean, and an
array of integers.

> mockState :: State
> mockState = ([(st, vs)], ps)
>   where st = [
>                ('a', IntType),
>                ('b', BoolType),
>                ('c', ArrayType IntType)
>              ]
>         vs = [
>                ('a', Int 99),
>                ('b', Bool False),
>                ('c', Arr (array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]) 4 IntType)
>              ]
>         ps = [("test",  -- procedure name
>               ([  -- parameter types
>                  ('x', IntType)
>                ],
>                [  -- procedure statements
>                   Asgn 'x' (Bin Plus (Var 'x') (ConstInt 1)),  -- local change
>                   Asgn 'a' (Bin Plus (Var 'a') (ConstInt 1))   -- global change
>                ])
>               )]


Test cases for static checking functions:

There are massive functions doing static checking (I wonder if what I did was
really a stupid approach Orz...). Each check has some (sort of) sub-functions
to check through different levels, e.g. to check undeclared variables, I have
`undeclaredVars` for programme level, then `undeclaredVarsBlock` for block
level, then `undeclaredVarsStmt` on statement level, then `undeclaredVarsExp` on
expression level. For each check, I only provide test cases for the top
(programme) level function, to make sure it works as expected. There is just no
need to test sub-functions. As Martin Fowler pointed out in Test Pyramid: Test
public API, not implementation details.

Also, these tests can't guarantee a good coverage, as there are too many edge
cases. I had neither enough time nor confidence to cover all of them. So most
test cases here are for happy path only.

Test cases for `dclrdButNotInitVars`:

> test_dclrdButNotInitVars :: Bool
> test_dclrdButNotInitVars = matchArray result "bc"
>   where result = dclrdButNotInitVars prog vStore
>         vStore = [
>                    ('a', Int 99)
>                  ]
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),  -- declared but not initialised
>                    ('c', ArrayType IntType)  -- declared but not initialised
>                  ]
>         pStore = emptyMap
>         block  = [Skip]
>         prog   = (symTab, pStore, block)

Test cases for `intiButNotdclrdVars`:

> test_intiButNotdclrdVars :: Bool
> test_intiButNotdclrdVars = matchArray result "ab"
>   where result = intiButNotdclrdVars prog vStore
>         vStore = [
>                    ('a', Int 99),  -- initialised but not declared
>                    ('b', Bool False),  -- initialised but not declared
>                    ('c', Arr (array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]) 4 IntType)
>                  ]
>         symTab = [
>                    ('c', ArrayType IntType)
>                  ]
>         pStore = emptyMap
>         block  = [Skip]
>         prog   = (symTab, pStore, block)

Test cases for `typeMismatchedVars`:

> test_typeMismatchedVars :: Bool
> test_typeMismatchedVars = matchArray result "ac"
>   where result = typeMismatchedVars prog vStore
>         vStore = [
>                    ('a', Int 99),
>                    ('b', Bool False),
>                    ('c', Arr (array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]) 4 IntType)
>                  ]
>         symTab = [
>                    ('a', BoolType),  -- type mismatch
>                    ('b', BoolType),
>                    ('c', BoolType)   -- type mismatch
>                  ]
>         pStore = emptyMap
>         block  = [Skip]
>         prog   = (symTab, pStore, block)

Test cases for `duplicateVars`:

> test_duplicateVars :: Bool
> test_duplicateVars = matchArray result "ac"
>   where result = duplicateVars prog vStore
>         vStore = [
>                    ('a', Int 99),
>                    ('a', Int 88),  -- duplicate
>                    ('b', Bool False),
>                    ('c', Arr (array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]) 4 IntType)
>                  ]
>         symTab = [
>                    ('a', BoolType),
>                    ('b', BoolType),
>                    ('c', BoolType),
>                    ('c', IntType)  -- duplicate
>                  ]
>         pStore = emptyMap
>         block  = [Skip]
>         prog   = (symTab, pStore, block)

Test cases for `undefinedPrcdrs`:

> test_undefinedPrcdrs :: Bool
> test_undefinedPrcdrs = matchArray result ["chuck", "taylor"]
>   where result = undefinedPrcdrs prog
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      InvokePrcdr "chuck" emptyMap,  -- where are you chuck?
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [InvokePrcdr "taylor" emptyMap]  -- where are you taylor?
>         prog   = (emptyMap, pStore, block)

Test cases for `undeclaredVars`:

> test_undeclaredVars :: Bool
> test_undeclaredVars = matchArray result "xyzuv"
>   where result = undeclaredVars prog
>         symTab = [
>                    ('a', BoolType),
>                    ('b', BoolType),
>                    ('c', BoolType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      Asgn 'u' (ConstInt 0),  -- illegal
>                      Declare 'f' IntType,
>                      Asgn 'f' (Var 'v'),  -- legal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    Asgn 'x' (Var 'y'),  -- illegal
>                    AsgnArrRef 'z' (Var 'y') (ConstInt 0),  -- illegal
>                    Declare 'd' IntType,
>                    Asgn 'd' (ConstInt 0),  -- legal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `reDeclarations`:

> test_reDeclarations :: Bool
> test_reDeclarations = matchArray result [Declare 'a' IntType, Declare 'd' IntType, Declare 'x' IntType, Declare 'e' IntType]
>   where result = reDeclarations prog
>         symTab = [
>                    ('a', BoolType),
>                    ('b', BoolType),
>                    ('c', BoolType)
>                  ]
>         pStore = [("chucktaylor",
>                   ([('x', BoolType)],
>                    [
>                      Declare 'x' IntType,  -- illegal
>                      Declare 'e' IntType,  -- legal
>                      Declare 'e' IntType,  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    Declare 'a' IntType,  -- illegal
>                    Declare 'd' IntType,  -- legal
>                    Declare 'd' IntType,  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `uninitVars`:

> test_uninitVars :: Bool
> test_uninitVars = matchArray result "uvde"
>   where result = uninitVars prog vStore
>         vStore = [
>                    ('a', Int 99),
>                    ('b', Bool False),
>                    ('c', Arr (array (0,3) [(0,Int 0),(1,Int 1),(2,Int 2),(3,Int 3)]) 4 IntType)
>                  ]
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType),
>                    ('d', IntType),
>                    ('e', IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      Declare 'v' IntType,
>                      Asgn 'a' (Var 'v'),  -- illegal
>                      Asgn 'a' (Var 'v'),  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    Declare 'u' IntType,
>                    Asgn 'a' (Var 'u'),  -- illegal
>                    AsgnArrRef 'c' (ConstInt 0) (Var 'd') ,  -- illegal
>                    If (ConstBool True) [Asgn 'a' (Var 'e')] [Asgn 'b' (ConstBool True)],  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `incorrectParamInvocs`:

> test_incorrectParamInvocs :: Bool
> test_incorrectParamInvocs = matchArray result [InvokePrcdr "chucktaylor" emptyMap]
>   where result = incorrectParamInvocs prog
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType),
>                    ('d', IntType),
>                    ('e', IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   ([('x', IntType), ('y', BoolType)],
>                    [InvokePrcdr "chucktaylor" [('x', Int 3), ('y', Int 3)]]  -- illegal
>                   ))]
>         block  = [
>                    InvokePrcdr "chucktaylor" emptyMap,  -- illegal
>                    InvokePrcdr "chucktaylor" [('x', Bool True), ('y', Bool True)]  -- illegal
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `arrVars`:

> test_arrVars :: Bool
> test_arrVars = matchArray result "cd"
>   where result = arrVars prog
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      Asgn 'a' (Var 'c'),  -- illegal
>                      Declare 'd' (ArrayType IntType),
>                      Asgn 'a' (Var 'd'),  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    Asgn 'a' (Var 'c'),  -- illegal
>                    AsgnArrRef 'c' (ConstInt 0) (Var 'c') ,  -- illegal
>                    If (ConstBool True) [Asgn 'a' (Var 'c')] [Asgn 'b' (ConstBool True)],  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `badArrRefs`:

> test_badArrRefs :: Bool
> test_badArrRefs = matchArray result [ArrRef 'a' (ConstInt 0), ArrRef 'c' (ConstBool True)]
>   where result = badArrRefs prog
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      Asgn 'a' (ArrRef 'c' (ConstBool True)),  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    Asgn 'a' (ArrRef 'a' (ConstInt 0)),  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `badAsgnArrRefs`:

> test_badAsgnArrRefs :: Bool
> test_badAsgnArrRefs = matchArray result [AsgnArrRef 'a' (ConstInt 0) (Var 'a'), AsgnArrRef 'c' (ConstInt 0) (Var 'b'), AsgnArrRef 'c' (ConstBool True) (Var 'b')]
>   where result = badAsgnArrRefs prog
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      AsgnArrRef 'c' (ConstBool True) (Var 'b'),  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    AsgnArrRef 'a' (ConstInt 0) (Var 'a'),  -- illegal
>                    AsgnArrRef 'c' (ConstInt 0) (Var 'b'),  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `badOps`:

> test_badOps :: Bool
> test_badOps = matchArray result [Una Not (ConstInt 1), Bin Le (ConstBool True) (ConstInt 1), Bin Plus (ConstBool True) (ConstInt 1)]
>   where result = badOps prog
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      Asgn 'b' (Una Not (ConstInt 1)),  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    Asgn 'a' (Bin Plus (ConstBool True) (ConstInt 1)),  -- illegal
>                    If (Bin Le (ConstBool True) (ConstInt 1)) [Asgn 'a' (Var 'e')] [Asgn 'b' (ConstBool True)],  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `wrongTypeAsgnmts`:

> test_wrongTypeAsgnmts :: Bool
> test_wrongTypeAsgnmts = matchArray result [Asgn 'a' (Var 'b'), Asgn 'a' (ConstBool True), Asgn 'b' (Var 'a'), Asgn 'c' (Bin Plus (ConstInt 1) (ConstInt 1))]
>   where result = wrongTypeAsgnmts prog
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      Asgn 'b' (Var 'a'),  -- illegal
>                      Asgn 'c' (Bin Plus (ConstInt 1) (ConstInt 1)),  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    Asgn 'a' (Var 'b'),  -- illegal
>                    Asgn 'a' (ConstBool True),  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

Test cases for `misConExps`:

> test_misConExps :: Bool
> test_misConExps = matchArray result [ConstInt 7, Var 'a', Bin Plus (ConstInt 7) (ConstInt 7)]
>   where result = misConExps prog
>         symTab = [
>                    ('a', IntType),
>                    ('b', BoolType),
>                    ('c', ArrayType IntType)
>                  ]
>         pStore = [("chucktaylor",
>                   (emptyMap,
>                    [
>                      If (Bin Plus (ConstInt 7) (ConstInt 7)) [Skip] [Skip],  -- illegal
>                      InvokePrcdr "chucktaylor" emptyMap
>                    ]))]
>         block  = [
>                    If (ConstInt 7) [Skip] [Skip],  -- illegal
>                    Do (Var 'a') [Skip],  -- illegal
>                    InvokePrcdr "chucktaylor" emptyMap
>                  ]
>         prog   = (symTab, pStore, block)

matchArray:
helper to check whether two arrays have same elements

> matchArray :: Ord a => [a] -> [a] -> Bool
> matchArray a b = sort a == sort b

I did not have time to write test cases for State functions. Ideally I'd
certainly test it because State is a critical structure for implementing scopes.
Truth is that I did find two bugs in State functions. It's just too much
workload for such a short time
¯\_(ツ)_/¯

TL'DR
