Name:              Fang Zhao (300364061)
Course Number:     COMP304
Assignment Number: 4
Question Number:   6 & 7

> module Family where

> import Control.Monad.State
> import Data.List (nub)


==============================================
             Functions for Person
==============================================


Gender is gender, nothing obscure here.

> data Gender = Male | Female deriving (Show, Eq)

A Name is just a String.

> type Name = String

A person can have multiple Attributes:
  - Name. A person has to have a name in our database. When creating a new
    record using `person` function, name has to be provided. Name also serves
    as the unique identifier of each person.
  - Gender. This can be Male or Female. This is not politically correct but
    sorry no other options available yet.
  - Spouses. A list of Name.
  - Children. A list of Name.
  - isSovereign. Default value: False. Can be set True by function `sovereign`.

> data Attribute = Name Name
>                | Gender Gender
>                | Spouses [Name]
>                | Children [Name]
>                | Sovereign Bool
>                deriving (Show, Eq)

A Person is just a list of Attribute.

> newtype Person = Person [Attribute] deriving (Show, Eq)

newGuy:
Create a new guy with the name as given and Sovereign attributes as False.

> newGuy :: String -> Person
> newGuy name = Person [Name name, Sovereign False]

update:
Update a person record with the given attribute. If this person already has this
attribute set, then replace it, otherwise add the given attribute as a new one.

> update :: Attribute -> Person -> Person
> update (Name _) _ = error "Updating name is not allowed!"
> update attr (Person attrs) = Person (updateOrAdd attr attrs)
>   where updateOrAdd x [] = [x]
>         updateOrAdd x (a : as)
>           | isSameType x a = x : as
>           | otherwise      = a : updateOrAdd x as
>         isSameType (Name _)      (Name _)      = True
>         isSameType (Gender _)    (Gender _)    = True
>         isSameType (Spouses _)   (Spouses _)   = True
>         isSameType (Children _)  (Children _)  = True
>         isSameType (Sovereign _) (Sovereign _) = True
>         isSameType _             _             = False

hasAttribute:
Does this guy has this atribute?

> hasAttribute :: Person -> Attribute -> Bool
> hasAttribute (Person []) _ = False
> hasAttribute (Person (a : as)) attr
>   | attr == a = True
>   | otherwise = hasAttribute (Person as) attr

getChildren:
Get all the children of his guy.

> getChildren :: Person -> [Name]
> getChildren (Person []) = []
> getChildren (Person (Children cs : _)) = cs
> getChildren (Person (_ : as)) = getChildren (Person as)

getSpouses:
Get all the spouses of his guy.

> getSpouses :: Person -> [Name]
> getSpouses (Person []) = []
> getSpouses (Person (Spouses ss : _)) = ss
> getSpouses (Person (_ : as)) = getSpouses (Person as)

getName:
Get the name of from a Person object.

> getName :: Person -> Name
> getName (Person []) = error "What did you do boi! How dare you create a Person without a name!"
> getName (Person (Name name : _)) = name
> getName (Person (_ : as)) = getName (Person as)


==============================================
             Test cases for Person
==============================================


test_newGuy:
There isn't much to test for a constructor-like function.

> test_newGuy :: Bool
> test_newGuy = all (== True) [t1]
>   where t1 = newGuy "Hector" == Person [Name "Hector", Sovereign False]

test_update:
Covers: adding new attribute, replacing existing attributes, replacing
list-valued existing attribute.

> test_update :: Bool
> test_update = all (== True) [t1, t2, t3, t4]
>   where t1 = update (Gender Female) mockGuy == Person [Name "Hector", Gender Female, Spouses ["Roxy"], Sovereign False]
>         t2 = update (Children ["Gitano"]) mockGuy == Person [Name "Hector", Gender Male, Spouses ["Roxy"], Sovereign False, Children ["Gitano"]]
>         t3 = update (Spouses ["Maria", "Maria"]) mockGuy == Person [Name "Hector", Gender Male, Spouses ["Maria", "Maria"], Sovereign False]
>         t4 = update (Spouses ["Roxy"]) mockGuy == mockGuy

test_hasAttribute:
Cover both positive and negative paths.

> test_hasAttribute :: Bool
> test_hasAttribute = all (== True) [t1, t2, t3, t4]
>   where t1 = mockGuy `hasAttribute` Name "Hector"
>         t2 = mockGuy `hasAttribute` Gender Male
>         t3 = not $ mockGuy `hasAttribute` Name "Roxy"
>         t4 = not $ mockGuy `hasAttribute` Spouses ["Maria", "Roxy"]

test_getChildren:
Straightforward test cases.

> test_getChildren :: Bool
> test_getChildren = all (== True) [t1, t2, t3]
>   where t1 = getChildren mockGuy == []
>         t2 = getChildren mockGuy' == ["Gitano"]
>         t3 = getChildren mockGuy'' == ["Gitano", "Dante"]
>         mockGuy' = update (Children ["Gitano"]) mockGuy
>         mockGuy'' = update (Children ["Gitano", "Dante"]) mockGuy

test_getSpouses:
Straightforward test cases.

> test_getSpouses :: Bool
> test_getSpouses = all (== True) [t1, t2, t3]
>   where t1 = getSpouses mockGuy == ["Roxy"]
>         t2 = getSpouses mockGuy' == []
>         t3 = getSpouses mockGuy'' == ["Maria", "Maria"]
>         mockGuy' = Person [Name "Hector", Sovereign False]
>         mockGuy'' = update (Spouses ["Maria", "Maria"]) mockGuy

mockGuy:
A mock subject for easy testing.

> mockGuy :: Person
> mockGuy = Person [Name "Hector", Gender Male, Spouses ["Roxy"], Sovereign False]

theTest_person:
One test to test them all!

> theTest_person :: Bool
> theTest_person = all (== True) [
>                    test_newGuy, test_update, test_hasAttribute,
>                    test_getChildren, test_getSpouses
>                  ]


==============================================
           The database for Q6 & Q7
==============================================


> facts :: State Monarchs ()
> facts = do
>   person "Elizabeth I"
>   person "Henry VIII"
>   person "Anne Boleyn"
>   person "Catherine of Aragon"
>   person "Jane Seymour"
>   person "Anne of Cleves"
>   person "Catherine Howard"
>   person "Catherine Parr"
>   person "Henry VII"
>   person "Mary I"
>   person "Philip"
>   person "Edward VI"
>   person "Elizabeth of York"
>   person "Thomas Boleyn"
>   person "Elizabeth Howard"
>   person "George Boleyn"
>   person "James I"
>   person "Charles I"
>   person "Charles II"
>   person "James II"
>   person "Mary II"
>   person "William III"
>   person "Anne"
>   person "Mary Henrietta"
>   person "Anne Hyde"
>   "Elizabeth I"   `isChildOf` "Henry VIII"
>   "Elizabeth I"   `isChildOf` "Anne Boleyn"
>   "Mary I"        `isChildOf` "Henry VIII"
>   "Mary I"        `isChildOf` "Catherine of Aragon"
>   "Edward VI"     `isChildOf` "Jane Seymour"
>   "Edward VI"     `isChildOf` "Henry VIII"
>   "Henry VIII"    `isChildOf` "Henry VII"
>   "Henry VIII"    `isChildOf` "Elizabeth of York"
>   "Anne Boleyn"   `isChildOf` "Thomas Boleyn"
>   "Anne Boleyn"   `isChildOf` "Elizabeth Howard"
>   "George Boleyn" `isChildOf` "Thomas Boleyn"
>   "George Boleyn" `isChildOf` "Elizabeth Howard"
>   "Charles I"     `isChildOf` "James I"
>   "Charles II"    `isChildOf` "Charles I"
>   "Mary Henrietta"`isChildOf` "Charles I"
>   "James II"      `isChildOf` "Charles I"
>   "Mary II"       `isChildOf` "James II"
>   "Mary II"       `isChildOf` "Anne Hyde"
>   "William III"   `isChildOf` "Mary Henrietta"
>   "Anne"          `isChildOf` "James II"
>   "Anne"          `isChildOf` "Anne Hyde"
>   "Henry VIII"    `married` "Catherine of Aragon"
>   "Henry VIII"    `married` "Anne Boleyn"
>   "Henry VIII"    `married` "Jane Seymour"
>   "Henry VIII"    `married` "Anne of Cleves"
>   "Henry VIII"    `married` "Catherine Howard"
>   "Henry VIII"    `married` "Catherine Parr"
>   "Henry VII"     `married` "Elizabeth of York"
>   "Thomas Boleyn" `married` "Elizabeth Howard"
>   "Mary I"        `married` "Philip"
>   "James II"      `married` "Anne Hyde"
>   "Mary I"        `married` "William III"
>   male "Henry VIII"
>   male "Henry VII"
>   male "Philip"
>   male "Edward VI"
>   male "Thomas Boleyn"
>   male "George Boleyn"
>   male "James I"
>   male "Charles I"
>   male "Charles II"
>   male "James II"
>   male "William III"
>   female "Anne Boleyn"
>   female "Catherine of Aragon"
>   female "Jane Seymour"
>   female "Anne of Cleves"
>   female "Catherine Howard"
>   female "Catherine Parr"
>   female "Mary I"
>   female "Elizabeth I"
>   female "Elizabeth of York"
>   female "Elizabeth Howard"
>   female "Mary II"
>   female "Anne"
>   female "Mary Henrietta"
>   female "Anne Hyde"
>   sovereign "Elizabeth I"
>   sovereign "Henry VIII"
>   sovereign "Henry VII"
>   sovereign "Mary I"
>   sovereign "Philip"
>   sovereign "Edward VI"
>   sovereign "James I"
>   sovereign "Charles I"
>   sovereign "Charles II"
>   sovereign "James II"
>   sovereign "Mary II"
>   sovereign "William III"
>   sovereign "Anne"


==============================================
              Question 6 Code
==============================================


Let's record some monarchs!

A Monarchs is just a pile of people.

> type Monarchs = [Person]

person:
Put a new guy into our monarchs. This is the only function to add a new record
into our database. If the provided name is already recorded, then do nothing.

> person :: String -> State Monarchs ()
> person name = do
>   people <- get
>   when (all (not . (`hasAttribute` Name name)) people) $ do
>     put (newGuy name : people)
>     return ()

isChildOf:
Set a Parent - Child relationship. This is a single directional relationship,
as the father and mother knows the child but the child doesn't know his/her
parents. (tears...)

> isChildOf :: Name -> Name -> State Monarchs ()
> isChildOf child parent = do
>   target <- getPerson parent
>   _      <- getPerson child    -- to detect whether we have this child in database
>   let cHildren = getChildren target
>   setAttribute parent (Children (child : cHildren))
>   return ()

married:
Set a marriage relationship between two guys. This is a bi-directional
relationship.

> married :: Name -> Name -> State Monarchs ()
> married guy1 guy2 = do
>   target1 <- getPerson guy1
>   target2 <- getPerson guy2
>   let spouses1 = getSpouses target1
>   let spouses2 = getSpouses target2
>   setAttribute guy2 (Spouses (guy1 : spouses2))
>   setAttribute guy1 (Spouses (guy2 : spouses1))
>   return ()

male:
Set this guy as Male.

> male :: String -> State Monarchs ()
> male name = setAttribute name (Gender Male)

female:
Set this guy as Female. Such power!

> female :: String -> State Monarchs ()
> female name = setAttribute name (Gender Female)

sovereign:
Set this guy as sovereign.

> sovereign :: String -> State Monarchs ()
> sovereign name = setAttribute name (Sovereign True)

getPerson:
Find and return the person with matched name. If there is no record found, then
throw an error. As most functions in this question will run past `getPerson`
first, any incorrect name will cause an error from here.

> getPerson :: String -> State Monarchs Person
> getPerson name = state $ \people -> (nameToPerson name people, people)

nameToPerson:
Given a name, find out the Person object from database. If cannot find one, then
an error will be thrown.

I made the design decision to let it explode when the provided name can not be
found, because I think that in most cases an operation on a non-existing record
should be illegal and reported back to user. My first implementation of this
function returns `Maybe Person`, and it works in a silent way (returns Nothing)
if the provided name cannot be found. Although it makes some sense to use Maybe
and throw no errors, plus one of Michael's replies on forum says "You aren't
required to throw any errors", I still don't think it's a good design on any
database. So I'll hold on my argument here.

> nameToPerson :: Name -> Monarchs -> Person
> nameToPerson name [] = error (name ++ " is not yet recorded in the system!")
> nameToPerson name (p : ps)
>   | p `hasAttribute` Name name = p
>   | otherwise                  = nameToPerson name ps

delPerson:
Delete and return the person with matched name (as a Just). If there is no
record found, an error will be thrown from `getPerson` function.

> delPerson :: String -> State Monarchs Person
> delPerson name = do
>   target <- getPerson name
>   people <- get
>   put $ filter (not . (`hasAttribute` Name name)) people
>   return target

setAttribute:
Set the given attribute to anyone who has the given name. If this name doesn't
belong to anybody in our database, then do nothing.

> setAttribute :: Name -> Attribute -> State Monarchs ()
> setAttribute name attr = do
>   target <- delPerson name
>   let updatedTarget = update attr target
>   people <- get
>   put (updatedTarget : people)
>   return ()


==============================================
            Question 6 Discussion
==============================================


1. Discuss the monad you chose, and why.

- The monad I chose for this question is State Monad. It makes perfect sense to
use State since we are trying maintain a updatable state. Also, using the State
Monad to maintain the database as a monadic context provides us a devlarative
way to define DSL within do notation.

I did put some thought on other types of Monads that we have learned. Maybe and
Either are usually for handling one alternative or optional values, so they
don't sound very suitable. List is for nondeterministic or branching-out values,
again, not very suitable for a family tree DSL. IO is not involved so IO Monad
is not an option. Writer could be it, but I guess in order to use Writer we have
to write many extra functions to manipulate the logs first. State is just an
intuitive option for this task.

2. Is there a better way this language could be designed? What is it? Why?

- A possible improvement could be using Set instead of List for attributes of a
Person, like `newtype Person = Person (Set Attribute)`; and for attributes that
hold multiple values, like `Attribute = Spouses (Set Person) | ...`. By using
Set it'd be much easier for managing duplicates and testing as well. And if the
database goes up in size, the List should definitely be replaced with Set, as
the speed of iterating on List is far slower. As for this question, List is just
easier to do pattern-matching to find a specific type of attribute.

A more adopted way of designing a database is to use self-incrementing integer
as the unique identifier (id), and the foreign key for other records to refer.
This system uses name (String) for this purpose, which is not that ideal.

The `nameToPerson` function will throw errors, which is a bit yuck (see
discussion above `nameToPerson` function). I'm sure there is a better design so
that we don't silently allow errors and prompt the user that a mistake has been
made. But if we let the error slip into our database silently and don't report
it, it'd be a fundanmental flaw in any database design.


==============================================
            Question 6 Test Cases
==============================================


test_person:
Cover cases of adding a new person, and adding a name that is already recorded.

> test_person :: Bool
> test_person = all (== True) [t1, t2, t3]
>   where t1 = runState (person "Hector") [] == ((), [Person [Name "Hector", Sovereign False]])
>         t2 = runState (do person "Hector"; person "Roxy") [] == ((), [Person [Name "Roxy", Sovereign False], Person [Name "Hector", Sovereign False]])
>         t3 = runState (do person "Hector"; person "Roxy"; person "Hector") [] == ((), [Person [Name "Roxy", Sovereign False], Person [Name "Hector", Sovereign False]])

test_isChildOf:
Covers cases of adding a child to a parent that has no child before, and
adding a child to a parent that already has some child(ren).

> test_isChildOf :: Bool
> test_isChildOf = all (== True) [t1, t2]
>   where t1 = runState ("Hector" `isChildOf` "Anna") [Person [Name "Hector", Sovereign False], Person [Name "Anna", Sovereign False]]
>           == ((), [Person [Name "Anna", Sovereign False, Children ["Hector"]], Person [Name "Hector", Sovereign False]])
>         t2 = runState ("Hector" `isChildOf` "Anna") [Person [Name "Hector", Sovereign False], Person [Name "Anna", Sovereign False, Children ["Tiger"]]]
>           == ((), [Person [Name "Anna", Sovereign False, Children ["Hector", "Tiger"]], Person [Name "Hector", Sovereign False]])

test_married:
Covers cases of adding a spouse to a single guy that has no spouse before, and
adding a spouse to a guy that already has some spouse(s).

> test_married :: Bool
> test_married = all (== True) [t1, t2]
>   where t1 = runState ("Hector" `married` "Roxy") [Person [Name "Hector", Sovereign False], Person [Name "Roxy", Sovereign False]]
>           == ((), [Person [Name "Hector", Sovereign False, Spouses ["Roxy"]], Person [Name "Roxy", Sovereign False, Spouses ["Hector"]]])
>         t2 = runState ("Hector" `married` "Maria") [Person [Name "Hector", Sovereign False, Spouses ["Roxy"]], Person [Name "Roxy", Sovereign False, Spouses ["Hector"]], Person [Name "Maria", Sovereign False]]
>           == ((), [Person [Name "Hector", Sovereign False, Spouses ["Maria", "Roxy"]], Person [Name "Maria", Sovereign False, Spouses ["Hector"]], Person [Name "Roxy", Sovereign False, Spouses ["Hector"]]])

test_male:
Covers cases of adding and replacing the Gender attribute.

> test_male :: Bool
> test_male = all (== True) [t1, t2]
>   where t1 = runState (male "Hector") [Person [Name "Hector", Sovereign False]] == ((), [Person [Name "Hector", Sovereign False, Gender Male]])
>         t2 = runState (male "Hector") [Person [Name "Hector", Sovereign False, Gender Female]] == ((), [Person [Name "Hector", Sovereign False, Gender Male]])

test_female:
Covers cases of adding and replacing the Gender attribute.

> test_female :: Bool
> test_female = all (== True) [t1, t2]
>   where t1 = runState (female "Roxy") [Person [Name "Roxy", Sovereign False]] == ((), [Person [Name "Roxy", Sovereign False, Gender Female]])
>         t2 = runState (female "Roxy") [Person [Name "Roxy", Sovereign False, Gender Male]] == ((), [Person [Name "Roxy", Sovereign False, Gender Female]])

test_sovereign:
Check to confirm Sovereign attribute is set as true.
The `male`, `female`, and `sovereign` function are essentially a same thing with
my implementation. And the test cases for `setAttribute` can increase the
confidence level of these three functions.

> test_sovereign :: Bool
> test_sovereign = all (== True) [t1]
>   where t1 = runState (sovereign "Hector") [Person [Name "Hector", Sovereign False]] == ((), [Person [Name "Hector", Sovereign True]])

test_getPerson:
Test the function can find the right person back.

> test_getPerson :: Bool
> test_getPerson = all (== True) [t1, t2]
>   where t1 = runState (getPerson "Hector") mockState == (hector, mockState)
>         t2 = runState (getPerson "Roxy") mockState == (roxy, mockState)
>         mockState = [hector, roxy]
>         hector    = Person [Name "Hector", Sovereign False]
>         roxy      = Person [Name "Roxy", Sovereign False]

test_delPerson:
Test the function can delete the right person, and return it back.

> test_delPerson :: Bool
> test_delPerson = all (== True) [t1, t2]
>   where t1 = runState (delPerson "Hector") mockState == (hector, [roxy])
>         t2 = runState (delPerson "Roxy") mockState == (roxy, [hector])
>         mockState = [hector, roxy]
>         hector    = Person [Name "Hector", Sovereign False]
>         roxy      = Person [Name "Roxy", Sovereign False]

test_setAttribute:
Cover cases of adding and replacing existing attribute.

> test_setAttribute :: Bool
> test_setAttribute = all (== True) [t1, t2]
>   where t1 = runState (setAttribute "Hector" (Gender Male)) mockState
>           == ((), [Person [Name "Hector", Sovereign False, Gender Male], roxy])
>         t2 = runState (do setAttribute "Roxy" (Gender Male); setAttribute "Roxy" (Sovereign True); setAttribute "Roxy" (Spouses ["Hector"])) mockState
>           == ((), [Person [Name "Roxy", Sovereign True, Gender Male, Spouses ["Hector"]], hector])
>         mockState = [hector, roxy]
>         hector    = Person [Name "Hector", Sovereign False]
>         roxy      = Person [Name "Roxy", Sovereign False]

theTest_q6:
One test to test them all!

> theTest_q6 :: Bool
> theTest_q6 = all (== True) [
>                test_person, test_married, test_isChildOf, test_male,
>                test_female, test_sovereign, test_getPerson,
>                test_delPerson, test_setAttribute,
>                theTest_person  -- better check that person type works fine as well
>              ]


==============================================
              Question 7 Code
==============================================


N.B. All functions in question 7 has the assumption that `facts` can be used as
a accessible database / global constant within this module.

parents:
Find out all parents of this guy.

> parents :: Name -> [Name]
> parents name = map getName $ filter (`hasChild` name) allPeople
>   where p `hasChild` child = child `elem` getChildren p

grandparents:
Find out all grandparents of this guy.

> grandparents :: Name -> [Name]
> grandparents name = concatMap parents $ parents name

siblings:
Find out all the (half or full) sibings of this guy

> siblings :: Name -> [Name]
> siblings name = nub siblings'
>   where prnts         = parents name
>         prntsAsPeople = map (`nameToPerson` allPeople) prnts
>         siblings'     = filter (/= name) $ concatMap getChildren prntsAsPeople

children:
Find all the children of this guy.

> children :: Name -> [Name]
> children name = getChildren $ nameToPerson name allPeople

grandchildren:
Find all the grandchildren of this guy.

> grandchildren :: Name -> [Name]
> grandchildren name = concatMap children $ children name

spouse:
Find all the spouse(s) of this guy.

> spouse :: Name -> [Name]
> spouse name = getSpouses $ nameToPerson name allPeople

allPeople:
Run the Monarchs State, get all people in the database.

> allPeople :: Monarchs
> allPeople = people
>   where (_, people) = runState facts []


==============================================
            Question 7 Discussion
==============================================


1. Which monad did you choose here, and why?

- Honestly I don't think there is any need to choose a Monad for this question,
given that all functions can automaticall query the database we defined in
question 6.

The only Monad I used here is the same Monad from question 6, `State Mornachs`,
which is only excuted by `runState` to extract the state out of Monad.

2. How, if at all, does it relate to what you chose in Q6?

- I think question 6 and 7 belongs to one system, as functions in Q6 are DDL
(Data Definition Language), and functions in Q7 are DQL (Data Query Language).
Intuitively we want to use same Monad for Q6 and Q7, and it works out.


==============================================
            Question 7 Test Cases
==============================================


N.B. Because functions in Q7 are using `facts` as a global constant, so these
functions are closely coupled with the content of `facts`. We can only test
against `facts`, so these tests aren't very useful.


showAll:
A helper function to list the result of wanted function for all people in
database.

E.g.
showAll parents
=> [("Anne", ["James II", "Anne Hyde"]), ("William III", ["Mary Henrietta"]), ...]
showAll siblings
=> [("Anne", ["Mary II"]), ("William III", []), ("Mary II", ["Anne"]), ...]

It's not used for testing, but it's useful to observe all the results and verify
against an actual table or family tree of these mornachs.

> showAll :: (Name -> t) -> [(Name, t)]
> showAll func = map ((\name -> (name, ($) func name)) . getName) allPeople


test_parents:
Test on cases that has both parents, single parent, or no parent recorded.

> test_parents :: Bool
> test_parents = all (== True) [t1, t2, t3]
>   where t1 = parents "Elizabeth I" == ["Henry VIII", "Anne Boleyn"]
>         t2 = parents "William III" == ["Mary Henrietta"]
>         t3 = parents "Anne Hyde" == []

test_grandparents:
Test on peroson whose has both parents, single parent, or no parent recorded.

> test_grandparents :: Bool
> test_grandparents = all (== True) [t1, t2, t3]
>   where t1 = grandparents "Elizabeth I" == ["Henry VII", "Elizabeth of York", "Elizabeth Howard", "Thomas Boleyn"]
>         t2 = grandparents "Edward VI" == ["Henry VII", "Elizabeth of York"]
>         t3 = grandparents "Elizabeth of York" == []

test_siblings:
Test on some random subjects.

> test_siblings :: Bool
> test_siblings = all (== True) [t1, t2, t3]
>   where t1 = siblings "Elizabeth I" == ["Edward VI", "Mary I"]
>         t2 = siblings "Anne" == ["Mary II"]
>         t3 = siblings "Thomas Boleyn" == []

test_children:
Test on some random subjects..

> test_children :: Bool
> test_children = all (== True) [t1, t2, t3]
>   where t1 = children "Charles I" == ["James II", "Mary Henrietta", "Charles II"]
>         t2 = children "Elizabeth of York" == ["Henry VIII"]
>         t3 = children "Charles II" == []

test_grandchildren:
Test on some random subjects...

> test_grandchildren :: Bool
> test_grandchildren = all (== True) [t1, t2, t3]
>   where t1 = grandchildren "Charles I" == ["Anne", "Mary II", "William III"]
>         t2 = grandchildren "Elizabeth Howard" == ["Elizabeth I"]
>         t3 = grandchildren "Philip" == []

test_spouse:
Test on some random subjects....

> test_spouse :: Bool
> test_spouse = all (== True) [t1, t2, t3]
>   where t1 = spouse "Henry VIII" == ["Catherine Parr", "Catherine Howard", "Anne of Cleves", "Jane Seymour", "Anne Boleyn", "Catherine of Aragon"]
>         t2 = spouse "Catherine of Aragon" == ["Henry VIII"]
>         t3 = spouse "George Boleyn" == []

theTest_q7:
One test to test them all!

> theTest_q7 :: Bool
> theTest_q7 = all (== True) [
>                test_parents, test_grandparents, test_siblings,
>                test_children, test_grandchildren, test_spouse
>              ]
