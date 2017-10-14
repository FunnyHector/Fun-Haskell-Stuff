> module Family where

> import Control.Monad.State


==============================================
             Functions for Person
==============================================


Gender is gender, nothing really complicated here.

> data GenderType = Male | Female deriving (Show, Eq)

A Name is a String.

> type Name = String

A person can have multiple Attributes:
  - Name. A person has to have a name in our database. It's the only mandatory
    attribute. When creating a new record using `person` function, name has to
    be provided. Name also serves as the unique identifier of each person.
  - Gender. This can be Male or Female. This is not politically correct but
    sorry no other option available yet.
  - Spouses. A list of Name.
  - Children. A list of Name.
  - isSovereign. Default value: False. Can be set True by function `sovereign`.

> data Attribute = Name Name
>                | Gender GenderType
>                | Spouses [Name]
>                | Children [Name]
>                | Sovereign Bool
>                deriving (Show, Eq)

A Person is just a list of Attribute.

> newtype Person = Person [Attribute] deriving (Show, Eq)

newGuy:
Create a new guy with the name as given and other attributes as default.

> newGuy :: String -> Person
> newGuy name = Person [Name name, Sovereign False]

update:
Update a person record with the given attribute. If this person already has this
attribute set, then update it, otherwise add the given attribute as a new one.

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

hasName:
Does this guy has this name?

> hasName :: Person -> Name -> Bool
> hasName (Person []) _ = False
> hasName (Person (a : as)) name
>   | Name name == a = True
>   | otherwise      = hasName (Person as) name

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
Put a new guy into our monarchs. This is the only function that can add a new
record into our database.

> person :: String -> State Monarchs ()
> person name = state $ \ps -> ((), newGuy name : ps)

isChildOf:
Set a Parent - Child relationship. This is a single directional relationship,
as the father and mother knows the child but the child doesn't know his/her
parents. (tears...)

> isChildOf :: Name -> Name -> State Monarchs ()
> isChildOf child parent = do
>   target <- getPerson parent
>   _      <- getPerson child    -- to detect whether we have this child in database
>   let children = getChildren target
>   setAttribute parent (Children (child : children))
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
I made the design decision to let it explode because I think any operation on a
non-existing record is illegal and should be reported back to user. My first
implementation of this function returns `State Monarchs (Maybe Person)` to let
a non-existing name pass silently. But I don't think it's a good design on any
database.

> getPerson :: String -> State Monarchs Person
> getPerson name = state $ \ps ->
>   let target    = find name ps
>       find _ [] = error (name ++ " is not yet recorded in the system!")
>       find n (x : xs)
>         | x `hasName` name = x
>         | otherwise        = find n xs
>   in  (target, ps)

delPerson:
Delete and return the person with matched name (as a Just). If there is no
record found, then return Nothing.

> delPerson :: String -> State Monarchs Person
> delPerson name = do
>   target <- getPerson name
>   ps <- get
>   put $ filter (not . (`hasName` name)) ps
>   return target

setAttribute:
Set the given attribute to anyone who has the given name. If this name doesn't
belong to anybody in our database, then do nothing.

> setAttribute :: Name -> Attribute -> State Monarchs ()
> setAttribute name attr = do
>   target <- delPerson name
>   let updatedTarget = update attr target
>   ps <- get
>   put (updatedTarget : ps)
>   return ()


==============================================
            Question 6 Discussion
==============================================


1. Discuss the monad you chose, and why.




2. Is there a better way this language could be designed? What is it? Why?








==============================================
            Question 6 Test Cases
==============================================


test_person:


> test_person :: Bool
> test_person = undefined


test_married:


> test_married :: Bool
> test_married = undefined


test_isChildOf:


> test_isChildOf :: Bool
> test_isChildOf = undefined



test_male:


> test_male :: Bool
> test_male = undefined



test_female:


> test_female :: Bool
> test_female = undefined


test_sovereign:


> test_sovereign :: Bool
> test_sovereign = undefined







theTest_q6:
One test to test them all!

> theTest_q6 :: Bool
> theTest_q6 = all (== True) [
>                test_person, test_married, test_isChildOf, test_male,
>                test_female, test_sovereign
>              ]




==============================================
              Question 7 Code
==============================================


parents:


> parents :: String -> [String]
> parents = undefined



grandparents:


> grandparents :: String -> [String]
> grandparents = undefined




siblings:



> siblings :: String -> [String]
> siblings = undefined




grandchildren:



> grandchildren :: String -> [String]
> grandchildren = undefined




spouse:



> spouse :: String -> [String]
> spouse = undefined




==============================================
            Question 7 Discussion
==============================================


1. Which monad did you choose here, and why?






2. How, if at all, does it relate to what you chose in Q6?






==============================================
            Question 7 Test Cases
==============================================


test_parents:


> test_parents :: Bool
> test_parents = undefined




test_grandparents:


> test_grandparents :: Bool
> test_grandparents = undefined



test_siblings:


> test_siblings :: Bool
> test_siblings = undefined



test_grandchildren:


> test_grandchildren :: Bool
> test_grandchildren = undefined



test_spouse:


> test_spouse :: Bool
> test_spouse = undefined




theTest_q7:
One test to test them all!

> theTest_q7 :: Bool
> theTest_q7 = all (== True) [
>                test_parents, test_grandparents, test_siblings,
>                test_grandchildren, test_spouse
>              ]
