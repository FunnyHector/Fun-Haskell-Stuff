> module Family where


==============================================
           The database for Q6 & Q7
==============================================


TODO: add a type signature for facts

facts = do
    person "Elizabeth I"
    person "Henry VIII"
    person "Anne Boleyn"
    person "Catherine of Aragon"
    person "Jane Seymour"
    person "Anne of Cleves"
    person "Catherine Howard"
    person "Catherine Parr"
    person "Henry VII"
    person "Mary I"
    person "Philip"
    person "Edward VI"
    person "Elizabeth of York"
    person "Thomas Boleyn"
    person "Elizabeth Howard"
    person "George Boleyn"
    person "James I"
    person "Charles I"
    person "Charles II"
    person "James II"
    person "Mary II"
    person "William III"
    person "Anne"
    person "Mary Henrietta"
    person "Anne Hyde"
    "Elizabeth I"   `isChildOf` "Henry VIII"
    "Elizabeth I"   `isChildOf` "Anne Boleyn"
    "Mary I"        `isChildOf` "Henry VIII"
    "Mary I"        `isChildOf` "Catherine of Aragon"
    "Edward VI"     `isChildOf` "Jane Seymour"
    "Edward VI"     `isChildOf` "Henry VIII"
    "Henry VIII"    `isChildOf` "Henry VII"
    "Henry VIII"    `isChildOf` "Elizabeth of York"
    "Anne Boleyn"   `isChildOf` "Thomas Boleyn"
    "Anne Boleyn"   `isChildOf` "Elizabeth Howard"
    "George Boleyn" `isChildOf` "Thomas Boleyn"
    "George Boleyn" `isChildOf` "Elizabeth Howard"
    "Charles I"     `isChildOf` "James I"
    "Charles II"    `isChildOf` "Charles I"
    "Mary Henrietta"`isChildOf` "Charles I"
    "James II"      `isChildOf` "Charles I"
    "Mary II"       `isChildOf` "James II"
    "Mary II"       `isChildOf` "Anne Hyde"
    "William III"   `isChildOf` "Mary Henrietta"
    "Anne"          `isChildOf` "James II"
    "Anne"          `isChildOf` "Anne Hyde"
    "Henry VIII"    `married` "Catherine of Aragon"
    "Henry VIII"    `married` "Anne Boleyn"
    "Henry VIII"    `married` "Jane Seymour"
    "Henry VIII"    `married` "Anne of Cleves"
    "Henry VIII"    `married` "Catherine Howard"
    "Henry VIII"    `married` "Catherine Parr"
    "Henry VII"     `married` "Elizabeth of York"
    "Thomas Boleyn" `married` "Elizabeth Howard"
    "Mary I"        `married` "Philip"
    "James II"      `married` "Anne Hyde"
    "Mary I"        `married` "William III"
    male "Henry VIII"
    male "Henry VII"
    male "Philip"
    male "Edward VI"
    male "Thomas Boleyn"
    male "George Boleyn"
    male "James I"
    male "Charles I"
    male "Charles II"
    male "James II"
    male "William III"
    female "Anne Boleyn"
    female "Catherine of Aragon"
    female "Jane Seymour"
    female "Anne of Cleves"
    female "Catherine Howard"
    female "Catherine Parr"
    female "Mary I"
    female "Elizabeth I"
    female "Elizabeth of York"
    female "Elizabeth Howard"
    female "Mary II"
    female "Anne"
    female "Mary Henrietta"
    female "Anne Hyde"
    sovereign "Elizabeth I"
    sovereign "Henry VIII"
    sovereign "Henry VII"
    sovereign "Mary I"
    sovereign "Philip"
    sovereign "Edward VI"
    sovereign "James I"
    sovereign "Charles I"
    sovereign "Charles II"
    sovereign "James II"
    sovereign "Mary II"
    sovereign "William III"
    sovereign "Anne"


==============================================
              Question 6 Code
==============================================


person:


> person :: String -> m ()
> person = undefined



married:


> married :: String -> String -> m ()
> married = undefined



isChildOf:


> isChildOf :: String -> String -> m ()
> isChildOf = undefined




male:



> male :: String -> m ()
> male = undefined



female:


> female :: String -> m ()
> female = undefined




sovereign:


> sovereign :: String -> m ()
> sovereign = undefined



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
