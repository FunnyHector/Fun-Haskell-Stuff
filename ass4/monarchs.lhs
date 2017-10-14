
Implement the person, isChildOf, married, male, female, and sovereign functions
here.

> person :: String -> Maybe ()  -- can change to other monad
> married :: String -> String -> Maybe ()
> isChildOf :: String -> String -> Maybe ()
> male :: String -> Maybe ()
> female :: String -> Maybe ()
> sovereign :: String -> Maybe ()  -- not required

You may find it useful to comment out or remove the > characters before some of
the sections while you build out the language.

The data follows:

> facts = do
>     person "Elizabeth I"
>     person "Henry VIII"
>     person "Anne Boleyn"
>     person "Catherine of Aragon"
>     person "Jane Seymour"
>     person "Anne of Cleves"
>     person "Catherine Howard"
>     person "Catherine Parr"
>     person "Henry VII"
>     person "Mary I"
>     person "Philip"
>     person "Edward VI"
>     person "Elizabeth of York"
>     person "Thomas Boleyn"
>     person "Elizabeth Howard"
>     person "George Boleyn"
>     person "James I"
>     person "Charles I"
>     person "Charles II"
>     person "James II"
>     person "Mary II"
>     person "William III"
>     person "Anne"
>     person "Mary Henrietta"
>     person "Anne Hyde"
>     "Elizabeth I"   `isChildOf` "Henry VIII"
>     "Elizabeth I"   `isChildOf` "Anne Boleyn"
>     "Mary I"        `isChildOf` "Henry VIII"
>     "Mary I"        `isChildOf` "Catherine of Aragon"
>     "Edward VI"     `isChildOf` "Jane Seymour"
>     "Edward VI"     `isChildOf` "Henry VIII"
>     "Henry VIII"    `isChildOf` "Henry VII"
>     "Henry VIII"    `isChildOf` "Elizabeth of York"
>     "Anne Boleyn"   `isChildOf` "Thomas Boleyn"
>     "Anne Boleyn"   `isChildOf` "Elizabeth Howard"
>     "George Boleyn" `isChildOf` "Thomas Boleyn"
>     "George Boleyn" `isChildOf` "Elizabeth Howard"
>     "Charles I"     `isChildOf` "James I"
>     "Charles II"    `isChildOf` "Charles I"
>     "Mary Henrietta"`isChildOf` "Charles I"
>     "James II"      `isChildOf` "Charles I"
>     "Mary II"       `isChildOf` "James II"
>     "Mary II"       `isChildOf` "Anne Hyde"
>     "William III"   `isChildOf` "Mary Henrietta"
>     "Anne"          `isChildOf` "James II"
>     "Anne"          `isChildOf` "Anne Hyde"
>     "Henry VIII"    `married` "Catherine of Aragon"
>     "Henry VIII"    `married` "Anne Boleyn"
>     "Henry VIII"    `married` "Jane Seymour"
>     "Henry VIII"    `married` "Anne of Cleves"
>     "Henry VIII"    `married` "Catherine Howard"
>     "Henry VIII"    `married` "Catherine Parr"
>     "Henry VII"     `married` "Elizabeth of York"
>     "Thomas Boleyn" `married` "Elizabeth Howard"
>     "Mary I"        `married` "Philip"
>     "James II"      `married` "Anne Hyde"
>     "Mary I"        `married` "William III"
>     male "Henry VIII"
>     male "Henry VII"
>     male "Philip"
>     male "Edward VI"
>     male "Thomas Boleyn"
>     male "George Boleyn"
>     male "James I"
>     male "Charles I"
>     male "Charles II"
>     male "James II"
>     male "William III"
>     female "Anne Boleyn"
>     female "Catherine of Aragon"
>     female "Jane Seymour"
>     female "Anne of Cleves"
>     female "Catherine Howard"
>     female "Catherine Parr"
>     female "Mary I"
>     female "Elizabeth I"
>     female "Elizabeth of York"
>     female "Elizabeth Howard"
>     female "Mary II"
>     female "Anne"
>     female "Mary Henrietta"
>     female "Anne Hyde"
>     sovereign "Elizabeth I"
>     sovereign "Henry VIII"
>     sovereign "Henry VII"
>     sovereign "Mary I"
>     sovereign "Philip"
>     sovereign "Edward VI"
>     sovereign "James I"
>     sovereign "Charles I"
>     sovereign "Charles II"
>     sovereign "James II"
>     sovereign "Mary II"
>     sovereign "William III"
>     sovereign "Anne"


Design and implement a suitable query language for implementing these functions:

> parents :: String -> [String]
> parents = undefined

> grandparents :: String -> [String]
> grandparents = undefined

> siblings :: String -> [String]
> siblings = undefined

> grandchildren :: String -> [String]
> grandchildren = undefined

> spouse :: String -> [String]
> spouse = undefined
