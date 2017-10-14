
===========================================================
redundant things
===========================================================

isRecorded:
Is this guy recorded in our database?

> -- isRecorded :: String -> State Monarchs Bool
> -- isRecorded name = state $ \ps ->
> --   let yn = elem name (map name ps)
> --   in  (yn, ps)


[
  Person [Name "Anne",Sovereign True,Gender Female],
  Person [Name "William III",Sovereign True,Spouses ["Mary I"],Gender Male],
  Person [Name "Mary II",Sovereign True,Gender Female],
  Person [Name "James II",Sovereign True,Children ["Anne","Mary II"],Spouses ["Anne Hyde"],Gender Male],
  Person [Name "Charles II",Sovereign True,Gender Male],
  Person [Name "Charles I",Sovereign True,Children ["James II","Mary Henrietta","Charles II"],Gender Male],
  Person [Name "James I",Sovereign True,Children ["Charles I"],Gender Male],
  Person [Name "Edward VI",Sovereign True,Gender Male],
  Person [Name "Philip",Sovereign True,Spouses ["Mary I"],Gender Male],
  Person [Name "Mary I",Sovereign True,Spouses ["William III","Philip"],Gender Female],
  Person [Name "Henry VII",Sovereign True,Children ["Henry VIII"],Spouses ["Elizabeth of York"],Gender Male],
  Person [Name "Henry VIII",Sovereign True,Children ["Edward VI","Mary I","Elizabeth I"],Spouses ["Catherine Parr","Catherine Howard","Anne of Cleves","Jane Seymour","Anne Boleyn","Catherine of Aragon"],Gender Male],
  Person [Name "Elizabeth I",Sovereign True,Gender Female],
  Person [Name "Anne Hyde",Sovereign False,Children ["Anne","Mary II"],Spouses ["James II"],Gender Female],
  Person [Name "Mary Henrietta",Sovereign False,Children ["William III"],Gender Female],
  Person [Name "Elizabeth Howard",Sovereign False,Children ["George Boleyn","Anne Boleyn"],Spouses ["Thomas Boleyn"],Gender Female],
  Person [Name "Elizabeth of York",Sovereign False,Children ["Henry VIII"],Spouses ["Henry VII"],Gender Female],
  Person [Name "Catherine Parr",Sovereign False,Spouses ["Henry VIII"],Gender Female],
  Person [Name "Catherine Howard",Sovereign False,Spouses ["Henry VIII"],Gender Female],
  Person [Name "Anne of Cleves",Sovereign False,Spouses ["Henry VIII"],Gender Female],
  Person [Name "Jane Seymour",Sovereign False,Children ["Edward VI"],Spouses ["Henry VIII"],Gender Female],
  Person [Name "Catherine of Aragon",Sovereign False,Children ["Mary I"],Spouses ["Henry VIII"],Gender Female],
  Person [Name "Anne Boleyn",Sovereign False,Children ["Elizabeth I"],Spouses ["Henry VIII"],Gender Female],
  Person [Name "George Boleyn",Sovereign False,Gender Male],
  Person [Name "Thomas Boleyn",Sovereign False,Children ["George Boleyn","Anne Boleyn"],Spouses ["Elizabeth Howard"],Gender Male]
]
