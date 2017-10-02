bool = [True, False]

infix -->
False --> _ = [True]
_ --> True = [True]
_ --> _ = []

infix |||
False ||| False = fail "||| false"
_ ||| _ = return True

infix &&&
True &&& True = return True
_ &&& _ = []

logic = do x <- bool
           y <- bool
           z <- bool
           x --> y
           w <- y --> z
           w ||| x
           return (x, y, z)
