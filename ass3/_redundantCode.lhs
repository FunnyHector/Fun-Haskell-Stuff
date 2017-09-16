These are redundant, or deleted from original code.
Not sure if they are useful any more, so here is a recycle bin.



varsOk:
Traverse the programme, and check whether all variables used in assignments and
expressions have been declared.

> varsOk :: Prog -> Bool
> varsOk (_, []) = True
> varsOk (symTab, stmt : stmts) = varsOk' (symTab, stmt) && varsOk (symTab, stmts)

varsOk':
Check for one statement whether all variables used in the statement have been
declared.

> varsOk' :: (SymTab, Stmt) -> Bool
> varsOk' (_, Skip)          = True
> varsOk' (symTab, Asgn v e) = hasKey v symTab && varsOk'' (symTab, e)
> varsOk' (symTab, If e c p) = varsOk'' (symTab, e) && varsOk c && varsOk p
> varsOk' (symTab, Do e p)   = varsOk'' (symTab, e) && varsOk p

varsOk'':
Check for one expression whether all variables used in the expression have been
declared.

> varsOk'' :: (SymTab, Exp) -> Bool
> varsOk'' (_,    Const _)      = True
> varsOk'' (symTab, Var v)      = hasKey v symTab
> varsOk'' (symTab, Bin _ e e') = varsOk'' (symTab, e) && varsOk'' (symTab, e')
> varsOk'' (symTab, Una _ e)    = varsOk'' (symTab, e)

asgnOk:
Check whether a variable is assigned with a value in correct type.

> -- asgnOk :: Prog -> Bool
> -- asgnOk (_, []) = True
> -- asgnOk (symTab, stmts) = varsOk' (symTab, stmt) && varsOk (symTab, stmts)
