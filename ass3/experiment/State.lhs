This file represents a state during the execution of programme. It's a
composition of local variable store, global variable store, local symbol table,
global symbol table, and procedure store.

> module State (
>   State(..),

>
>
> ) where

> import While (SymTab, VarStore, PrcdrStore)


> data State = State {
>   symTabL   :: SymTab,
>   symTabG   :: SymTab,
>   varStoreL :: VarStore,
>   varStoreG :: VarStore,
>   prcStore  :: PrcdrStore
> } deriving (Show)
