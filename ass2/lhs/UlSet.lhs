I decide to use algebraic data type for `Set`, because stricktly speaking a set is not
same as a list. A type synonym would be inaccurate.

To enforce the behaviour of a Set, I decide to hide the constructor to make the module
safe. To construct a Set, one has to use the function `makeSet` instead of directly call
the constructor.

"Can you use (==) instead of equals as the name for this function?"
Yes, we can. And in my opinion we should, because `equals` and `(==)` are semantically
identical. Therefore, I overrode the `(==)` function with `equals`. They are just alias to each other.
Now we can conveniently use `==` to compare the equivalence of two sets. If `==` wasn't
overriden, it would use the default implementation to compare two internal lists.

I decide to not put `Set` under `Ord` class, and not to implement the `(<=)`, `<`, `>=`, or `(>)` function.
In my opinion, sets cannot and should not be ordered. Being a subset to another and being less or
equal to another, if it is a valid claim, are two different things.
