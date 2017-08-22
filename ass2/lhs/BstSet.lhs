Union: Union is not an operation that we often apply on trees. So What I did here is
convert the second tree into a list, and add elements of the list to the first tree.
To avoid building a heavily unbalanced tree, I use `toInOrderList` to convert the
tree.

Intersect: again, I don't think intersect is an operation that we often apply on
trees. Two equivalent sets can be backed by two drastically different trees. My approach
here is convert two trees into two lists using function `toInOrderList`, intersect
the list (retaining the order), and make a new tree from the intersected list using
function `makeSet`


Select: I take two different approaches here. Approach 1: convert the tree to a list,
filter the list, and reconstruct the tree; Approach 2: traverse the tree, and delete
every node that doesn't satisfy the function. They produce different trees, but same
sets. I guess approach 1 is more efficient but I have no concrete reason to support
it.
