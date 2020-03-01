# `traverse`: Traversable data structures with applicative functors

This library provides:
- a module signature `Traverse.Applicative.S` for applicative functors
  [[McBride and Patterson, 2008]],
- two functions `Traverse.list` and `Traverse.seq` to traverse lists
  and sequences by a given applicative functor,
- many instances of applicative functors:
  `Traverse.Applicative.{iter, map, reduce, env, fold, pair, forall,
  exists, option, result, list}`.

[McBride and Patterson, 2008]: http://www.staff.city.ac.uk/~ross/papers/Applicative.html

The purpose of this library is to provide a common dictionary of
"visitors", which is orthogonal to the visited data structure. For
instance, `Traverse.list Traverse.Applicative.iter` is equivalent to
`List.iter`, `Traverse.list Traverse.Applicative.map` is equivalent to
`List.map`, etc.  Moreover, one can get a visitor which combine, for
instance, both `List.map` and `List.fold_left` by invoking
`Traverse.list Traverse.Applicative.(pair map fold)`. To get the same
visitors for a sequence, one just have to replace `Traverse.list` by
`Traverse.seq`, and it is easy to define a traversal for any algebraic
data structures.
