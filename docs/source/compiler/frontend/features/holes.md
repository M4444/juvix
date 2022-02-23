# Holes

Holes stand for parts of terms which have not yet been defined, but which the typechecker can still reason about in some limited capacity by their context.

Holes can be invoked by writing `hole` and the name one wishes to have
displayed:

```
sig term : Int
let term = hole term_rhs
```
