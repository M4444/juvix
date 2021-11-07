# A Taste of Juvix

```{warning} 
Some of these examples may not compile.
```

The objective of this section is to provide a glimpse of Juvix with some examples. The first one is a demonstration of dependently typed programming. 

## Dependent Types: Vectors

```haskell
type nat : ty = | Zero | Succ nat

type vec : nat -> ty -> ty = 
  | Nil : vec Zero a 
  | Cons : a -> vec n a -> vec (Succ n) a
```

## Pattern matching

```haskell
mod Norm where

open Prelude
open LLVM

type SomePoint = P3 int int int | P2 int int 

sig norm : SomePoint -> double
let norm (P3 x y z) = sqrt (x^2 + y^2 + z^2)
let norm (P2 x y) = sqrt (x^2 + y^2)

sig main : double
let main = norm (P3 2 3 4)
```