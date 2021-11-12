# Usage

Juvix's frontend syntax is primarily inspired by Haskell
{footcite}`haskell` and other Hindly Milner derived languages.

```haskell
sig zipWith : (a -> b -> c) -n-> Vect n a -> Vect n b -> Vect n c
let zipWith _ Nil       Nil       = Nil
let zipWith f (x :: xs) (y :: ys) = f x y :: zipWith f xs ys

sig zipAdd : Num a => Vect n a -> Vect n a -> Vect n a
let zipAdd = zipWith (+)

sig test_zip_add : zipAdd (1 :: 2 :: 3) = 6
let test_zip_add = Refl
```

elaborates to

```haskell
sig zipWith : a : 0 Type -> b : 0 Type -> c : 0 Type -> (n : 0 Nat) -> (a -> b -> c) -n-> Vect n a -> Vect n b -> Vect n c
let zipWith (a : 0 Type) (b : 0 Type) (c : 0 Type) (Z : 0 Nat)   _ Nil        Nil       = Nil
let zipWith (a : 0 Type) (b : 0 Type) (c : 0 Type) (S k : 0 Nat) f (x :: xs)  (y :: ys) = f x y :: zipWith a b c k f xs ys

sig zipAdd : (a : Type) -0-> (n : Nat) -0-> (d : Num a) -n-> Vect n a -> Vect n a -> Vect n a
let zipAdd a n d xs ys = zipWith a a a n ((+) d) xs ys
```

Usage annotations are optional. Implicit usage arguments or
constraints are inferred where possible, but explicit annotations may
sometimes be required.

```haskell
sig f : 2 (x : 3 Int) -> Double

sig f : ω (x : 2 Int -> y : () -> IO ())
```

By default, no usage annotation is equivalent to an implicit usage
argument. The unification algorithm will attempt to infer constraints
involving multiple usage arguments where possible.

```{footbibliography}
```
