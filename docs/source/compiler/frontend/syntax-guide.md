# Syntax Guide


```{note}
This page is subject to change with the syntax redesign.
```

## Syntax Guide
This document is heavily inspired by the [Idris syntax guide](http://docs.idris-lang.org/en/latest/reference/syntax-guide.html).

## File Organization
A file contains zero or more [top level declarations](#top-level-declarations).

For example:
```haskell
  -- this is a function with a signature!
  sig foo : Natural
  let foo = 3

  -- this is a data type declaration
  type list a = Cons a (List a)
              | Nil

  -- this is a module declaration!
  mod Boo =
    let fi = 10
  end
```

## Comments
Comments are denoted by two dashes, `--`. The two dashes and all characters up until the end of the line are discarded.

Example:

```haskell
  -- This is a comment!
```
## Symbols
Symbols are used for any name declared in the Juvix programming
language. Symbols are broken into two categories, infix and prefix.

Prefix symbols start with either an letter or an underscore,
which can be followed up by any alphanumeric character, underscores,
punctuation(?, !), or dashes.

```haskell
  -- a valid symbol
  hello-there

  -- another valid symbol
  _unused-symbol

  -- another valid symbol. Typically '?' denotes that the symbol is a predicate
  even?

  -- An important action
  foo!

  -- not a valid prefix symbol
  -foo
```

An infix symbol starts with any character other than a letter or an underscore.

```haskell
  -- This is a valid infix symbol

  !!

  -- this is also another valid infix symbol

  -<++

  -- this is not a valid infix symbol, but instead a comment of a -

  ---

  -- here is an arrow infix symbol

  ->
```

## Top Level Declarations
### Functions

Functions are started by writing `let` which is followed by any valid
prefix symbol or an infix symbol surrounded by parentheses which shall
be referred to as the function name. Then, there are zero or more
arguments, with implicit arguments surrounded by curly braces
(`{}`). The argument list ends when an equal sign (`=`) is placed,
which denotes the start of the body.

Example:
```haskell
  -- this is a valid function
  let f x = x + 3

  -- this is another valid variable/function
  let y = 5

  -- this is a valid infix symbol
  let (+) = plus


  -- a function with an implicit argument
  let foo {prf} x = x
```

Another important part of a function is the signature.

A signature is denoted first by `sig`, then the function name.
From here colon (`:`) denotes the start of the type of the function name.
Subsequently, any valid type can be written.

Example:
```haskell
  -- a valid signature and function
  sig foo : int -> int
  let foo x = x + 3


  -- an example of a dependent signature
  sig add-nat-int
      :  x : nat
      -> y : int
      -> if | x > y -> nat
            | else  -> int
  let add-nat-int = (+)
```

### Types
Types are very similar to Haskell and Idris `ADT` and `GADT`
declarations.

Types are declared by writing `type` following by the name of the type
and arguments much like function syntax. Optionally a type signature
can be given at this point, by writing colon (`:`) followed by the type.

An equals sign (`=`) denotes the start of the body of the
type declaration.

From here a declaration can take a few forms.

1. Zero or more sums, each of which starts with pipe (`|`) and
   contains a tagged product.
2. A tagged product which starts with the new constructor name and
   either the arguments separated by spaces, a colon (`:`) followed
   by the arguments separated by arrows, or a base record.
3. A base record which is denoted by curly braces (`{}`). inside the
   curly braces, a name is given to every argument, which type is
   started via colon and terminated by a comma (`,`).

```haskell
  -- This is a valid type
  -- the a is a type argument
  type list a
    -- Cons is the constructor
    -- Cons takes an item of type a and a List of a
    = Cons a (list a)
    -- Nil is another constructor taking no arguments
    | Nil


  -- this is the same type, but GADT style arrow syntax
  -- is given to the constructor
  type list a : (a : set) -> set
  -- Curly braces can be used here to name the arguments
    = Cons { car : a,
             cdr : list a }
    | Nil

  -- Same type again but using GADT syntax in the constructors
  -- The first product can have a pipe!
  type list a =
    | Cons : a -> list a -> list a
    | Nil  : list a

  -- an example of a base record!
  type coords a = {
    x : a,
    y : a
  }

  -- Same example but we have a trailing comma
  type cords a = {
    x : a,
    y : a,
  }

  -- An example of a dependent type
  type vect : (len : nat) -> (elem : set) -> set =
    | Nil  : vect 0 elem
    | Cons : elem -> vect len elem -> vect (succ len) elem
```


### Modules

Modules are denoted similarly to functions except that instead of using
`let`, `mod` is used instead.

Instead of an expression, the body consists of zero or more top-level
declarations followed by `end`.

``` haskell

  -- example defining a module

  mod Foo =
    sig bar : nat
    let bar = 3

    -- The type is inferred here
    let baz = 5

  -- end ends the module definition
  end

  -- example using a module
  let test = Foo.bar + Foo.baz
```
### Imports
A module can be imported in two ways.

Importing a module unqualified via `open`ing them means that every
symbol in the module becomes unqualified.

A module can be `open`-ed:

Example:
```haskell
  -- A valid open
  open Foo

  -- opening the module Baz in the moudle Bar in the moudle Bar
  open Foo.Bar.Baz

  -- This is the same statement as above.
  open Foo
  open Bar.Baz


  -- let us define a module
  mod IntMod =
    let t = int

    sig of-nat : int -> t
    let of-nat x = x

    sig add : t -> t -> t
    let add = (+)
  end

  -- now we shall open it into our scope
  open IntMod

  -- we can now use it unqualified
  sig use-int-mod : nat -> nat -> t
  let use-int-mod x y = add (of-nat x) (of-nat y)
```

A module can also be aliased with a `let`:

Example:
```haskell
  -- a valid module alias
  let F = Foo
```

## Expressions
### Conditionals
#### If
If expressions have a non-zero number of clauses. Each clause consists of a boolean test, followed by a body term.

Example:
``` haskell
  -- this is a valid if expression!
  if | x == 3 -> 5
     | else   -> 6
  -- ^ test      ^ consequence

  -- this is also a valid a valid if expression
  if | x == 10     -> 25
     | positive? x -> x
     | negative? x -> abs x
     | else        -> 0
```

The `else` name is just an alias for `True`.
#### Case
Case expressions have a non-zero number of clauses. Each clause
consists of a pattern, followed by a consequence.

A pattern works much like Haskell or Idris, in that one can
deconstruct on a record or a constructor. We also allow record punning
on matches.

Example:
```haskell
  type tree a = Branch (tree a) a (tree a)
              | Leaf a
              | Empty


  -- an example with match!
  sig func : Tree nat -> nat
  let func foo =
    case foo of
    | Branch left ele right ->
      func left + ele + func right
    | Leaf ele ->
      ele
    | Empty ->
      0

  -- This is the same function!
  let func2 (Branch left ele right) =
    func2 left + ele + func2 right
  let func2 (Leaf ele) =
    ele
  let func2 Empty =
    0

  type coords = {
    x : int,
    y : int
  }

  -- match on record

  sig origin? : coords -> boolean
  let origin? {x, y}
    | x == y && x == 0 = True
    | else             = False

  -- same function as origin
  sig origin2? : coords -> boolean
  let origin2? {x = origX, y = origY}
    | origX == origY && origX == 0 =
      True
    | else = False
```
#### Dependent matching

### Definitions
Definitions within an expression are like their top level
counterparts, except that `in` followed by an expression must be
written after the definition.

#### Let
```haskell
  let foo =
    let bar = 3 in
    bar + 10
```
#### Modules
```haskell
  let foo =
    mod Bar =
      let foo = 3
      let bat = 10
    end in
    Bar.foo + Bar.bat
```
#### Signatures
```haskell
let foo =
  sig foo : int -> int in
  let foo x = x + 3 in
  foo 3
```
#### Types
```haskell
  let foo =
    type bar = Foo int
             | Bar nat
    in [Foo 3, Bar 10]
```
### Lists
List literals are started by the open bracket character (`[`). Within,
elements are separated by commas (`,`) before ending with a closing
bracket (`]`).

List literal syntax is just sugar for the `Cons` and `Nil`
constructors.

Example:
``` haskell
  -- this is a valid list
  [1]

  -- another valid list
  [1,2,3]

  -- the same list without sugar
  Cons 1 (Cons 2 (Cons 3 Nil))
```
### Tuples
Tuples are formatted like lists, however instead of using brackets,
parenthesis are used instead ( `(` `)` ).

Example:
``` haskell
  -- this is a tuple
  (1, 2)

  -- this is not a tuple
  (1)

  -- this is a 5 tuple!
  (1,2,3,4,5)
```
### Records
Record literals are started by an open curly brace (`{`). Within,
elements are bound to the corresponding name of the record via the
equals sign (`=`), or punned by the name directly. Elements, like
lists, are separated by commas (`,`) before ending with a closing
brace (`}`).

Example:
```haskell

  type coords = {
    x : int,
    y : int
  }

  -- a new construct called foo for coords
  sig create-cords : int -> int -> coords
  let create-cords x-dir y-dir = {
    x = x-dir,
    y = y-dir
  }


  -- same function with punning
  sig create-cords : int -> int -> coords
  let create-cords x y = {x, y}
```
#### Record updating syntax
<!-- add when it is in? -->
### Constants
#### String Literals
Strings are enclosed by double quotes (`"`)
 <!-- add escape characters once they are in -->

Example:
``` haskell
  let foo =
    "this is a string!"
```
#### Integers/Naturals
<!-- Update when we get floats and rationals -->
numbers are denoted by the characters 123456789.

Examples:
``` haskell
  -- a valid number literal
  let foo = 123


  -- another valid number
  let number-one = 1
```
### Do Notation

Do notation works similarly as it does in Haskell with changes to make
it indent insensitive. Namely, this means that after every binding a
semicolon (`;`) is needed to start the next expression. Further, no
`do` is needed, the semicolon is enough to determine if an expression
is in do syntax or not.

Thus like Haskell to bind terms, one states the name, then a left
arrow (`<-`), then the monadic expression terminated by a semicolon.

For non bindings, just the monadic expression with a semicolon is
needed.

The last expression in do notation does not need a semicolon.

Example:
``` haskell
  let foo my =
    x <- Just 5;
    y <- my;
    pure (x + y)


  let bar =
    Out.print "hello";
    name <- In.prompt "What is your name";
    Out.print ("hello" <> name)
```

### Local opens

Local opens work just like global opens, however one has to write `in`
then a body like other inner definitions.

Example:
```haskell
  let foo xs ys zs =
    open List in
    append xs (append ys zs)
```

There is also a more brief syntax where the module is then following
by `.( ... code here ... )`

Example:
```haskell
  let foo xs ys zs =
    List.(append xs (append ys zs))
```
