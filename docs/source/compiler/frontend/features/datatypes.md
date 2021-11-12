# Datatypes

Similary to Idris & Agda, datatypes are defined with inductive family
`data` declarations:

```
type d a₁ … aₙ =
  | C1 ty₁₁ … ty₁ₙ
  | C2 { name : ty₂₁, …, nameₙ : ty₂ₙ }
  ...
  | Cn : name : tyₙ₁ -> … -> d a₁ … aₙ
```


- `d` is the datatype family
- `C1` through `Cn` are the sum type constructors with the given type.
  + `C1` shows Haskell style constructor
  + `C2` shows a record style constructor
  + `Cn` shows a GADT/Agda style constructor
- `a₁` through `aₙ` are the paramaters, scoped over the types of the sum
  constructors, which must be constant in their targets.


In general, constructor types can reference `d` inductively.
Different parameterisations of core may represent datatypes in different ways, and might impose
restrictions such as positivity (`d` can only occur to the left of an even number of arrows in the constructor type)
or strict positivity (`d` cannot occur to the left of an arrow anywhere in the constructor type).

Mutually inductive-recursive definitions will be supported at a later date.
