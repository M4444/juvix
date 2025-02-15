# Record Types

Record types can be defined with the `record` keyword:

```
type r a₁ … aₙ = {
  x : a,
  y : B.t aᵢ
  z : C.t aⱼ aₖ
}
```

This declaration creates a type `r`, and record accessors `x`, `y`,
and `z`. Note that the type of `y` depends on `aᵢ`, and the type of
`z` depends on `aⱼ` and `aₖ`.  `a₁ … aₙ` are the parameters, scoped
over the types of the fields and the type of the constructor.

Record types & accessor functions desugar to dependent pairs (although the frontend typechecker treats separate record declarations with the same fields as separate types).
