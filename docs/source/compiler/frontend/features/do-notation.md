# Do Notation

Actions in any monad can be sequenced implicitly

```
sig func : m Int.t
let func =
  one <- func-one
  two <- func-two one
  return (two + 3)
```

The implicit do notation desugars into `bind` (`>>=`) and `return` as
defined by the monad instance.
