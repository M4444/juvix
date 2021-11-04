# Prior Work

## Dependently-typed languages

Three dependently-typed languages have seen substantial contemporary usage: Agda, Coq, and Idris {footcite}`idris-systems-programming-meets-full-dependent-types`. The first two are focused on theorem proving rather than executable code output, and have been primarily used to verify mathematical formulae or proofs of algorithmic correctness, where the algorithms which have been verified are then implemented in or extracted to another language for execution.

Idris does intend to simultaneously support dependently-typed program verification and produce executable code output, but falls short of the requirements of wide deployment: the compilation output is not efficient enough, too much effort is required to write proofs of properties of terms, and insufficient engineering effort has been dedicated to the inclusion of optimising transformations which take advantage of the expressive typesystem (understandably so, since Idris is primarily developed by a university lecturer in his free time!). Furthermore, the economics of most standard programs running on the desktop or web favour development & execution speed over safety and correctness.

## Dependently-typed smart contracts

One prior work {footcite}`safer-smart-contracts-through-type-driven-development` wrote an Idris {footcite}`idris-systems-programming-meets-full-dependent-types` backend targeting Ethereum's LLL language {footcite}`lisp-like-language`. Juvix shares many of the goals outlined in that paper, but the approach described therein failed to take advantage of well-known optimisations such as tail-call optimisation and handicapped itself by compiling to LLL instead of directly to EVM opcodes. The effects system described may be a sensible model for smart contract programs written in Juvix (or other dependently-typed smart contract languages).

```{warning}
**TODO: Add and link to an example Juvix contract utilising effects for asset transfers.**
```

Formality {footcite}`formality` was a substantial inspiration for this work, particularly the low-level interaction net execution model, although Juvix does not utilise interaction nets at the moment.

```{footbibliography}
```
